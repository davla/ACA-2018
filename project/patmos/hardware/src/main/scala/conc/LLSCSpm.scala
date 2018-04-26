/*
 * A scratchpad memory supporting LL/SC
 *
 * Author: Davide Laezza - Roberts Fanning - Wenhao Li
 */

package conc

import Chisel._

import ocp._
import patmos._
import patmos.Constants._

class DirtyBits (
    nrCores :Int,
    size :Int
) extends Module {
    import DirtyBits._

    val io = new Bundle() {
        val addr = Bits(INPUT, log2Up(size))
        val core = Bits(INPUT, log2Up(nrCores))
        val dva = Bool(INPUT)
        val data = Bits(INPUT, 1)
        val coreBit = Bits(OUTPUT, 1)
    }

    // Dirty bits
    val dirtyBits = Mem(makeInitLocation(nrCores), size)

    val location = dirtyBits(io.addr)
    val coreBit = (location.toUInt >> io.core)(0)

    // Read
    io.coreBit := coreBit

    // Write
    when (io.dva) {
        switch (io.data) {

            // Making the bit for the core pristine
            is (PRISTINE) {
                val mask = ~(UInt(1) << io.core)
                location := location & mask
            }

            // Making the bits dirty for every core
            is (DIRTY) {
                location := Fill(nrCores, DIRTY)
            }
        }
    }
}

object DirtyBits {
    val PRISTINE = Bits(0)
    val DIRTY = Bits(1)

    def apply(nrCores :Int, size: Int) = {
        Module(new DirtyBits(nrCores, size))
    }

    def makeInitLocation(nrCores :Int) = {
        val location = Fill(nrCores, PRISTINE)
        location.setWidth(nrCores)
        location
    }
}

class LLSCSpm(
    val granularity :Int,
    nrCores: Int,
    size: Int
) extends Module {
    import DirtyBits._
    import LLSCSpm._

    if (!isPow2 (granularity)) {
        sys.error (s"LLSCSpm: granularity must be a power of 2, but $granularity was provided.")
    }

    if (!isPow2 (size)) {
        sys.error (s"LLSCSpm: size must be a power of 2, but $size was provided.")
    }

    if (granularity > size) {
        sys.error (s"LLSCSpm: granularity has to be less than size, but granularity $granularity and size $size were provided.")
    }

    val io = new Bundle() {
        val slave = new OcpCoreSlavePort(ADDR_WIDTH, DATA_WIDTH)
        val core = UInt(INPUT, log2Up(nrCores))
    }

    val addrBits = log2Up(size / BYTES_PER_WORD)

    // generate byte memories
    val mem = new Array[MemBlockIO](BYTES_PER_WORD)
    for (i <- 0 until BYTES_PER_WORD) {
        mem(i) = MemBlock(size / BYTES_PER_WORD, BYTE_WIDTH, bypass = false).io
    }

    val dirtyBits = DirtyBits(nrCores, size / granularity)

    dirtyBits.io.addr := io.slave.M.Addr >> log2Up(granularity)
    dirtyBits.io.core := io.core
    dirtyBits.io.dva := Bool(false)
    dirtyBits.io.data := Bits(0)

    val isPristine = dirtyBits.io.coreBit === PRISTINE
    val shouldWrite = io.slave.M.Cmd === OcpCmd.WR && isPristine

    val cmdReg = Reg(next = io.slave.M.Cmd)
    val dataReg = RegInit(io.slave.M.Data)

    // store
    val stmsk = Mux(shouldWrite, io.slave.M.ByteEn, Bits(0))
    for (i <- 0 until BYTES_PER_WORD) {
        mem(i) <= (stmsk(i), io.slave.M.Addr(addrBits + 1, 2),
            io.slave.M.Data(BYTE_WIDTH*(i+1)-1, BYTE_WIDTH*i))
    }

    val rdData = mem.map(_(io.slave.M.Addr(addrBits + 1, 2))).reduceLeft((x,y) => y ## x)

    io.slave.S.Resp := Mux(cmdReg === OcpCmd.WR || cmdReg === OcpCmd.RD,
        OcpResp.DVA, OcpResp.NULL)
    io.slave.S.Data := Mux(cmdReg === OcpCmd.RD, rdData, dataReg)

    switch (io.slave.M.Cmd) {
        is (OcpCmd.RD) {
            dirtyBits.io.dva := Bool(true)
            dirtyBits.io.data := PRISTINE
        }

        is (OcpCmd.WR) {
            when (isPristine) {
                dirtyBits.io.dva := Bool(true)
                dirtyBits.io.data := DIRTY

                dataReg := Result.SUCCESS
            }.otherwise {
                dataReg := Result.FAIL
            }
        }
    }
}

object LLSCSpm {
    object Result {
        val SUCCESS = Bits(0)
        val FAIL = Bits(1)
    }

    def apply(granularity :Int, nrCores: Int, size: Int) = {
        Module(new LLSCSpm(granularity, nrCores, size))
    }
}

class LLSCSpmTester(dut: LLSCSpm) extends Tester(dut) {
    import LLSCSpmTester._

  println("LL/SC SPM Tester")

  def read(addr: Int) = {
    poke(dut.io.slave.M.Addr, addr)
    poke(dut.io.slave.M.Cmd, 2) // OcpCmd.RD
    step(1)
    poke(dut.io.slave.M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io.slave.S.Resp) != 1) {
      step(1)
    }
    peek(dut.io.slave.S.Data)
    dut.io.slave.S.Data
  }

  def write(addr: Int, data: Int) = {
    poke(dut.io.slave.M.Addr, addr)
    poke(dut.io.slave.M.Data, data)
    poke(dut.io.slave.M.Cmd, 1) // OcpCmd.WR
    poke(dut.io.slave.M.ByteEn, 0x0f)
    step(1)
    poke(dut.io.slave.M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io.slave.S.Resp) != 1) {
      step(1)
    }
    peek(dut.io.slave.S.Data)
    dut.io.slave.S.Data
  }

  poke(dut.io.core, 0)

  // Basic read-write test

  for (i <- 0.until(1024, GRANULARITY)) {
    expect(write(i, i * 0x100 + 0xa), 0)
  }
  step(1)
  for (i <- 0.until(1024, GRANULARITY)) {
    expect(read(i), i * 0x100 + 0xa)
  }

  // LL/SC tests

  poke(dut.io.core, 0)

  // Write to the same memory location without reading in between should fail
  expect(write(0, 0xFF), 0)
  expect(write(0, 0xF0), 1)

  // Failed writes should not affect memory
  expect(read(0), 0xFF)

  // Write to the same memory location with a read from the same core in
  expect(write(0, 0xFF), 0)
  expect(read(0), 0xFF)
  expect(write(0, 0xF0), 0)

  // Write to memory in the same granularity block should fail
  if (GRANULARITY != 1) {
      expect(write(GRANULARITY * 9, 0xFF), 0)
      expect(write(GRANULARITY * 9 + 1, 0xF0), 1)
  }

  // Write to memory in different granularity blocks should succeed
  expect(write(GRANULARITY * 5, 0xFF), 0)
  expect(write(GRANULARITY * 3, 0xF0), 0)

  // Write frrom a different core should invalidate subsequent writes
  // from other cores
  poke(dut.io.core, 0)
  read(GRANULARITY * 8)
  poke(dut.io.core, 1)
  read(GRANULARITY * 8)
  expect(write(GRANULARITY * 8, 0xF0), 0)
  poke(dut.io.core, 0)
  expect(write(GRANULARITY * 8, 0xFF), 1)

  // The first write after a read shoudl always succeed
  poke(dut.io.core, 0)
  read(GRANULARITY * 8)
  poke(dut.io.core, 1)
  read(GRANULARITY * 8)
  expect(write(GRANULARITY * 8, 0xF0), 0)
  poke(dut.io.core, 0)
  expect(read(GRANULARITY * 8), 0xF0)
  expect(write(GRANULARITY * 8, 0xFF), 0)

  // Unlike CAS, LL/SC doesn't siffer the ABA problem
  val aValue = 0x19
  val bValue = 0x84

  poke(dut.io.core, 0)
  read(GRANULARITY * 8)                     // So that next write doesn't fail
  expect(write(GRANULARITY * 8, aValue), 0) // a value is written by core 0
  expect(read(GRANULARITY * 8), aValue)     // a value is read

  poke(dut.io.core, 1)
  expect(read(GRANULARITY * 8), aValue)     // So that next write doesn't fail
  expect(write(GRANULARITY * 8, bValue), 0) // value b is written by core 1
  expect(read(GRANULARITY * 8), bValue)     // So that next write doesn't fail
  expect(write(GRANULARITY * 8, aValue), 0) // vaue a is written again by core 1
  poke(dut.io.core, 0)
  expect(write(GRANULARITY * 8, 0xFF), 1)   // Store Conditional fails
}

object LLSCSpmTester {
    val GRANULARITY = 32

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c",
      "--compile", "--vcd", "--targetDir", "generated"),
      () => LLSCSpm(GRANULARITY, 4, 1024)) {
        c => new LLSCSpmTester(c)
      }
  }
}
