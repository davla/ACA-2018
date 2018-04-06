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

object DirtyBits {
    val PRISTINE = Bits(0, width = 1)
    val DIRTY = Bits(1, width = 1)

    def apply(nrCores :Int) = {
        val location = Fill(nrCores, DirtyBits.PRISTINE)
        location.setWidth(nrCores)
        location
    }

    def get(location :Bits, index :UInt) = {
        (location.toUInt >> index)(0)
    }

    def makeDirty(location :Bits) = {
        val newLocation = Fill(location.getWidth, Bits(1, width = 1))
        newLocation.setWidth(location.getWidth)
        newLocation
    }

    def makePristine(location :Bits, index :UInt) = {
        val mask = ~(UInt(1, width = 1) << index)
        location & mask
    }
}

class LLSCSpm(
    val granularity :Int,
    nrCores: Int,
    size: Int
) extends Module {
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
        val db = Bits(OUTPUT, 2)
    }

    // The actual scratchpad memory
    val spm = Module(new Spm(size))

    // Dirty bits
    val dirtyBits = Mem(DirtyBits(nrCores), size / granularity)


    val masterReg = RegInit(io.slave.M)
    masterReg := io.slave.M
    val slaveReg = Reg(new OcpSlaveSignals(DATA_WIDTH))

    val delayReg = RegInit(UInt(1, width = 1))

    spm.io.M := masterReg
    io.slave.S <> Mux(delayReg === UInt(0), slaveReg, spm.io.S)

    val currDirtyBits = getDirtyBits(io.slave.M.Addr)
    io.db := spm.io.S.Resp

    when (delayReg === UInt(0)) {
        slaveReg.Resp := OcpResp.NULL
    }

    when (slaveReg.Resp =/= OcpResp.NULL) {
        delayReg := delayReg - UInt(1)
    }

    switch (io.slave.M.Cmd) {
        is (OcpCmd.RD) {
            currDirtyBits := DirtyBits.makePristine(currDirtyBits, io.core)
        }

        is (OcpCmd.WR) {
            delayReg := UInt(1)
            slaveReg.Resp := OcpResp.DVA

            when (DirtyBits.get(currDirtyBits, io.core) === DirtyBits.PRISTINE) {
                currDirtyBits := DirtyBits.makeDirty(currDirtyBits)
                slaveReg.Data := Bits(0)

            }.otherwise {
                masterReg.Cmd := OcpCmd.IDLE
                slaveReg.Data := Bits(1)
            }
        }
    }

    def getDirtyBits(address :UInt) = {
        val dirtyBitAddr = address >> log2Up(granularity)
        dirtyBits(dirtyBitAddr)
    }
}

object LLSCSpm {
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
    peek(dut.io.db)
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
    peek(dut.io.db)
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
