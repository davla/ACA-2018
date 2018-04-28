/*
 * A scratchpad memory supporting transactional memory
 *
 * Author: Davide Laezza - Roberts Fanning - Wenhao Li
 */

package conc

import Chisel._

import conc.Util._
import ocp._
import patmos._
import patmos.Constants._

class StatusBits (
    nrCores :Int,
    size :Int
) extends Module {
    import StatusBits._

    val io = new Bundle() {
        val addr = Bits(INPUT, log2Up(size))
        val core = Bits(INPUT, log2Up(nrCores))
        val wrEnable = Bool(INPUT)
        val data = Bits(INPUT, stateWidth)
        val canCommit = Bool(OUTPUT)
    }

    val statusBits = Mem(makeInitLocation(nrCores), size)
    val canCommit = RegInit(Fill(nrCores, Bits(1)))

    val location = statusBits(io.addr)
    val coreShift = io.core << indexShift

    io.canCommit := (canCommit >> coreShift)(0)

    when (io.wrEnable) {
        switch (io.data) {
            is (PRISTINE) {
                val bitMask = UInt(1) << coreShift
                location := location | bitMask
            }

            is (DIRTY) {
                val andBitMask = ~(UInt("b11") << coreShift)
                val orBitMask = Fill(nrCores, Bits("b10")) & andBitMask

                val newLocation = (location & andBitMask) | orBitMask
                location := newLocation

                val newCommits = new Array[Bool](nrCores)
                for (core <- 0 until nrCores) {
                    val coreState = getCoreState(newLocation, core)
                    newCommits(core) = coreState != (IN_TRANSACTION ## DIRTY)
                }

                // Status bits are reversed
                canCommit := catAll(newCommits.reverse)
            }
        }
    }
}

object StatusBits {
    val NOT_IN_TRANSACTION = Bits(0)
    val IN_TRANSACTION = Bits(1)
    val PRISTINE = Bits(0)
    val DIRTY = Bits(1)

    val stateWidth = 2
    val indexShift = log2Up(stateWidth)

    def apply(nrCores :Int, size :Int) = {
        Module(new StatusBits(nrCores, size))
    }

    def getCoreState(location :Bits, core :Int) = {
        val start = core * stateWidth
        val end = start + stateWidth - 1
        location(end, start)
    }

    def makeInitLocation(nrCores :Int) = {
        val location = Fill(nrCores, NOT_IN_TRANSACTION ## PRISTINE)
        location.setWidth(nrCores * stateWidth)
        location
    }
}

class TransSpm(
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

    import StatusBits._
    import TransSpm._

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

    // Status bits
    val statusBits = StatusBits(nrCores, size / granularity)

    statusBits.io.addr := io.slave.M.Addr >> log2Up(granularity)
    statusBits.io.core := io.core
    statusBits.io.wrEnable := Bool(false)
    statusBits.io.data := Bits(0)

    val shouldWrite = io.slave.M.Cmd === OcpCmd.WR && statusBits.io.canCommit

    val cmdReg = Reg(next = io.slave.M.Cmd)
    val dataReg = RegInit(io.slave.M.Data)

    // store
    val stmsk = Mux(shouldWrite, io.slave.M.ByteEn, Bits(0))
    for (i <- 0 until BYTES_PER_WORD) {
        mem(i) <= (stmsk(i), io.slave.M.Addr(addrBits + 1, 2),
            io.slave.M.Data(BYTE_WIDTH*(i+1)-1, BYTE_WIDTH*i))
    }

    // Concatenates output from the memory blocks
    val rdData = mem.map(_(io.slave.M.Addr(addrBits + 1, 2))).reduceLeft((x,y) => y ## x)

    io.slave.S.Resp := Mux(cmdReg === OcpCmd.WR || cmdReg === OcpCmd.RD,
        OcpResp.DVA, OcpResp.NULL)
    io.slave.S.Data := Mux(cmdReg === OcpCmd.RD, rdData, dataReg)

    switch (io.slave.M.Cmd) {
        is (OcpCmd.RD) {
            statusBits.io.wrEnable := Bool(true)
            statusBits.io.data := PRISTINE
        }

        is (OcpCmd.WR) {
            when (statusBits.io.canCommit) {
                statusBits.io.wrEnable := Bool(true)
                statusBits.io.data := DIRTY
                dataReg := Result.SUCCESS
            }.otherwise {
                dataReg := Result.FAIL
            }
        }
    }
}

object TransSpm {
    object Result {
        val SUCCESS = Bits(0)
        val FAIL = Bits(1)
    }

    def apply(granularity :Int, nrCores: Int, size: Int) = {
        Module(new TransSpm(granularity, nrCores, size))
    }
}

class TransSpmTester(dut: TransSpm) extends Tester(dut) {
  import TransSpmTester._

  println("Transactional SPM Tester")

  // Response defaults ot OcpResp.DVA
  def read(addr: Int) = {
    println("---------------------------")
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
    println("---------------------------")
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

  // Transactional tests

  // Simple failed commit on a single variable
  poke(dut.io.core, 0)
  read(0)

  poke(dut.io.core, 1)
  expect(write(0, 0xFF), 0)

  poke(dut.io.core, 0)
  expect(write(0, 0xFF), 1)

  // Failed commit on more than one variable
  poke(dut.io.core, 0)
  for (addr <- 0 until(256, GRANULARITY)) {
      read(addr)
  }

  poke(dut.io.core, 1)
  for (addr <- 0 until(256, GRANULARITY)) {
      expect(write(addr, 0xFF), 0)
  }

  poke(dut.io.core, 0)
  for (addr <- 0 until(256, GRANULARITY)) {
      expect(write(addr, 0xFF), 1)
  }

  // Even writing one variable makes the whole transaction not commit
  poke(dut.io.core, 0)
  for (addr <- 0 until(256, GRANULARITY)) {
      read(addr)
  }

  poke(dut.io.core, 1)
  expect(write(128, 0xFF), 0)

  poke(dut.io.core, 0)
  for (addr <- 0 until(256, GRANULARITY)) {
      expect(write(addr, 0xFF), 1)
  }
}

object TransSpmTester {
    val GRANULARITY = 32

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c",
      "--compile", "--vcd", "--targetDir", "generated"),
      () => TransSpm(GRANULARITY, 4, 1024)) {
        c => new TransSpmTester(c)
      }
  }
}
