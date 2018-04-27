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

object StatusBits {
    val WRITTEN_BEFORE_START = Bits("b00")  //initial state
    val UNTOUCHED_SINCE_START = Bits("b01")
    val WRITTEN_AFTER_START = Bits("b11")

    val stateWidth = WRITTEN_BEFORE_START.getWidth
    val indexShift = log2Up(stateWidth)

    def apply(nrCores :Int) = {
        val location = Fill(nrCores, WRITTEN_BEFORE_START)
        location.setWidth(nrCores * stateWidth)
        location
    }

    def applyBitMask(location :Bits, andMask :Bits, orMask :Bits) = {
        (location & andMask) | orMask
    }

    def get(location :Bits, index :Int) = {
        val start = index * stateWidth
        val end = start + stateWidth - 1
        location(end, start)
    }

    def makeWrittenBefore(location: Bits, index :UInt) = {
        val andBitMask = ~(UInt("b11") << (index << indexShift))
        val orBitMask = UInt(0)
        applyBitMask(location, andBitMask, orBitMask)
    }

    def makeUntouched(location :Bits, index :UInt) = {
        val andBitMask = ~(UInt(1) << ((index << indexShift) + UInt(1)))
        val orBitMask = UInt(1) << (index << indexShift)
        applyBitMask(location, andBitMask, orBitMask)
    }

    def write(location :Bits, core :UInt) = {
        val nrCores = location.getWidth / stateWidth

        val thisCoreSet = makeWrittenBefore(location, core)

        val newStates = new Array[Bits](nrCores)
        for (k <- 0 until nrCores) {
            val coreState = get(thisCoreSet, k)
            val isUntouched = coreState === UNTOUCHED_SINCE_START

            // Status bits are reversed
            newStates(nrCores - 1 - k) = Mux(isUntouched, WRITTEN_AFTER_START,
                coreState)
        }

        catAll(newStates)
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

        val commits = Bits(OUTPUT, 8)
        val location = Bits(OUTPUT, 8)
        val db = Bits(OUTPUT, 2)
        val written = Bits(OUTPUT, 1)
    }

    val addrBits = log2Up(size / BYTES_PER_WORD)
    val statusBitsSize = size / granularity

    // generate byte memories
    val mem = new Array[MemBlockIO](BYTES_PER_WORD)
    for (i <- 0 until BYTES_PER_WORD) {
        mem(i) = MemBlock(size / BYTES_PER_WORD, BYTE_WIDTH, bypass = false).io
    }

    // Status bits
    val statusBits = Mem(StatusBits(nrCores), statusBitsSize)

    // Each of the OR gates here has as input the left bit of a core state
    // for every location. It is used to check whether any location is in the
    // WRITTEN_AFTER_START state. It is ignored during commits, that is the
    // only moment in which the state can be WRITTEN_IN_COMMIT
    val allUntouchedArr = new Array[Bool](nrCores)
    for (core <- 0 until nrCores) {
        val leftBits = new Array[Bits](statusBitsSize)
        for (addr <- 0 until statusBitsSize) {
            val location = statusBits(addr)
            val coreState = StatusBits.get(location, core)
            leftBits(addr) = coreState(1)
        }
        allUntouchedArr(core) = orAll(leftBits) === Bits(0)
    }
    val allUntouched = Vec(allUntouchedArr)

    val currentStatusBits = getStatusBits(io.slave.M.Addr)

    val shouldWrite = io.slave.M.Cmd === OcpCmd.WR && allUntouched(io.core)

    io.db := Bits(0)
    io.location := Bits(0)
    io.commits := Bits(0)
    io.written := Bits(0)

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
            currentStatusBits := StatusBits.makeUntouched(currentStatusBits, io.core)
        }

        is (OcpCmd.WR) {
            when (allUntouched(io.core)) {
                currentStatusBits := StatusBits.write(currentStatusBits, io.core)
                dataReg := Result.SUCCESS
            }.otherwise {
                dataReg := Result.FAIL
            }
        }
    }

    def getStatusBits(address :UInt) = {
        val statusBitsAddr = address >> log2Up(granularity)
        statusBits(statusBitsAddr)
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
  def read(addr: Int, resp :Int = 1) = {
    println("---------------------------")
    poke(dut.io.slave.M.Addr, addr)
    poke(dut.io.slave.M.Cmd, 2) // OcpCmd.RD
    peek(dut.io.db)
    peek(dut.io.location)
    peek(dut.io.commits)
    peek(dut.io.written)
    step(1)
    poke(dut.io.slave.M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io.slave.S.Resp) != resp) {
      step(1)
    }
    peek(dut.io.db)
    peek(dut.io.location)
    peek(dut.io.commits)
    peek(dut.io.written)
    peek(dut.io.slave.S.Data)
    dut.io.slave.S.Data
  }

  def write(addr: Int, data: Int) = {
    println("---------------------------")
    poke(dut.io.slave.M.Addr, addr)
    poke(dut.io.slave.M.Data, data)
    poke(dut.io.slave.M.Cmd, 1) // OcpCmd.WR
    poke(dut.io.slave.M.ByteEn, 0x0f)
    peek(dut.io.db)
    peek(dut.io.location)
    peek(dut.io.commits)
    peek(dut.io.written)
    step(1)
    poke(dut.io.slave.M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io.slave.S.Resp) != 1) {
      step(1)
    }
    peek(dut.io.db)
    peek(dut.io.location)
    peek(dut.io.commits)
    peek(dut.io.written)
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

  // // Commits can not overlap
  // poke(dut.io.core, 0)
  // for (addr <- 0 until(256, GRANULARITY)) {
  //     read(addr)
  // }

  // for (addr <- 0 until(128, GRANULARITY)) {
  //     expect(write(addr, 0xFF), 0)
  // }

  // Already committed by core 0
  // poke(dut.io.core, 1)
  // for (addr <- 0 until(128, GRANULARITY)) {
  //     expect(write(addr, 0xFF), 1)
  // }

  // Not yet committed by core 0
  // for (addr <- 128 until(256, GRANULARITY)) {
  //     expect(write(addr, 0xFF), 1)
  // }

  // Reading uncommitted variables should also fail
  // for (addr <- 128 until(256, GRANULARITY)) {
  //     read(addr, 2)
  // }
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
