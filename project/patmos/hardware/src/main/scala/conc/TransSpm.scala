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
        val dva = Bool(INPUT)
        val data = Bits(INPUT, stateWidth)
        val canRead = Bits(OUTPUT, 1)
        val canWrite = Bits(OUTPUT, 1)
        val allWritten = Bits(OUTPUT, 1)
    }

    // Status bits
    val statusBits = Vec.fill(size)(RegInit(Fill(nrCores, WRITTEN_BEFORE_START)))

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

    // Each of these bit-strings is the concatenation of the right bit of a
    // core state for every location. As explained below, they are used to
    // detect the end of a commit
    val allRightBitsArr = new Array[Bits](nrCores)
    for (core <- 0 until nrCores) {
        val rightBits = new Array[Bits](statusBitsSize)
        for (addr <- 0 until statusBitsSize) {
            val location = statusBits(addr)
            val coreState = StatusBits.get(location, core)
            rightBits(addr) = coreState(0)
        }
        allRightBitsArr(core) = catAll(rightBits)
    }
    val allRightBits = Vec(allRightBitsArr)

    // After masking the bit for the current location, the negation of the
    // recursive OR of the bit strings for this core tells whether all the
    // read locations have been written to, marking the end of a commit. The
    // masking of the current location assumes a write in that location in the
    // current cycle.
    io.allWritten := ~((allRightBits(io.core) & ~(UInt(1) << io.addr)).orR)

    val location = statusBits(io.addr)

    val hasUncommitted = StatusBits.orMap(location, isUncommitted _)
    val hasInCommit = StatusBits.orMap(location, isInCommit _)
    val committingHere = hasUncommitted || hasInCommit

    io.canRead := ~hasUncommitted
    io.canWrite := ~committingHere && (inCommit(io.core) || allUntouched(io.core))

    when (io.dva) {
        switch (io.data) {
            is (UNTOUCHED_SINCE_START) {
                makeUntouched()
            }

            is (WRITTEN_IN_COMMIT) {
                for (addr <- 0 until size) {
                    val thisLocation = statusBits(size)

                    when (UInt(addr) === io.addr) {
                        thisLocation := write(thisLocation, io.core, Bool(false))

                    }.otherwise {
                        makeWrittenBefore(thisLocation, io.core)
                    }
            }
        }
    }

    def applyBitMask(andMask :Bits, orMask :Bits) = {
        (location & andMask) | orMask
    }

    def getCoreState(index :Int location :Bits = this.location) = {
        val start = index * stateWidth
        val end = start + stateWidth - 1
        location(end, start)
    }

    def makeWrittenBefore(location: Bits, index :UInt) = {
        val andBitMask = ~(UInt("b11") << (index << indexShift))
        val orBitMask = UInt(0)
        applyBitMask(location, andBitMask, orBitMask)
    }

    def makeInCommit(location :Bits, index :UInt) = {
        val andBitMask = ~(UInt(1) << (index << indexShift))
        val orBitMask = UInt(1) << ((index << indexShift) + UInt(1))
        applyBitMask(location, andBitMask, orBitMask)
    }


    def makeUntouched = {
        val andBitMask = ~(UInt(1) << ((io.core << indexShift) + UInt(1)))
        val orBitMask = UInt(1) << (io.core << indexShift)
        location := applyBitMask(location, andBitMask, orBitMask)
    }

}

object StatusBits {
    val WRITTEN_BEFORE_START = Bits("b00")  //initial state
    val UNTOUCHED_SINCE_START = Bits("b01")
    val WRITTEN_AFTER_START = Bits("b11")
    val WRITTEN_IN_COMMIT = Bits("b10")

    val stateWidth = WRITTEN_BEFORE_START.getWidth
    val indexShift = log2Up(stateWidth)

    def apply(nrCores :Int, size :Int) = {
        Module(new StatusBits(nrCores, size))
    }

    def orMap(location :Bits, f :(Bits, Int) => Bool) = {
        val nrCores = location.getWidth / stateWidth
        var acc = Bits()
        acc := Bits(0, width = nrCores)

        for (k <- 0 until nrCores) {
            acc(k) := f(get(location, k), k)
        }

        acc.orR
    }

    def write(location :Bits, core :UInt, inCommit :Bool) = {
        val nrCores = location.getWidth / stateWidth

        val thisCoreSet = Mux(inCommit, makeInCommit(location, core),
                makeWrittenBefore(location, core))

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
    val statusBits = StatusBits(nrCores, statusBitsSize)

    statusBits.io.addr := io.slave.M.Addr >> log2Up(granularity)
    statusBits.io.core := io.core

    // Commit bits
    val inCommit = RegInit(Bits(0, width = nrCores))

    val shouldWrite = io.slave.M.Cmd === OcpCmd.WR && statusBits.io.canWrite

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
            when (statusBits.io.canRead) {
                statusBits.io.dva := Bool(true)
                statusBits.io.data := UNTOUCHED_SINCE_START
            }
        }

        is (OcpCmd.WR) {
            when (statusBits.io.canWrite) {
                statusBits.io.dva := Bool(true)

                when (statusBits.io.allWritten) {
                    statusBits.io.data := WRITTEN_BEFORE_START

                }.otherwise {
                    statusBits.io.data := WRITTEN_IN_COMMIT
                }

                inCommit(io.core) := ~allWritten
                dataReg := Bits(0)

            }.otherwise {
                dataReg := Bits(1)
            }
        }
    }

    def doCommit(core :UInt, lastWrite :UInt) = {
        for (addr <- 0 until statusBitsSize) {
            val location = statusBits(addr)

            when (UInt(addr) === lastWrite) {
                currentStatusBits := StatusBits.write(currentStatusBits, io.core, Bool(false))

            }.otherwise {
                location := StatusBits.makeWrittenBefore(currentStatusBits, io.core)
            }
        }
    }

    def getStatusBits(address :UInt) = {
        val statusBitsAddr = address >> log2Up(granularity)
        statusBits(statusBitsAddr)
    }

    def isInCommit(coreState :Bits, core :Int) = {
        coreState === WRITTEN_IN_COMMIT
    }

    def isUncommitted(coreState :Bits, core :Int) = {
        inCommit(core) && coreState === UNTOUCHED_SINCE_START
    }
}

object TransSpm {
    def apply(granularity :Int, nrCores: Int, size: Int) = {
        Module(new TransSpm(granularity, nrCores, size))
    }
}

class TransSpmTester(dut: TransSpm) extends Tester(dut) {
  import TransSpmTester._

  println("Transactional SPM Tester")

  def read(addr: Int) = {
    println("---------------------------")
    poke(dut.io.slave.M.Addr, addr)
    poke(dut.io.slave.M.Cmd, 2) // OcpCmd.RD
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

  poke(dut.io.core, 0)
  read(0)

  poke(dut.io.core, 1)
  expect(write(0, 0xFF), 0)

  poke(dut.io.core, 0)
  expect(write(0, 0xFF), 1)
  sys.exit(0)

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

// def apply(nrCores :Int) = {
//     val location = Bits()
//     location := Fill(nrCores, WRITTEN_BEFORE_START)
//     location
// }
