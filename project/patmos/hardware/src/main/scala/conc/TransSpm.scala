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
    val WRITTEN_IN_COMMIT = Bits("b10")

    val stateWidth = StatusBits.WRITTEN_BEFORE_START.getWidth
    val indexShift = log2Up(stateWidth)

    def apply(nrCores :Int) = {
        val location = Fill(nrCores, StatusBits.WRITTEN_BEFORE_START)
        location.setWidth(nrCores * stateWidth)
        location
    }

    def applyBitMask(location :Bits, andMask :Bits, orMask :Bits) = {
        (location & andMask) | orMask
    }

    def makeUntouched(location :Bits, index :UInt) = {
        val andBitMask = ~ (UInt(1) << (index << indexShift + 1))
        val orBitMask = UInt(1) << (index << indexShift)
        applyBitMask(location, andBitMask, orBitMask)
    }

    def makeWritten(location :Bits, index :UInt) = {
        val andBitMask = ~ (UInt(0, width = stateWidth) << (index << indexShift))
        val orBitMask = UInt(0, width = location.getWidth)
        applyBitMask(location, andBitMask, orBitMask)
    }

    def hasUncommitted(location :Bits, commitStatus :Bits) = {
        val nrCores = location.getWidth / stateWidth
        val found = Bits()
        found := Bits(0, width = nrCores)
        found.setWidth(nrCores)

        for (k <- 0 until nrCores) {
            val coreState = get(location, k)
            found(k) := commitStatus(k) && coreState === UNTOUCHED_SINCE_START
        }

        found.orR()
    }

    def hasInCommit(location :Bits) = {
        val nrCores = location.getWidth / stateWidth
        var found = Bits()
        found := Bits(0, width = nrCores)
        found.setWidth(nrCores)

        for (k <- 0 until nrCores) {
            val coreState = get(location, k)
            found(k) := coreState === WRITTEN_IN_COMMIT
        }

        found.orR()
    }

    def write(location :Bits, core :UInt) = {
        val nrCores = location.getWidth / stateWidth
        val newLocation = location

        for (k <- 0 until nrCores) {
            val start = k * stateWidth
            val end = start + stateWidth - 1

            // Current core should write WRITTEN_IN_COMMIT
            when (UInt(k) === core) {
                newLocation(end, start) := WRITTEN_IN_COMMIT
            }

            // Core has read, setting to WRITTEN_AFTER_START
            .elsewhen (get(location, k) === StatusBits.UNTOUCHED_SINCE_START) {
                newLocation(end, start) := StatusBits.WRITTEN_AFTER_START
            }
        }

        newLocation
    }

    def write0(location :Bits, core :UInt, doneCommitting :Bool) = {
        val nrCores = location.getWidth / stateWidth
        val newLocation = location

        for (k <- 0 until nrCores) {
            val start = k * stateWidth
            val end = start + stateWidth - 1

            // Current core should write WRITTEN_IN_COMMIT
            when (UInt(k) === core) {
                newLocation(end, start) := Mux(doneCommitting,
                    WRITTEN_BEFORE_START, WRITTEN_IN_COMMIT)
            }

            // Core has read, setting to WRITTEN_AFTER_START
            .elsewhen (get(location, k) === StatusBits.UNTOUCHED_SINCE_START) {
                newLocation(end, start) := StatusBits.WRITTEN_AFTER_START
            }
        }

        newLocation
    }

    def get(location :Bits, index :Int) = {
        val start = index * stateWidth
        val end = start + stateWidth - 1
        location(end, start)
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

    val io = new Bundle() {
        val slave = new OcpCoreSlavePort(ADDR_WIDTH, DATA_WIDTH)
        val core = UInt(INPUT, log2Up(nrCores))
        val location = Bits(OUTPUT, 8)
        val db = Bits(OUTPUT, 2)
    }

    val addrBits = log2Up(size / BYTES_PER_WORD)
    val statusBitsSize = size / granularity

    // generate byte memories
    val mem = new Array[MemBlockIO](BYTES_PER_WORD)
    for (i <- 0 until BYTES_PER_WORD) {
        mem(i) = MemBlock(size / BYTES_PER_WORD, BYTE_WIDTH, bypass = false).io
    }

    // Status bits
    val statusBits = Vec.fill(statusBitsSize)(RegInit(StatusBits(nrCores)))
    // val statusBits = Mem(StatusBits(nrCores), statusBitsSize)

    // Or gates
    val allUntouched = Vec.tabulate (nrCores) (core => {
        val allLeftBits = statusBits.map(location => {
            val coreState = StatusBits.get(location, core)
            coreState(1)
        })
        orAll(allLeftBits.toArray) === Bits(0)
    })
    val allWritten = Vec.tabulate (nrCores) (core => {
        val allRightBits = statusBits.map(location => {
            val coreState = StatusBits.get(location, core)
            coreState(0)
        })
        orAll(allRightBits.toArray) === Bits(0)
    })

    // Commit bits
    val inCommit = RegInit(Bits(0, width = nrCores))

    val currentStatusBits = getStatusBits(io.slave.M.Addr)

    val hasUncommitted = StatusBits.hasUncommitted(currentStatusBits, inCommit)
    val hasInCommit = StatusBits.hasInCommit(currentStatusBits)
    val committingHere = hasUncommitted || hasInCommit

    val canRead = ~hasUncommitted
    val canWrite = ~committingHere && (inCommit(io.core) || allUntouched(io.core))

    val shouldWrite = io.slave.M.Cmd === OcpCmd.WR && canWrite

    io.db := hasUncommitted ## hasInCommit
    io.location := currentStatusBits

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
            when (canRead) {
                currentStatusBits := StatusBits.makeUntouched(currentStatusBits, io.core)
            }
        }

        is (OcpCmd.WR) {
            when (canWrite) {
                currentStatusBits := StatusBits.write(currentStatusBits,
                        io.core)
                dataReg := Bits(0, width = DATA_WIDTH)

                inCommit(io.core) := Mux(allWritten(io.core), Bool(false), Bool(true))
                when (allWritten(io.core)) {
                    doCommit(io.core, io.slave.M.Addr)
                }
            }.otherwise {
                dataReg := Bits(1, width = DATA_WIDTH)
            }
        }
    }

    def getStatusBits(address :UInt) = {
        val statusBitsAddr = address >> log2Up(granularity)
        statusBits(statusBitsAddr)
    }

    def doCommit(core :UInt, currAddr :UInt) = {
        for (addr <- 0 until statusBitsSize) {
            val location = statusBits(UInt(addr))
            location := Mux(UInt(addr) === currAddr,
                StatusBits.write(currentStatusBits, io.core),
                StatusBits.makeWritten(location, core))
        }
    }

    // val collectCoresState = Vec.tabulate (nrCores) (core => {
    //     val allRightBits = (0 until statusBitsSize).map(UInt(_))
    //         .map(addr => {
    //             val statusBits = statusBits(addr)
    //             val coreState = StatusBits.get(statusBits, core)
    //             coreState(1)
    //         })
    //     orAll(allLeftBits) === Bits(0)
    // })
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
    poke(dut.io.slave.M.Addr, addr)
    poke(dut.io.slave.M.Cmd, 2) // OcpCmd.RD
    peek(dut.io.db)
    peek(dut.io.location)
    step(1)
    poke(dut.io.slave.M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io.slave.S.Resp) != 1) {
      step(1)
    }
    peek(dut.io.db)
    peek(dut.io.location)
    peek(dut.io.slave.S.Data)
    dut.io.slave.S.Data
  }

  def write(addr: Int, data: Int) = {
    poke(dut.io.slave.M.Addr, addr)
    poke(dut.io.slave.M.Data, data)
    poke(dut.io.slave.M.Cmd, 1) // OcpCmd.WR
    poke(dut.io.slave.M.ByteEn, 0x0f)
    peek(dut.io.db)
    peek(dut.io.location)
    step(1)
    poke(dut.io.slave.M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io.slave.S.Resp) != 1) {
      step(1)
    }
    peek(dut.io.db)
    peek(dut.io.location)
    peek(dut.io.slave.S.Data)
    dut.io.slave.S.Data
  }

  poke(dut.io.core, 0)

  // Basic read-write test
  // for (i <- 0.until(1024, GRANULARITY)) {
  //   read(i)
  // }
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
