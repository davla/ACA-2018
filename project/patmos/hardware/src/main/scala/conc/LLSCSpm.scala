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
        val db = Bits(OUTPUT, nrCores)
    }

    // The actual scratchpad memory
    val spm = Module(new Spm(size))

    // Dirty bits
    val dirtyBits = Mem(DirtyBits(nrCores), size / granularity)

    val masterReg = RegInit(io.slave.M)
    masterReg := io.slave.M

    spm.io.M := masterReg
    io.slave.S.Data := spm.io.S.Data
    io.slave.S.Resp := spm.io.S.Resp

    val currDirtyBits = getDirtyBits(io.slave.M.Addr)
    io.db := io.slave.M.Addr

    switch (io.slave.M.Cmd) {
        is (OcpCmd.RD) {
            currDirtyBits := DirtyBits.makePristine(currDirtyBits, io.core)
        }

        is (OcpCmd.WR) {
            when (DirtyBits.get(currDirtyBits, io.core) === DirtyBits.PRISTINE) {
                currDirtyBits := DirtyBits.makeDirty(currDirtyBits)
                io.slave.S.Data := Bits(0)

            }.otherwise {
                masterReg.Cmd := OcpCmd.IDLE
                io.slave.S.Data := Bits(1)
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

  println("LL/SC SPM Tester")

  def read(addr: Int) = {
    poke(dut.io.slave.M.Addr, addr)
    poke(dut.io.slave.M.Cmd, 2) // OcpCmd.RD
    step(1)
    poke(dut.io.slave.M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io.slave.S.Resp) != 1) {
        peek(dut.io.db)
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
        peek(dut.io.db)
      step(1)
    }
  }

  poke(dut.io.core, 1)

  write(0, 0 * 0x100 + 0xa)
  // write(8, 1 * 0x100 + 0xa)
  // write(16, 1 * 0x100 + 0xa)
  write(32, 2 * 0x100 + 0xa)
  // write(48, 3 * 0x100 + 0xa)
  write(64, 3 * 0x100 + 0xa)

  // for (i <- 0 until(1024, 64)) {
  //   write(i, i * 0x100 + 0xa)
  //   // write(1, i+32, i * 0x10000 + 0xb)
  // }
  // step(1)
  // for (i <- 0 until 1) {
  //   expect(read(i * 64), i * 0x100 + 0xa)
  //   // expect(read(1, i+32), i * 0x10000 + 0xb)
  // }
}

object LLSCSpmTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c",
      "--compile", "--vcd", "--targetDir", "generated"),
      () => LLSCSpm(32, 4, 1024)) {
        c => new LLSCSpmTester(c)
      }
  }
}
