/*
 * A concurrent shared scratchpad memory.
 *
 * Author: Davide Laezza - Roberts Fanning - Wenhao Li
 */

package conc

import Chisel._

import conc.Util._
import cmp._
import ocp._
import patmos._
import patmos.Constants._

class LLSCOcpAdapter(
    addrWidth :Int,
    dataWidth :Int
) extends Module {
    import LLSCOcpAdapter._

    val io = new Bundle() {
        val master = new OcpCoreMasterPort(addrWidth, dataWidth)
        val slave = new OcpCoreSlavePort(addrWidth, dataWidth)
        val statusBitIn = UInt(INPUT, width = 1)
        val statusBitOut = UInt(OUTPUT, width = 1)
    }

    val masterReg = Reg(next = io.slave.M)
    val slaveReg = Reg(next = io.master.S)
    val statusBitReg = Reg(next = io.statusBitIn)

    io.master.M := masterReg
    io.slave.S := slaveReg
    io.statusBitOut := statusBitReg


    switch (io.slave.M.Cmd) {
        is (OcpCmd.RD) {
            statusBitReg := StatusBit.PRISTINE
        }

        is (OcpCmd.WR) {
            slaveReg.Resp := OcpResp.DVA

            when (io.statusBitIn === StatusBit.PRISTINE) {
                statusBitReg := StatusBit.DIRTY
                slaveReg.Data := StatusCode.SUCCESS

            }.otherwise {
                masterReg.Cmd := OcpCmd.IDLE
                slaveReg.Data := StatusCode.FAILURE
            }
        }
    }
}

object LLSCOcpAdapter {
    object StatusBit {
        val PRISTINE = Bits(0, width=1)
        val DIRTY = Bits(1, width=1)
    }

    object StatusCode {
        val SUCCESS = UInt(0)
        val FAILURE = UInt(1)
    }

    def apply(addrWidth :Int, dataWidth :Int) = {
        Module(new LLSCOcpAdapter(addrWidth, dataWidth))
    }
}

class ConcurrentSPM (
    val granularity :Int,
    nrCores: Int,
    size: Int
) extends Module {
    import LLSCOcpAdapter._

    if (!isPow2 (granularity)) {
        sys.error (s"ConcurrentSPM: granularity must be a power of 2, but $granularity was provided.")
    }

    // Check for other exceptions

    // The actual scratchpad memory
    val spm = Module(new Spm(size))

    // ncCores OCP slaves to connect to the cores
    val io = Vec.fill(nrCores)(new OcpCoreSlavePort(ADDR_WIDTH, DATA_WIDTH))

    val statusBits = Reg (init = UInt(StatusBit.PRISTINE, width=(size / granularity)))

    // Bridges from OCP slaves to the Scratchpad Memory
    val adapters = new Array[LLSCOcpAdapter](nrCores)
    val arbiters = new Array[NodeSPM](nrCores)
    for (i <- 0 until nrCores) {
        adapters(i) = LLSCOcpAdapter(ADDR_WIDTH, DATA_WIDTH)
        arbiters(i) = Module(new NodeSPM(i, nrCores))

        val llscAdapter = adapters(i)
        val arbiter = arbiters(i)

        arbiter.io.fromCore <> io(i)
        arbiter.io.toMem <> llscAdapter.io.slave

        llscAdapter.io.master.S <> spm.io.S
    }

    val address = orAll(arbiters.map(_.io.toMem.M.Addr))
    val currStatusBit = getStatusBit(address)

    adapters.foreach (_.io.statusBitIn := currStatusBit)
    currStatusBit := orAll(adapters.map(_.io.statusBitOut))

    // OR-ing master inputs to memory
    spm.io.M <> orAllOcpMaster(adapters.map(_.io.master.M))

    def getStatusBit(address: UInt) = {
         statusBits(address >> log2Up(size / granularity))
    }
}

object ConcurrentSPM {
    def apply (granularity :Int, nrCores :Int, size :Int) =
        Module (new ConcurrentSPM (granularity, nrCores, size))
}

class ConcurrentSPMTester(dut: ConcurrentSPM) extends Tester(dut) {

  println("Concurrent SPM Tester")

  def read(n: Int, addr: Int) = {
    poke(dut.io(n).M.Addr, addr << 2)
    poke(dut.io(n).M.Cmd, 2) // OcpCmd.RD
    step(1)
    poke(dut.io(n).M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io(n).S.Resp) != 1) {
      step(1)
    }
    peek(dut.io(n).S.Data)
    dut.io(n).S.Data
  }

  def write(n: Int, addr: Int, data: Int) = {
    poke(dut.io(n).M.Addr, addr << 2)
    poke(dut.io(n).M.Data, data)
    poke(dut.io(n).M.Cmd, 1) // OcpCmd.WR
    poke(dut.io(n).M.ByteEn, 0x0f)
    step(1)
    poke(dut.io(n).M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io(n).S.Resp) != 1) {
      step(1)
    }
  }


  for (i <- 0 until 32) {
    write(0, i, i * 0x100 + 0xa)
    write(1, i+32, i * 0x10000 + 0xb)
  }
  step(1)
  for (i <- 0 until 32) {
    expect(read(0, i), i * 0x100 + 0xa)
    expect(read(1, i+32), i * 0x10000 + 0xc)
  }
}

object ConcurrentSPMTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c",
      "--compile", "--vcd", "--targetDir", "generated"),
      () => Module(new ConcurrentSPM(64, 4, 1024))) {
        c => new ConcurrentSPMTester(c)
      }
  }
}
