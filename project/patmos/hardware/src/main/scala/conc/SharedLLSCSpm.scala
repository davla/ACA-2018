/*
 * A shared scratchpad memory supporting LL/SC, with
 * time allocated arbitration.
 *
 * Author: Davide Laezza - Roberts Fanning - Wenhao Li
 */

package conc

import Chisel._

import conc._
import conc.Util._
import ocp._
import patmos.Constants._

class SharedLLSCSpm(
    granularity :Int,
    nrCores: Int,
    size: Int
) extends Module {
    // ncCores OCP slaves to connect to the cores
    val io = Vec.fill(nrCores)(new OcpCoreSlavePort(ADDR_WIDTH, DATA_WIDTH))

    // The LL/SC scratchpad memory
    val mem = LLSCSpm(granularity, nrCores, size)

    val arbiters = new Array[Arbiter](nrCores)
    for (i <- 0 until nrCores) {
        arbiters(i) = Arbiter(i, nrCores, 1)
        val arbiter = arbiters(i)

        arbiter.io.slave <> io(i)
        arbiter.io.master.S <> mem.io.slave.S
    }

    mem.io.slave.M <> orAllOcpMaster(arbiters.map(_.io.master.M))
    mem.io.core := orAll(arbiters.map(_.io.core))
}

object SharedLLSCSpm {
    def apply(granularity :Int, nrCores: Int, size: Int) = {
        Module(new SharedLLSCSpm(granularity, nrCores, size))
    }
}

class SharedLLSCSpmTester(dut: SharedLLSCSpm) extends Tester(dut) {

  println("Shared LL/SC SPM Tester")

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
        for (k <- 0 until 4) {
            peek(dut.io(k).S.Resp)
        }
      step(1)
    }
  }

  for (i <- 0 until 32) {
    write(0, i, i * 0x100 + 0xa)
    // write(1, i+32, i * 0x10000 + 0xb)
  }
  step(1)
  for (i <- 0 until 32) {
    expect(read(0, i), i * 0x100 + 0xa)
    // expect(read(1, i+32), i * 0x10000 + 0xb)
  }
}

object SharedLLSCSpmTester {
  def main(args: Array[String]): Unit = {
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c",
      "--compile", "--vcd", "--targetDir", "generated"),
      () => SharedLLSCSpm(64, 4, 1024)) {
        c => new SharedLLSCSpmTester(c)
      }
  }
}
