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

class SharedTransSpm(
    granularity :Int,
    nrCores: Int,
    size: Int
) extends Module {
    // ncCores OCP slaves to connect to the cores
    val io = Vec.fill(nrCores)(new OcpCoreSlavePort(ADDR_WIDTH, DATA_WIDTH))

    // The LL/SC scratchpad memory
    val mem = TransSpm(granularity, nrCores, size)

    val arbiters = new Array[Arbiter](nrCores)
    for (i <- 0 until nrCores) {
        arbiters(i) = Arbiter(i, nrCores, 0)
        val arbiter = arbiters(i)

        arbiter.io.slave <> io(i)
        arbiter.io.master.S <> mem.io.slave.S
    }

    mem.io.slave.M <> orAllOcpMaster(arbiters.map(_.io.master.M))
    mem.io.core := orAll(arbiters.map(_.io.core))
}

object SharedTransSpm {
    def apply(granularity :Int, nrCores: Int, size: Int) = {
        Module(new SharedTransSpm(granularity, nrCores, size))
    }
}

class SharedTransSpmTester(dut: SharedTransSpm) extends Tester(dut) {
    import SharedTransSpmTester._

  println("Shared Transactional SPM Tester")

  def read(n :Int, addr: Int) = {
    println("---------------------------")
    poke(dut.io(n).M.Addr, addr)
    poke(dut.io(n).M.Cmd, 2) // OcpCmd.RD
    step(1)
    poke(dut.io(n).M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io(n).S.Resp) != 1) {
      step(1)
    }
    peek(dut.io(n).S.Data)
    dut.io(n).S.Data
  }

  def write(n :Int, addr: Int, data: Int) = {
    println("---------------------------")
    poke(dut.io(n).M.Addr, addr)
    poke(dut.io(n).M.Data, data)
    poke(dut.io(n).M.Cmd, 1) // OcpCmd.WR
    poke(dut.io(n).M.ByteEn, 0x0f)
    step(1)
    poke(dut.io(n).M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io(n).S.Resp) != 1) {
      step(1)
    }
    peek(dut.io(n).S.Data)
    dut.io(n).S.Data
  }

  // Basic read-write test
  for (i <- 0.until(1024, GRANULARITY)) {
    expect(write(0, i, i * 0x100 + 0xa), 0)
  }
  step(1)
  for (i <- 0.until(1024, GRANULARITY)) {
    expect(read(0, i), i * 0x100 + 0xa)
  }

  // Transactional tests

  // Simple failed commit on a single variable
  read(0, 0)

  expect(write(1, 0, 0xFF), 0)

  expect(write(0, 0, 0xFF), 1)

  // Failed commit on more than one variable
  for (addr <- 0 until(256, GRANULARITY)) {
      read(0, addr)
  }

  for (addr <- 0 until(256, GRANULARITY)) {
      expect(write(1, addr, 0xFF), 0)
  }

  expect(write(0, 128, 0xFF), 1)

  // Even writing one variable makes the whole transaction not commit
  for (addr <- 0 until(256, GRANULARITY)) {
      read(0, addr)
  }

  expect(write(1, 128, 0xFF), 0)

  expect(write(0, 64, 0xFF), 1)

  // Reading a variable while in transaction after it has been written by
  // another core makes the commit fail

  for (addr <- 0 until(64, GRANULARITY)) {
      read(0, addr)
  }

  for (addr <- 64 until(256, GRANULARITY)) {
      expect(write(1, addr, 0xFF), 0)
  }

  read(0, 128)
  expect(write(0, 0, 0xFF), 1)

  // A writes terminates the current transaction, regardless of its result

  // Commited transaction
  for (addr <- 0 until(256, GRANULARITY)) {
      read(0, addr)
  }

  expect(write(0, 128, 0xFF), 0)
  expect(write(0, 512, 0xFF), 0)

  // Uncommited transaction
  for (addr <- 0 until(256, GRANULARITY)) {
      read(0, addr)
  }

  expect(write(1, 128, 0xFF), 0)

  expect(write(0, 64, 0xFF), 1)
  expect(write(0, 512, 0xFF), 0)
}

object SharedTransSpmTester {
    val GRANULARITY = 32

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c",
      "--compile", "--vcd", "--targetDir", "generated"),
      () => SharedTransSpm(GRANULARITY, 4, 1024)) {
        c => new SharedTransSpmTester(c)
      }
  }
}
