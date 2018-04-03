/*
 * A local OCP arbiter with delay.
 *
 * Author: Davide Laezza - Roberts Fanning - Wenhao Li
 */

package conc

import Chisel._
import conc.Util._
import ocp._
import patmos.Constants._

/**

 Local arbitration with the TDM counter and a tiny FSM.

   * Tree to the memory is node local enabled AND and an OR for merge
   * Three state FSM: idle, read, write
   * loose one cycle by registering the command and switching state
   * local counter tells when slot is there, drives the and gate
   * DVA generated from FSM, not from memory
     * read data comes from memory one cycle after slot, dva is simply a register delay
     * write dva also delayed to be back in idle for pipelined operation, check this out with Patmos or a test case
   * memory DVA is ignored as we know the timing
   * maybe test also byte and word access, should work as in any other spm
 */
class Arbiter(
    id: Int,
    nrCores: Int,
    delay :Int = 1
) extends Module {

    val io = new Bundle() {
        val slave = new OcpCoreSlavePort(ADDR_WIDTH, DATA_WIDTH)
        val master = new OcpCoreMasterPort(ADDR_WIDTH, DATA_WIDTH)
        val core = UInt(OUTPUT, log2Up(nrCores))
    }

    val waitingCmd :: waitingTurn :: waitingDva :: Nil = Enum(UInt(), 3)
    val state = RegInit(waitingCmd)

    // TODO: how to reset with a harmless IDLE command?
    val masterReg = RegInit(io.slave.M)
    val dvaRepl = Reg(init = OcpResp.NULL, next = OcpResp.NULL)

    val cnt = ClockCounter(nrCores)
    val enabled = cnt.io.n === UInt(id)
    val dva = cnt.io.n === UInt((id + delay) % nrCores)

    // Data comes from the SPM, response from the FSM
    io.slave.S.Resp := dvaRepl
    io.slave.S.Data := io.master.S.Data

    io.master.M.Addr := UInt(0)
    io.master.M.Data := UInt(0)
    io.master.M.Cmd := UInt(0)
    io.master.M.ByteEn := UInt(0)

    io.core := UInt(0)

    switch (state) {
        is (waitingCmd) {
            masterReg := io.slave.M
            state := Mux(io.slave.M.Cmd === OcpCmd.IDLE,
                    waitingCmd, waitingTurn)
        }

        is (waitingTurn) {
            when (enabled) {
                masterReg.Cmd := OcpCmd.IDLE
                io.master.M := masterReg
                io.core := UInt(id)
                state := waitingDva
            }
        }

        is (waitingDva) {
            when (dva) {
                dvaRepl := OcpResp.DVA
                state := waitingCmd
            }
        }
    }
}

object Arbiter {
    def apply(id: Int, nrCores :Int, delay :Int) = {
        Module(new Arbiter(id, nrCores, delay))
    }

    def apply(id :Int, nrCores :Int) = {
        Module(new Arbiter(id, nrCores))
    }
}
