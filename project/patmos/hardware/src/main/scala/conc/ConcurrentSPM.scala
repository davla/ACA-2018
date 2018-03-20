/*
 * A concurrent shared scratchpad memory.
 *
 * Author: Davide Laezza - Roberts Fanning - Wenhao Li
 */

package conc

import Chisel._

import patmos._
import cmp._
import patmos.Constants._
import ocp._

class ConcurrentSPM (
    val granularity :Int,
    nrCores: Int,
    size: Int
) extends Module {
    import ConcurrentSPM._

    if (!isPow2 (granularity)) {
        sys.error (s"ConcurrentSPM: granularity must be a power of 2, but $granularity was provided.")
    }

    // Check for other exceptions

    val io = Vec(nrCores, new OcpCoreSlavePort(ADDR_WIDTH, DATA_WIDTH))
    val spm = Module(new Spm(size))
    val nd = createArbiters (nrCores)
    wireSPM (spm)

    val statusBits = Bits (0, width=(size / granularity))

    val thisStatusBit = getStatusBit (spm.io.M.Addr)

    switch (nd.map(_.io.toMem.M.Cmd).reduce((x, y) => x | y)) {

        // Every read is a Load Linked
        is (OcpCmd.RD) {
            thisStatusBit := StatusBit.PRISTINE
            // Resp is set to DVA by spm since we set CMD to RD
            spm.io.M.Cmd := OcpCmd.RD
        }

        // Every write is a Store Conditional
        is (OcpCmd.WR) {
            when (thisStatusBit === StatusBit.PRISTINE) {
                thisStatusBit := StatusBit.DIRTY
                spm.io.M.Cmd := OcpCmd.WR
                // Resp is set to DVA by spm since we set CMD to WR
                spm.io.S.Data := StatusCode.SUCCESS
            }.otherwise {
                spm.io.S.Resp := OcpResp.DVA
                spm.io.S.Data := StatusCode.FAILURE
            }
        }
    }

    def createArbiters(nrCores :Int) = {
        val nd = new Array[NodeSPM](nrCores)
        for (i <- 0 until nrCores) {
          nd(i) = Module(new NodeSPM(i, nrCores))
          nd(i).io.fromCore <> io(i)
          nd(i).io.toMem.S <> spm.io.S
        }
        nd
    }

    def getStatusBit (address: UInt) = {
         statusBits (address >> log2Up (granularity))
    }

    def wireSPM(spm :Spm) = {
        // Or the master signals
        spm.io.M.Addr := nd.map(_.io.toMem.M.Addr).reduce((x, y) => x | y)
        spm.io.M.Data := nd.map(_.io.toMem.M.Data).reduce((x, y) => x | y)
        spm.io.M.ByteEn := nd.map(_.io.toMem.M.ByteEn).reduce((x, y) => x | y)

        spm.io.M.Cmd := OcpCmd.IDLE
    }
}

object ConcurrentSPM {
    object StatusBit {
        val PRISTINE = Bits(0, width=1)
        val DIRTY = Bits(1, width=1)
    }

    object StatusCode {
        val SUCCESS = UInt(0)
        val FAILURE = UInt(1)
    }

    def apply (granularity :Int, nrCores :Int, size :Int) =
        Module (new ConcurrentSPM (granularity, nrCores, size))
}
