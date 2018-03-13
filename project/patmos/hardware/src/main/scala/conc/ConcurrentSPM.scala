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
) extends SharedSPM(nrCores, size) {
    import ConcurrentSPM._

    if (!isPow2 (granularity)) {
        sys.error (s"ConcurrentSPM: granularity must be a power of 2, but $granularity was provided.")
    }

    // Check for other exceptions

    val statusBits = Bits (0, width=(size / granularity))

    val thisStatusBit = getStatusBit (spm.io.M.Addr)
    val cmd = nd.map(_.io.toMem.M.Cmd).reduce((x, y) => x | y)

    spm.io.M.Cmd := OcpCmd.IDLE

    switch (cmd) {

        // Every read is a Load Linked
        is (OcpCmd.RD) {
            thisStatusBit := StatusBit.PRISTINE
        }

        // Every write is a Store Conditional
        is (OcpCmd.WR) {
            // We need to write success/failure as output
            when (thisStatusBit === StatusBit.PRISTINE) {
                thisStatusBit := StatusBit.DIRTY
                spm.io.M.Cmd := cmd
            }
        }
    }

    def getStatusBit (address: UInt) = {
         statusBits (address >> log2Up (granularity))
    }
}

object ConcurrentSPM {
    object StatusBit {
        val PRISTINE = Bits(0, width=1)
        val DIRTY = Bits(1, width=1)
    }

    def apply (granularity :Int, nrCores :Int, size :Int) =
        Module (new ConcurrentSPM (granularity, nrCores, size))
}
