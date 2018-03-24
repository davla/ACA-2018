package conc

import Chisel._

import ocp._

object Util {
    def orAll[T <: Bits](items :Array[T]) = items.reduce(_ | _)

    def orAllOcpMaster(ports :Array[OcpCoreMasterSignals]) = {
        val first = ports.head
        val out = new OcpCoreMasterSignals(first.Addr.getWidth,
            first.Data.getWidth)

        out.Addr := orAll(ports.map(_.Addr))
        out.ByteEn := orAll(ports.map(_.ByteEn))
        out.Cmd := orAll(ports.map(_.Cmd))
        out.Data := orAll(ports.map(_.Data))

        out
    }
}
