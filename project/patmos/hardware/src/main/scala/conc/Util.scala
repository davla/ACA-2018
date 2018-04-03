package conc

import Chisel._

import ocp._

object Util {
    def catAll[T <: Bits](items :Array[T]) = items.reduce(_ ## _)

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

    class ClockCounter(max :Int) extends Module {
        val io = new Bundle() {
            val n = UInt(OUTPUT, log2Up(max))
        }

        val cnt = Reg(UInt(0, log2Up(max)))
        io.n := cnt
        cnt := Mux(cnt === UInt(max - 1), UInt(0), cnt + UInt(1))
    }

    object ClockCounter {
        def apply(n :Int) = {
            Module(new ClockCounter(n))
        }
    }
}
