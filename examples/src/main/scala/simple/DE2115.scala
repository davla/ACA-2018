package simple

import Chisel._

object DE2115 {

    class BinTo7SegDigit extends Module {

        val io = new Bundle {
            val bin = UInt(INPUT, 4)
            val dec = Bits(OUTPUT, 7)
        }

        io.dec := Bits(0, 7)

        switch (io.bin) {
            is (UInt (0)) {
                io.dec := Bits(0x40, 7)
            }
            is (UInt (1)) {
                io.dec := Bits(0x79, 7)
            }
            is (UInt (2)) {
                io.dec := Bits(0x24, 7)
            }
            is (UInt (3)) {
                io.dec := Bits(0x30, 7)
            }
            is (UInt (4)) {
                io.dec := Bits(0x19, 7)
            }
            is (UInt (5)) {
                io.dec := Bits(0x12, 7)
            }
            is (UInt (6)) {
                io.dec := Bits(0x02, 7)
            }
            is (UInt (7)) {
                io.dec := Bits(0x78, 7)
            }
            is (UInt (8)) {
                io.dec := Bits(0x00, 7)
            }
            is (UInt (9)) {
                io.dec := Bits(0x10, 7)
            }
        }
    }

    object BinTo7SegDigit {
        def apply () =
            Module (new BinTo7SegDigit ())
    }

    class BinTo7Seg (
        bitsCount :Int
    ) extends Module {
        import math.pow
        import Util._

        val OUT_DIGITS = decLength (bitsCount)

        val io = new Bundle {
            val bin = UInt(INPUT, bitsCount)
            val dec = Bits(OUTPUT, OUT_DIGITS * 7)
        }

        io.dec := Bits(0)

        for (k <- 0 until OUT_DIGITS) {
            val decDigit = BinTo7SegDigit ()

            decDigit.io.bin := (io.bin / UInt(pow (10, k).toInt)) % UInt(10)
            io.dec(k * 7 + 6, k * 7) := decDigit.io.dec
        }
    }

    object BinTo7Seg {
        def apply (bitsCount :Int) =
            Module (new BinTo7Seg (bitsCount))
    }
}
