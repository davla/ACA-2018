package simple

import Chisel._

object Util {
    import math.{ceil, log10, pow}

    sealed trait Direction
    case object Left extends Direction
    case object Right extends Direction

    def decLength (binLength :Int) =
        ceil (log10 (pow (2, binLength))).toInt

    def rotate(bits :Bits, shift :Int, direction :Direction) = {
        direction match {
            case Left => {
                val lastVictim = bits.getWidth - 1
                val firstVictim = lastVictim - shift + 1

                val victims = bits(lastVictim, firstVictim)
                val shifted = UInt(bits) << UInt(shift)
                Cat(shifted(firstVictim, shift), victims)
            }

            case Right => {
                val victims = bits(shift - 1, 0)
                Cat(victims, UInt(bits) >> UInt(shift))
            }
        }
    }

    val rotateLeft = (rotate _) (_ :UInt, _ :Int, Left)
    val rotateRight = (rotate _) (_ :UInt, _ :Int, Right)
}
