package simple

import Chisel._

object Util {
    sealed trait Direction
    case object Left extends Direction
    case object Right extends Direction

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

class Snake(length :Int, blankCount :Int, stillClocks :Int) extends Module {
    import Util._

    val TOT_LENGTH = length + blankCount

    val io = new Bundle {
        val direction = Bool(INPUT)
        val speedMultiplier = UInt(INPUT, 16)
        val out = UInt(OUTPUT, TOT_LENGTH)
    }

    val snake = Reg(init = Cat(Fill(blankCount, Bits(0)),
        Fill(length, Bits(1))))
    val stillCount = Reg(init = UInt(0))

    io.out <> snake

    when (stillCount === UInt(0)) {
        when (io.speedMultiplier =/= UInt(0)) {
            stillCount := UInt(stillClocks) / io.speedMultiplier

            when (io.direction) {
                snake := rotateLeft (snake, 1)
            } otherwise {
                snake := rotateRight (snake, 1)
            }
        }
    } otherwise {
        stillCount := stillCount - UInt(1)
    }
}

class SnakeTop(
        length :Int = 10,
        availableLeds :Int = 18,
        stillClocks :Int = 500000000 - 1
) extends Module {

    val BLANK_LEDS_COUNT = availableLeds - length

    val io = new Bundle {
      val led = UInt(OUTPUT, availableLeds)
      val sw = UInt(INPUT, 17)
    }

    val snake = Module(new Snake(length, BLANK_LEDS_COUNT, stillClocks))

    io.led <> snake.io.out
    io.sw(15, 0) <> snake.io.speedMultiplier
    snake.io.direction := io.sw(16)
}

object SnakeMain {
  def main(args: Array[String]): Unit = {
    chiselMain(Array("--backend", "v", "--targetDir", "generated"),
      () => Module(new SnakeTop()))
  }
}
