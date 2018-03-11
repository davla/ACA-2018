package simple

import Chisel._

class Snake (
        length :Int,
        blankCount :Int,
        multLength :Int,
        stillClocks :Int
) extends Module {

    import DE2115._
    import Util._

    val TOT_LENGTH = length + blankCount
    val MULT_DEC_LENGTH = decLength (multLength) * 7

    val io = new Bundle {
        val decDigit = Bits(OUTPUT, MULT_DEC_LENGTH)
        val direction = Bool(INPUT)
        val out = UInt(OUTPUT, TOT_LENGTH)
        val speedMultiplier = UInt(INPUT, multLength)
        val stop = Bool(INPUT)
    }

    val snake = Reg(init = Cat(Fill(blankCount, Bits(0)),
        Fill(length, Bits(1))))
    val stillCount = Reg(init = UInt(0))

    val bin2Dec = BinTo7Seg (multLength)

    io.out := snake

    bin2Dec.io.bin := io.speedMultiplier
    io.decDigit := bin2Dec.io.dec

    when (io.stop || io.speedMultiplier === UInt(0)) {
        stillCount === UInt(0)

    }.elsewhen (stillCount === UInt(0)) {
        stillCount := UInt(stillClocks) / io.speedMultiplier

        when (io.direction) {
            snake := rotateLeft (snake, 1)
        } otherwise {
            snake := rotateRight (snake, 1)
        }

    }.otherwise {
        stillCount := stillCount - UInt(1)
    }
}

object Snake {
    def apply(
            length :Int,
            blankCount :Int,
            multLength :Int,
            stillClocks :Int
    ) =
        Module (new Snake (length, blankCount, multLength, stillClocks))
}

class SnakeTop (
        length :Int = 10,
        availableLeds :Int = 18,
        availableSwitches :Int = 17,
        stillClocks :Int = 500000000 - 1
) extends Module {

    import Util._

    val BLANK_LEDS_COUNT = availableLeds - length
    val SPEED_MULT_LENGTH = 10
    val DEC_DIGITS_LENGTH = decLength (SPEED_MULT_LENGTH) * 7

    val io = new Bundle {
        val btn = UInt(INPUT, 1)
        val led = UInt(OUTPUT, availableLeds)
        val sw = UInt(INPUT, availableSwitches)
        val dec = UInt(OUTPUT, DEC_DIGITS_LENGTH)
    }

    val snake = Snake(
            length,
            BLANK_LEDS_COUNT,
            SPEED_MULT_LENGTH,
            stillClocks
        )

    io.dec := snake.io.decDigit
    io.led := snake.io.out
    snake.io.direction := io.sw (availableSwitches - 1)
    snake.io.speedMultiplier := io.sw (SPEED_MULT_LENGTH - 1, 0)
    snake.io.stop := ~io.btn
}

object SnakeMain {
  def main(args: Array[String]): Unit = {
    chiselMain(Array("--backend", "v", "--targetDir", "generated"),
      () => Module(new SnakeTop()))
  }
}
