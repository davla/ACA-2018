/*
 * Copyright: 2014-2018, Technical University of Denmark, DTU Compute
 * Author: Martin Schoeberl (martin@jopdesign.com)
 * License: Simplified BSD License
 * 
 * A UART is a serial port, also called an RS232 interface.
 * 
 */

package uart

import Chisel._

/**
 * This is a minimal AXI style data plus handshake channel.
 */
class Channel extends Bundle {
  val data = Bits(INPUT, 8)
  val ready = Bool(OUTPUT)
  val valid = Bool(INPUT)
}

/**
 * Transmit part of the UART.
 * A minimal version without any additional buffering.
 * Use an AXI like valid/ready handshake.
 */
class Tx(frequency: Int, baudRate: Int) extends Module {
  val io = new Bundle {
    val txd = Bits(OUTPUT, 1)
    val channel = new Channel()
  }

  val BIT_CNT = UInt((frequency + baudRate / 2) / baudRate - 1)

  val shiftReg = Reg(init = Bits(0x7ff))
  val cntReg = Reg(init = UInt(0, 20))
  val bitsReg = Reg(init = UInt(0, 4))

  io.channel.ready := (cntReg === UInt(0)) && (bitsReg === UInt(0))
  io.txd := shiftReg(0)

  // TODO: make the counter a tick generator
  when(cntReg === UInt(0)) {

    cntReg := UInt(BIT_CNT)
    when(bitsReg =/= UInt(0)) {
      val shift = shiftReg >> 1
      shiftReg := Cat(Bits(1), shift(9, 0))
      bitsReg := bitsReg - UInt(1)
    }.otherwise {
      when(io.channel.valid) {
        shiftReg(0) := Bits(0) // start bit
        shiftReg(8, 1) := io.channel.data // data
        shiftReg(10, 9) := Bits(3) // two stop bits
        bitsReg := UInt(11)
      }.otherwise {
        shiftReg := Bits(0x7ff)
      }
    }

  }.otherwise {
    cntReg := cntReg - UInt(1)
  }

  debug(shiftReg)
}

/**
 * Receive part of the UART.
 * A minimal version without any additional buffering.
 * Use an AXI like valid/ready handshake.
 *
 * The following code is inspired by Tommy's receive code at:
 * https://github.com/tommythorn/yarvi
 */
class Rx(frequency: Int, baudRate: Int) extends Module {
  val io = new Bundle {
    val rxd = Bits(INPUT, 1)
    val channel = new Channel().flip
  }

  val BIT_CNT = UInt((frequency + baudRate / 2) / baudRate - 1)
  val START_CNT = UInt((3 * frequency / 2 + baudRate / 2) / baudRate - 1)

  // Sync in the asynchronous RX data
  val rxReg = Reg(next = Reg(next = io.rxd))

  val shiftReg = Reg(init = Bits('A', 8))
  val cntReg = Reg(init = UInt(0, 20))
  val bitsReg = Reg(init = UInt(0, 4))
  val valReg = Reg(init = Bool(false))

  when(cntReg =/= UInt(0)) {
    cntReg := cntReg - UInt(1)
  }.elsewhen(bitsReg =/= UInt(0)) {
    cntReg := BIT_CNT
    shiftReg := Cat(rxReg, shiftReg >> 1)
    bitsReg := bitsReg - UInt(1)
    // the last shifted in
    when(bitsReg === UInt(1)) {
      valReg := Bool(true)
    }
  }.elsewhen(rxReg === UInt(0)) { // wait 1.5 bits after falling edge of start
    cntReg := START_CNT
    bitsReg := UInt(8)
  }

  when(io.channel.ready) {
    valReg := Bool(false)
  }

  io.channel.data := shiftReg
  io.channel.valid := valReg
}

/**
 * A single byte buffer with an AXI style channel
 */
//# Buffer class - component
class Buffer extends Module {
  val io = new Bundle {
    val in = new Channel()	//#declare in assigned with new channel obj
    val out = new Channel().flip //#declare out assigned with new channel obj fliped 
  }

  val empty :: full :: Nil = Enum(UInt(), 2) //# declare states
  val stateReg = Reg(init = empty)			 //# declare state register and initialize to empty
  val dataReg = Reg(init = Bits(0, 8))		 //# declare data register 

  io.in.ready := stateReg === empty			 //# assign io.in.ready with true when stateReg === empty, tell buffered transmitter data is ready 
  io.out.valid := stateReg === full			 //# assign io.out.valid with true when stateReg === full, tell transmitter to shift data

  when(stateReg === empty) {				 //# when state is empty
    when(io.in.valid) {						 //# if io.in.valid is true then 
      dataReg := io.in.data					 //# assign input data to dataReg
      stateReg := full						 //# next state is changed to full
    }
  }.otherwise { // full, io.out.valid := true //# when state is not empty
    when(io.out.ready) {					  //# if io.out.ready is true then
      stateReg := empty						  //# next state is change to empty 
    }
  }
  io.out.data := dataReg					  //# output io.out.data is assigned by dataReg
}

/**
 * A transmitter with a single buffer.
 */
//# BufferedTx class - component
class BufferedTx(frequency: Int, baudRate: Int) extends Module {
  val io = new Bundle {
    val txd = Bits(OUTPUT, 1)	//#declare transmitt data txd as output 
    val channel = new Channel() //#assign new Channel obj to channel
  }
  val tx = Module(new Tx(frequency, baudRate)) //#declare tx assign with new Tx obj
  val buf = Module(new Buffer())			   //#declare buf assign with new Buffer obj

  buf.io.in <> io.channel					   //#connect BufferedTx io.channel to buffer io in(channel obj), data in, valid(full state) flag in, ready(empty state)flag out
  tx.io.channel <> buf.io.out				   //#connect buf.io.out(channel obj), data out, valid(full state) flag out, ready(empty state)flag in to tx.io.channel
  io.txd <> tx.io.txd						   //#connect tx.io.txd to BufferedTx io txd
}

/**
 * Send 'hello'.
 */
//# Sender class - component
class Sender1(frequency: Int, baudRate: Int) extends Module {
  val io = new Bundle {
    val txd = Bits(OUTPUT, 1) //# declare transmitt txd as output
	val led = Bits(OUTPUT, 1)
  }

  val tx = Module(new BufferedTx(frequency, baudRate)) //#assign tx with new BuuferedTx object

  io.txd := tx.io.txd								   //#assign io.txd with tx.io.txd, bypass

  // This is not super elegant
//  val hello = Array[Bits](Bits('H'), Bits('e'), Bits('l'), Bits('l'), Bits('o'))
//  val text = Vec[Bits](hello)

//  val cntReg = Reg(init = UInt(0, 3))

//  tx.io.channel.data := text(cntReg)
//  tx.io.channel.valid := cntReg =/= UInt(5)

//  when(tx.io.channel.ready && cntReg =/= UInt(5)) {
//    cntReg := cntReg + UInt(1)
//  }					
  val cmdReg = Reg(init = UInt(0, 8))
  val validReg = Reg(init = UInt(0, 1))
  tx.io.channel.data := cmdReg		
  tx.io.channel.valid := validReg =/= UInt(1)
  val cntReg = Reg(init = UInt(frequency / 2 - 1, 32))
  val blkReg = Reg(init = UInt(0, 1))
//  cntReg := cntReg - UInt(1)					--bad example
//  when(cntReg === UInt(0)) {
//    cntReg := UInt(frequency / 2 - 1)
//    blkReg := ~blkReg
//    when(blkReg === UInt(0)){
//      cmdReg := UInt("h30")
//    }.elsewhen(blkReg === UInt(1)){
//	  cmdReg := UInt("h31")
//    }
//	when(tx.io.channel.ready) { 
//      validReg := UInt(1)	
//    }
//  }
  when(cntReg =/= UInt(0)) {
    cntReg := cntReg - UInt(1)
	validReg := UInt(1)
  }.elsewhen(cntReg === UInt(0)){
    cntReg := UInt(frequency / 2 - 1)
    blkReg := ~blkReg
    when(blkReg === UInt(0)){
      cmdReg := UInt("h30")
    }.elsewhen(blkReg === UInt(1)){
	  cmdReg := UInt("h31")
    }
	when(tx.io.channel.ready) { 
      validReg := UInt(0)	
    }
  }
  io.led := blkReg
}

class Sender2(frequency: Int, baudRate: Int) extends Module {
  val io = new Bundle {
    val txd = Bits(OUTPUT, 1) //# declare transmitt txd as output
	val led = Bits(OUTPUT, 1)
  }

  val tx = Module(new BufferedTx(frequency, baudRate)) //#assign tx with new BuuferedTx object

  io.txd := tx.io.txd								   //#assign io.txd with tx.io.txd, bypass

  // This is not super elegant
  val number = Array[Bits](Bits('9'), Bits('8'), Bits('7'), Bits('6'), Bits('5'), Bits('4'), Bits('3'), Bits('2'), Bits('1'), Bits('0'))
  val text = Vec[Bits](number)
  val validReg = Reg(init = UInt(0, 1))
  val cntReg = Reg(init = UInt(10, 4))
  val blkReg = Reg(init = UInt(0, 1))
  tx.io.channel.data := text(cntReg)
  tx.io.channel.valid := validReg =/= UInt(1)
  when(tx.io.channel.ready && cntReg =/= UInt(0)){
	cntReg := cntReg - UInt(1)
    validReg := UInt(0)
  }.elsewhen(tx.io.channel.ready && cntReg === UInt(0)){
    validReg := UInt(1)
	cntReg := UInt(10)
  }
  io.led := blkReg
}

class Led(frequency: Int) extends Module {
  val io = new Bundle {
    val led = UInt(OUTPUT, 1)
  }
  val CNT_MAX = UInt(frequency / 2 - 1);
  
  val cntReg = Reg(init = UInt(0, 32))
  val blkReg = Reg(init = UInt(0, 1))

  cntReg := cntReg + UInt(1)
  when(cntReg === CNT_MAX) {
    cntReg := UInt(0)
    blkReg := ~blkReg
  }
  io.led := blkReg
}

class Echo(frequency: Int, baudRate: Int) extends Module {
  val io = new Bundle {
    val txd = Bits(OUTPUT, 1)
    val rxd = Bits(INPUT, 1)
  }
  // io.txd := Reg(next = io.rxd, init = UInt(0))
  val tx = Module(new BufferedTx(frequency, baudRate))
  val rx = Module(new Rx(frequency, baudRate))
  io.txd := tx.io.txd
  rx.io.rxd := io.rxd
  tx.io.channel <> rx.io.channel
  tx.io.channel.valid := Bool(true)	
//  tx.io.channel.data := Bits('H')
}
//# UartMain class - component
class UartMain(frequency: Int, baudRate: Int) extends Module {
  val io = new Bundle {
    val rxd = Bits(INPUT, 1)	//# declare recieve data rxd as input
    val txd = Bits(OUTPUT, 1)	//# declare transmitt data txd as output
	val ledG = Bits(OUTPUT, 1)
    val ledR = Bits(OUTPUT, 1)
  }
  
  val u = Module(new Sender2(50000000, 115200))	//# assign u with new Sender obj with argument
  // val u = Module(new Echo(50000000, 115200))
  io.txd := u.io.txd							//# assign UartMain component io.txd with sender io txd, bypassing txd from sender
  // u.io.rxd := io.rxd
  val l = Module(new Led(frequency))
  io.ledG := l.io.led
  io.ledR := u.io.led
}
//#chisel main
object UartMain {
  def main(args: Array[String]): Unit = {
    chiselMain(Array[String]("--backend", "v", "--targetDir", "generated"),
      () => Module(new UartMain(50000000, 115200))) //#pass argument to new object UartMain
  }
}

