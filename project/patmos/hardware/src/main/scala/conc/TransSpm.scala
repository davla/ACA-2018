/*
 * A scratchpad memory supporting transactional memory
 *
 * Author: Davide Laezza - Roberts Fanning - Wenhao Li
 */

package conc

import Chisel._

import conc.Util._
import ocp._
import patmos._
import patmos.Constants._

/*
    The StatusFlags module creates a pair of flags for each core at every
    addressable memory locaiton. These flags signal whether a location is
    involved in a transaction for a certain core and whether the address has
    been written to since the last read/commit. This means that they carry
    all the necessary information to determine whether a core can commit.
*/
class StatusFlags (
    nrCores :Int,
    size :Int
) extends Module {
    import StatusFlags._

    /*
      addr      : the address of the memory location whose dirty bits are operated
      core      : the core id whose dirty bit is dealt with
      wrEnable  : write enable to allowing for the writing of the flags
      data      : data is the pair of flags that serve as input when writing
      canCommit : coreBit is the output status of each dirty bit for the cores

    */
    val io = new Bundle() {
        val addr = Bits(INPUT, log2Up(size))
        val core = Bits(INPUT, log2Up(nrCores))
        val wrEnable = Bool(INPUT)
        val data = Bits(INPUT, stateWidth)
        val canCommit = Bool(OUTPUT)
    }

    /*
        Status flags.
        Implemented as a register file with a register for every core.
        Each register holds the status flags for its core at every location.
        This allows cheap overwriting of all the status flags for a core.
    */
    val statusFlags = Vec.fill(nrCores)(RegInit(makeInit(size)))

    /*
        These flags tell wether a core can commit, and are updated when the
        status flags are written
    */
    val canCommit = RegInit(Fill(nrCores, Bool(true)))

    /*
        These flags tell wether a core is in a transaction. They are used when
        reading, since the first read should begin a transaction.
    */
    val inTransaction = RegInit(Fill(nrCores, Bool(false)))

    // Read
    // After the shift, the bit for this core is always at position 0
    io.canCommit := (canCommit >> io.core)(0)

    // All the flag for the input core
    val coreFlags = statusFlags(io.core)

    // The shift to apply to the core flags to read the ones for the input address
    val addrShift = io.addr << indexShift

    // After the shift, the bit for this core is always at position 0
    val coreInTransaction = (inTransaction >> io.core)(0)

    // Write: when the statusFlags write enable signal is high one of the
    //        following can occur:
    //          - The *transaction flag* is set to NOT_IN_TRANSACTION, meaning
    //              that the current transaction is finished. In this case,
    //              the dirty flag set to PRISTINE is ignored, as it would
    //              signify a read operation, that mplies that the transaction
    //              is still ongoing.
    //          - The *dirty flag* is set to DIRTY, meaning that the memory
    //              location has been successfully written to. This updates the
    //              canCommit flag and the dirty flag of all the other cores.
    //              As a transaction is finished at the first successful write,
    //              this input is ignored if the *transaction flag* is set to
    //              IN_TRANSACTION.
    //          - The *dirty flag* is set to PRISTINE, meaning that a location
    //              has been read. This updates the canCommit flag of the
    //              current core only. Since a transaction is not finished on
    //              a read operation, the inTransaction flag is always set in
    //              this case.
    when (io.wrEnable) {

        // Transaction finished
        when (io.data(1) === NOT_IN_TRANSACTION) {

            // Resetting transaction flag. The mask sets the core's bit to 0
            inTransaction := inTransaction & ~(UInt(1) << io.core)

            // Setting the canCommit flag. The mask sets the core's bit to 1
            canCommit := canCommit | (UInt(1) << io.core)

            /*
                The transaction is over, meaning that all the status flags
                can be reset
            */
            coreFlags := Bits(0)

            // A write has been succesfully carried out
            when (io.data(0) === DIRTY) {

                // This mask is used to set the dirty flag to 1.
                val bitMask = UInt(1) << addrShift

                /*
                    An array of multiplexers setting the status flags of each
                    core. For the current one, they are set to 0, otherwise
                    the bit mask is used to set to 1 the dirty flag of the
                    current memory location.
                */
                val newStates = new Array[UInt](nrCores)
                for (core <- 0 until nrCores) {
                    val coreFlags = statusFlags(core) | bitMask
                    newStates(core) = Mux(UInt(core) === io.core, Bits(0),
                            coreFlags)
                    statusFlags(core) := newStates(core)
                }

                // Updating the canCommit flags with the new status flags
                canCommit := updateCommits(Vec(newStates.toSeq))
            }

        // Transaction ongoing and read performed
        }.elsewhen (io.data(0) === PRISTINE) {

            // Resetting transaction flag. The mask sets the core's bit to 1
            inTransaction := inTransaction | (UInt(1) << io.core)

            // This mask is used to set the in transaction flag to 1.
            val bitMask = UInt(1) << (addrShift + UInt(1))

            /*
                If the core is in a transaction, a read can cause it to fail
                to commit, if the address has been written to since the start
                of the transaction.
            */
            when (coreInTransaction) {

                // Setting the in transaction flag
                val newcoreFlags = coreFlags | bitMask
                coreFlags := newcoreFlags

                /*
                    An array of multiplexers, outputting the new states only
                    for the current core. They are used to update the
                    canCommit flags.
                */
                val newStates = new Array[UInt](nrCores)
                for (core <- 0 until nrCores) {
                    newStates(core) = Mux(UInt(core) === io.core,
                            newcoreFlags, statusFlags(core))
                }

                // Updating the canCommit flags with the new status flags
                canCommit := updateCommits(Vec(newStates.toSeq))

            /*
                If the core is not in a transaction, it has to be started.
                This means that the status flags for this core need to be
                cleared and its canCommit flag has to be set.
            */
            }.otherwise {

                /*
                    All flags except for the in transaction flag for the
                    current location have to be set to 0. This happens to be
                    exactly the bit mask.
                */
                coreFlags := bitMask

                // Setting the canCommit flag. The mask sets the core's bit to 1
                canCommit := canCommit | (UInt(1) << io.core)
            }
        }
    }

    // This method returns the flags for the given core at the current address
    def getCoreFlags(states :Vec[UInt], core :Int) = {
        /*
            The status flags of a core are always the least significan bits
            after shifting.
        */
        (states(core) >> addrShift)(stateWidth - 1, 0)
    }

    // This method updates the canCommit flags of all cores
    def updateCommits(states :Vec[UInt]) = {

        /*
            An array of comparators and AND ports. These reset the canCommit
            flags whenever both status flags are set, since the commit fail
            condition is a dirty location involved in a transaction. Furthermore,
            this condition should be kept until reset explicitly, that is the
            reason that makes an AND port necessary.
        */
        val newCommits = new Array[Bool](nrCores)
        for (core <- 0 until nrCores) {
            val coreState = getCoreFlags(states, core)
            val isOk = coreState =/= (IN_TRANSACTION ## DIRTY)
            newCommits(core) = canCommit(core) && isOk
        }

        // Concatenating themultiplexed in reversed order, to ease access
        catAll(newCommits.reverse)
    }
}

object StatusFlags {

    // Named encoding of the flags to ease reading
    val NOT_IN_TRANSACTION = Bits(0)
    val IN_TRANSACTION = Bits(1)
    val PRISTINE = Bits(0)
    val DIRTY = Bits(1)

    // How many flags per core states
    val stateWidth = 2

    // Shift associated to each state
    val indexShift = log2Up(stateWidth)

    def apply(nrCores :Int, size :Int) = {
        Module(new StatusFlags(nrCores, size))
    }

    // The initial value of the status bits is not in transaction and pristine
    def makeInit(size :Int) = {
        var coreLocations = Fill(size, NOT_IN_TRANSACTION ## PRISTINE)
        coreLocations.setWidth(size * stateWidth)
        coreLocations
    }
}

/*
* A special scratchpad memory is created to perform the hardware support to
* transactional memory. This implementation requires no new instructions for
* the instruction set since all normal read and write commands to this spm
* have augmented transactional semantics. In particular, every write commits,
* and the first read after a write starts a new transaction.
*/
class TransSpm(
    // The granularity specifies the amount of memory assigned to a set of
    // status flags. For instances a set status flags for each core could cover
    // a byte, a word, a double word etc.
    val granularity :Int,
    nrCores: Int,           //number of cores
    size: Int               //size of each core memory
) extends Module {

    // memory coherence error checks.
    if (!isPow2 (granularity)) {
        sys.error (s"LLSCSpm: granularity must be a power of 2, but $granularity was provided.")
    }

    if (!isPow2 (size)) {
        sys.error (s"LLSCSpm: size must be a power of 2, but $size was provided.")
    }

    if (granularity > size) {
        sys.error (s"LLSCSpm: granularity has to be less than size, but granularity $granularity and size $size were provided.")
    }

    import StatusFlags._
    import TransSpm._

    // An OCP slave port plus the core ID that is operating
    val io = new Bundle() {
        val slave = new OcpCoreSlavePort(ADDR_WIDTH, DATA_WIDTH)
        val core = UInt(INPUT, log2Up(nrCores))
    }

    val addrBits = log2Up(size / BYTES_PER_WORD)

    // generate byte memories
    val mem = new Array[MemBlockIO](BYTES_PER_WORD)
    for (i <- 0 until BYTES_PER_WORD) {
        mem(i) = MemBlock(size / BYTES_PER_WORD, BYTE_WIDTH, bypass = false).io
    }

    // A StatusFlags module is created with the number of cores and the number of
    // memory addresses being watched by a set of flags.
    val statusFlags = StatusFlags(nrCores, size / granularity)

    // Wiring for reading
    // The address is shifted by the granularity since a set of status flags
    // covers multiple memory locations.
    statusFlags.io.addr := io.slave.M.Addr >> log2Up(granularity)
    statusFlags.io.core := io.core

    // Default wires set not to write
    statusFlags.io.wrEnable := Bool(false)
    statusFlags.io.data := Bits(0)

    /*
        Should only write if the write command is received and the current
        core can commit.
    */
    val shouldWrite = io.slave.M.Cmd === OcpCmd.WR && statusFlags.io.canCommit

    /* To connect with the Ocp slave */
    val cmdReg = Reg(next = io.slave.M.Cmd)
    val dataReg = RegInit(io.slave.M.Data)

    // store
    /*
        Setting the mask to 0 also when the transaction can not commit.
    */
    val stmsk = Mux(shouldWrite, io.slave.M.ByteEn, Bits(0))
    for (i <- 0 until BYTES_PER_WORD) {
        mem(i) <= (stmsk(i), io.slave.M.Addr(addrBits + 1, 2),
            io.slave.M.Data(BYTE_WIDTH*(i+1)-1, BYTE_WIDTH*i))
    }

    // Concatenates output from the memory blocks
    val rdData = mem.map(_(io.slave.M.Addr(addrBits + 1, 2))).reduceLeft((x,y) => y ## x)

    /* slave response
       mutilplexed by Ocp write || read
       if the command is write || read holds then DVA, else null
    */
    io.slave.S.Resp := Mux(cmdReg === OcpCmd.WR || cmdReg === OcpCmd.RD,
        OcpResp.DVA, OcpResp.NULL)

    /* slave data
       mutilplexed by Ocp read
       if the command is read holds then rdData, else dataReg
    */
    io.slave.S.Data := Mux(cmdReg === OcpCmd.RD, rdData, dataReg)

    switch (io.slave.M.Cmd) {
        //For a read command write enable is set to true and then the flags
        // are set to IN_TRANSACTION and PRISTINE, so that the status flags
        // are updated accordingly. No conditions need to be checked since a
        // read command is always possible
        is (OcpCmd.RD) {
            statusFlags.io.wrEnable := Bool(true)
            statusFlags.io.data := IN_TRANSACTION ## PRISTINE
        }

        // When a core attempts a write command we must first check if that
        // core can commit. If so, DIRTY flag is set in the input, otherwise
        // it is set to its negation. In both cases the transaction finishes,
        // so the *in transaction* flag is set to NOT_IN_TRANSACTION, and
        // the write enable is set since the status flags need to be written
        // anyway. The result is reported as either a success or a failure.
        is (OcpCmd.WR) {
            statusFlags.io.wrEnable := Bool(true)

            when (statusFlags.io.canCommit) {
                statusFlags.io.data := NOT_IN_TRANSACTION ## DIRTY
                dataReg := Result.SUCCESS
            }.otherwise {
                statusFlags.io.data := NOT_IN_TRANSACTION ## ~DIRTY
                dataReg := Result.FAIL
            }
        }
    }
}

object TransSpm {
    object Result {
        val SUCCESS = Bits(0)
        val FAIL = Bits(1)
    }

    def apply(granularity :Int, nrCores: Int, size: Int) = {
        Module(new TransSpm(granularity, nrCores, size))
    }
}

class TransSpmTester(dut: TransSpm) extends Tester(dut) {
  import TransSpmTester._

  println("Transactional SPM Tester")

  def read(addr: Int) = {
    println("---------------------------")
    poke(dut.io.slave.M.Addr, addr)
    poke(dut.io.slave.M.Cmd, 2) // OcpCmd.RD
    step(1)
    poke(dut.io.slave.M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io.slave.S.Resp) != 1) {
      step(1)
    }
    peek(dut.io.slave.S.Data)
    dut.io.slave.S.Data
  }

  def write(addr: Int, data: Int) = {
    println("---------------------------")
    poke(dut.io.slave.M.Addr, addr)
    poke(dut.io.slave.M.Data, data)
    poke(dut.io.slave.M.Cmd, 1) // OcpCmd.WR
    poke(dut.io.slave.M.ByteEn, 0x0f)
    step(1)
    poke(dut.io.slave.M.Cmd, 0) // OcpCmd.IDLE
    while (peek(dut.io.slave.S.Resp) != 1) {
      step(1)
    }
    peek(dut.io.slave.S.Data)
    dut.io.slave.S.Data
  }

  poke(dut.io.core, 0)

  // Basic read-write test
  for (i <- 0.until(1024, GRANULARITY)) {
    expect(write(i, i * 0x100 + 0xa), 0)
  }
  step(1)
  for (i <- 0.until(1024, GRANULARITY)) {
    expect(read(i), i * 0x100 + 0xa)
  }

  // Transactional tests

  // Simple failed commit on a single variable
  poke(dut.io.core, 0)
  read(0)

  poke(dut.io.core, 1)
  expect(write(0, 0xFF), 0)

  poke(dut.io.core, 0)
  expect(write(0, 0xFF), 1)

  // Failed commit on more than one variable
  poke(dut.io.core, 0)
  for (addr <- 0 until(256, GRANULARITY)) {
      read(addr)
  }

  poke(dut.io.core, 1)
  for (addr <- 0 until(256, GRANULARITY)) {
      expect(write(addr, 0xFF), 0)
  }

  poke(dut.io.core, 0)
  expect(write(128, 0xFF), 1)

  // Even writing one variable makes the whole transaction not commit
  poke(dut.io.core, 0)
  for (addr <- 0 until(256, GRANULARITY)) {
      read(addr)
  }

  poke(dut.io.core, 1)
  expect(write(128, 0xFF), 0)

  poke(dut.io.core, 0)
  expect(write(64, 0xFF), 1)

  // Reading a variable while in transaction after it has been written by
  // another core makes the commit fail

  poke(dut.io.core, 0)
  for (addr <- 0 until(64, GRANULARITY)) {
      read(addr)
  }

  poke(dut.io.core, 1)
  for (addr <- 64 until(256, GRANULARITY)) {
      expect(write(addr, 0xFF), 0)
  }

  poke(dut.io.core, 0)
  read(128)
  expect(write(0, 0xFF), 1)

  // A writes terminates the current transaction, regardless of its result

  // Commited transaction
  poke(dut.io.core, 0)
  for (addr <- 0 until(256, GRANULARITY)) {
      read(addr)
  }

  expect(write(128, 0xFF), 0)
  expect(write(512, 0xFF), 0)

  // Uncommited transaction
  poke(dut.io.core, 0)
  for (addr <- 0 until(256, GRANULARITY)) {
      read(addr)
  }

  poke(dut.io.core, 1)
  expect(write(128, 0xFF), 0)

  poke(dut.io.core, 0)
  expect(write(64, 0xFF), 1)
  expect(write(512, 0xFF), 0)
}

object TransSpmTester {
    val GRANULARITY = 32

  def main(args: Array[String]): Unit = {
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c",
      "--compile", "--vcd", "--targetDir", "generated"),
      () => TransSpm(GRANULARITY, 4, 1024)) {
        c => new TransSpmTester(c)
      }
  }
}
