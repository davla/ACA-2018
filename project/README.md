# LL/SC + Transactional memory project

## Files

The hardware files are located in `patmos/hardware/src/main/scala/conc/`
The test C programs are in `patmos/c/apps/llsc/` and `patmos/c/apps/trans`

## Utilities

Patmos makefile system has been wrapped into `cmd.sh` to ease usage. The
script can be invoked these ways:

* `bash cmd.sh bootloader` -- Burns the bootloader into the default FPGA ROM.
* `bash cmd.sh burn-rom <PROGRAM>` -- Burns PROGRAM into the default FPGA ROM.
* `bash cmd.sh program <PROGRAM>` -- Compiles and writes PROGRAM to the
    default FPGA memory.
* `bash cmd.sh [-c] emulate <PROGRAM>` -- Builds the emulator, compiles and
    emulates PROGRAM. If the -c flag is given, the emulator is not built.
