#!/usr/bin/env bash

# Wrapper script around patmos makefiles, since we always forget
# the exact command.
# Arguments:
#   $1: The action to be performed
#   $2: The program to be loaded

# This function burns the passed program to the default FPGA ROM.
# Arguments:
#   $1: The program to be burned to the ROM
function burn-rom {
    local PROGRAM="$1"

    make BOOTAPP=bootable-"$PROGRAM" gen synth config
}

CMD="$1"

make tools
case "${CMD,,}" in

    # Burns the bootloader to the ROM
    'bootloader'|'bl')
        burn-rom 'bootloader'
        ;;

    # Burns a program to the ROM
    'burn-rom'|'br')
        burn-rom "$2"
        ;;

    # Compiles a program and writes in into the FPGA memory
    'program'|'app')
        PROGRAM="$2"
        make APP="$PROGRAM" comp config download
        ;;
esac
