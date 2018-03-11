#!/usr/bin/env bash

# Wrapper script around patmos makefiles, since we always forget
# the exact command.
# Arguments:
#   -c: Flag. Disabled the build of the emulator.
#   $1: The action to be performed
#   $2: The program to be loaded

# This function burns the passed program to the default FPGA ROM.
# Arguments:
#   $1: The program to be burned to the ROM
function burn-rom {
    local PROGRAM="$1"

    make BOOTAPP=bootable-"$PROGRAM" gen synth config
}

COMPILE_ONLY='false'
while getopts 'c' OPTION; do
    case "$OPTION" in
        'c')
            COMPILE_ONLY='true'
            ;;
    esac
done
shift $(( OPTIND - 1 ))

CMD="$1"

make tools
case "${CMD,,}" in

    'bootloader'|'bl')
        burn-rom 'bootloader'
        ;;

    'burn-rom'|'br')
        burn-rom "$2"
        ;;

    'emulate'|'emu')
        PROGRAM="$2"
        EXEC=$(basename "$PROGRAM" .c)

        [[ "$COMPILE_ONLY" == 'false' ]] && make emulator
        patmos-clang "$PROGRAM" -o "$EXEC"
        patemu "$EXEC"
        ;;

    'program'|'app')
        PROGRAM="$2"

        make APP="$PROGRAM" comp config download
        ;;
esac
