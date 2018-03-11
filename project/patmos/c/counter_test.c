#include <machine/exceptions.h>
#include <machine/patmos.h>
#include <stdio.h>
#include <stdlib.h>

volatile _IODEV int* us_ptr = (volatile _IODEV int*) (PATMOS_IO_TIMER + 12);
volatile _IODEV int* io_ptr = (volatile _IODEV int*) 0xf00b0000;

void wait(int time) {
    int next = *us_ptr + time;
    while (*us_ptr != next);
    return;
}

int main() {
    int k, val;
    
    for (k = 0; k < 10000; ++k) {
        val = *io_ptr;
        printf("%d\n", val);
        wait(1000);
    }
}
