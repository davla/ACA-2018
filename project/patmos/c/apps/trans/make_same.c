/*
    Small test program for the shared SPM

    Author: Martin Schoeberl
*/

#include <stdio.h>
#include <stdlib.h>
#include <machine/patmos.h>
#include <machine/spm.h>

#include "libcorethread/corethread.h"

#define INIT (-1)

#define TRANS_SPM (0xE8000000)

// Whatever this contant means, it is needed
const int NOC_MASTER = 0;

volatile _IODEV int* us_ptr = (volatile _IODEV int *) (PATMOS_IO_TIMER+12);
volatile _SPM int* sspm = (volatile _SPM int *) (TRANS_SPM);
volatile _SPM int* addr1 = (volatile _SPM int *) (TRANS_SPM + 32);
volatile _SPM int* addr2 = (volatile _SPM int *) (TRANS_SPM + 128);
volatile _SPM int* addr3 = (volatile _SPM int *) (TRANS_SPM + 256);

void wait_period(unsigned int period) {
    int next = *us_ptr + period;
    while (*us_ptr - next < 0);
}

void make_same(void* args) {
    unsigned int seed = 0x7FFF - ((unsigned int) get_cpu_usecs());
    unsigned int period = rand_r(&seed) & 0xFFFF;
    int my_value = (int) args;

    *addr3 = 55;

    int value1 = *addr1;
    int value2 = *addr2;

    wait_period(period);

    if (value1 == my_value) {
        *addr2 = value1;
    }
    else if (value2 == my_value) {
        *addr1 = value2;
    }

    int ret = 0;
    corethread_exit((void*) ret);
}

int main() {
    int value1 = 1;
    int value2 = 2;

    int res;

    *addr1 = value1;
    *addr2 = value2;

    corethread_create(1, &make_same, (void*) value1);
    corethread_create(2, &make_same, (void*) value2);

    corethread_join(1, (void**) &res);
    corethread_join(2, (void**) &res);

    printf("%d = %d\n", *addr1, *addr2);

    return 0;
}
