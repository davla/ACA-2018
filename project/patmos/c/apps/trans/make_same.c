/*
    Test program for the Transactional SPM.

    Two threads try to set two shared locations to the same value. It is not
    important which value is set, as long as they are the same.

    Authors: Davide Laezza - Roberts Fanning - Wenhao Li
*/

#include <stdio.h>
#include <stdlib.h>
#include <machine/patmos.h>
#include <machine/spm.h>

#include "libcorethread/corethread.h"

// Whatever this contant means, it is needed
const int NOC_MASTER = 0;

// Timer pointer for random delay
volatile _IODEV int* us_ptr = (volatile _IODEV int *) (PATMOS_IO_TIMER+12);

/*
    Shared scratchpad memory address, whatever the actual type.
    Needs to be a macro otherwise the compiler complains about addresses not
    being constant at compile-time.
*/
#define SHARED_SPM (0xE8000000)
volatile _SPM int* sspm = (volatile _SPM int *) (SHARED_SPM);
volatile _SPM int* addr1 = (volatile _SPM int *) (SHARED_SPM + 32);
volatile _SPM int* addr2 = (volatile _SPM int *) (SHARED_SPM + 128);

// This function busy waits for the specified period, in microseconds
void wait_period(unsigned int period) {
    int next = *us_ptr + period;
    while (*us_ptr - next < 0);
}

/*
    This method reads two shared locations, and then sets one to the value of
    the other according to the values read and the arguments that it's given.
    The transactional memory is necessary to watch an address while the other
    is written, so that the write is not carried out if an update occurred.
*/
void make_same(void* args) {

    // Random period creation
    unsigned int seed = 0x7FFF - ((unsigned int) get_cpu_usecs());
    unsigned int period = rand_r(&seed) & 0xFFFF;

    // The argument based on which the value to set is chosen
    int my_value = (int) args;

    // Reading the two values
    int value1 = *addr1;
    int value2 = *addr2;

    // This increases the chances of a lost update occurring
    wait_period(period);

    if (value1 != value2) {
        // If the argument is equal to the first value, setting the second
        if (value1 == my_value) {
            *addr2 = value1;
        }
        // If the argument is equal to the second value, setting the first
        else if (value2 == my_value) {
            *addr1 = value2;
        }
    }

    // Returning success
    int ret = 0;
    corethread_exit((void*) ret);
}

/*
    The main function. Writes the initial values to the shared addresses, and
    spawns two threads with the correct arguments. Then joins the thread and
    checks whether the two shared addresses contain the same value.
*/
int main() {

    // The two values to be set
    int value1 = 1;
    int value2 = 2;

    // Used when joining the threads
    int res;

    // Writing the values to the shared addresses
    *addr1 = value1;
    *addr2 = value2;

    // This thread will set *addr2 to value1
    corethread_create(1, &make_same, (void*) value1);
    // This thread will set *addr1 to value2
    corethread_create(2, &make_same, (void*) value2);

    //Joining the threads
    corethread_join(1, (void**) &res);
    corethread_join(2, (void**) &res);

    // Reading the vlues for checking
    value1 = *addr1;
    value2 = *addr2;

    // Checking for euqality of tead values
    if (value1 == value2) {
        printf("Success! Found: ");
    }
    else {
        printf("Error! Found: ");
    }
    printf("%d = %d\n", value1, value2);

    return 0;
}
