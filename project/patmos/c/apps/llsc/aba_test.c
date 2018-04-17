/*
    Small test program for the shared SPM

    Author: Martin Schoeberl
*/

#include <stdio.h>
#include <stdlib.h>
#include <machine/patmos.h>
#include <machine/spm.h>

#include "libcorethread/corethread.h"

#define CNT 4
#define SHARED_SPM *((volatile _SPM int *) 0xE8000000)

#define A_VAL (0x19)
#define B_VAL (0x84)

#define VICTIM_WRITTEN (0xDEADBEEF)
#define ATTACKER_DONE (0xDEADDEAD)

// Whatever this contant means, it is needed
const int NOC_MASTER = 0;

volatile _IODEV int* timer_ptr = (volatile _IODEV int *) (PATMOS_IO_TIMER+4);
volatile _SPM int* sspm = (volatile _SPM int *) (0xE8000000);
volatile _SPM int* shared_addr = (volatile _SPM int *) (0xE8000000 + 32);
volatile _SPM int* token_addr = (volatile _SPM int *) (0xE8000000 + 64);

void victim(void* args) {
    int a = *shared_addr;   // So that A_VAL can be written
    *shared_addr = A_VAL;   // Writng A_VAL to shared variable
    a = *shared_addr;    // Start watching this address

    a = *token_addr;
    *token_addr = VICTIM_WRITTEN;

    // The attacker is doing its ABA stuff
    while (*token_addr != ATTACKER_DONE);

    *shared_addr = 0xFF;
    if (*shared_addr != A_VAL) {
        printf("Test failed\n");
    }
    else {
        printf("test successful\n");
    }

}

void attack(void* args) {
    while (*token_addr != VICTIM_WRITTEN);

    int a = *shared_addr;
    *shared_addr = B_VAL;
    a = *shared_addr;
    *shared_addr = A_VAL;

    a = *token_addr;
    *token_addr = ATTACKER_DONE;
}

int main() {
  int a = *token_addr;
  *token_addr = VICTIM_WRITTEN - 1;

  corethread_create(1, &attack, NULL);
  victim(NULL);
  return 0;
}
