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

#define INIT (-1)

#define LLSC_MEM (0xE8000000)

// Whatever this contant means, it is needed
const int NOC_MASTER = 0;

volatile _IODEV int* us_ptr = (volatile _IODEV int *) (PATMOS_IO_TIMER+12);
volatile _SPM int* sspm = (volatile _SPM int *) (LLSC_MEM);
volatile _SPM int* shared_addr = (volatile _SPM int *) (LLSC_MEM + 32);

void wait_period(unsigned int period) {
    int next = *us_ptr + period;
    while (*us_ptr - next < 0);
}

void choose(void* args) {
    unsigned int seed = 0x7FFF - ((unsigned int) get_cpu_usecs());
    unsigned int period = rand_r(&seed) & 0xFFFF;
    int my_value = (int) args;

    int value = *shared_addr;

    wait_period(period);

    if (value == INIT) {
        *shared_addr = my_value;
    }

    int ret = *shared_addr;
    corethread_exit((void*) ret);
}

int main() {
  int a = *shared_addr;
  int slaves_count = get_cpucnt() - 1;
  int k;
  int* choices = (int*) malloc(sizeof(int) * slaves_count);
  int broken = 0;

  *shared_addr = INIT;

  for (k = 0; k < slaves_count; ++k) {
    corethread_create(k + 1, &choose, (void*) k);
  }

  for (k = 0; k < slaves_count; ++k) {
    corethread_join(k + 1, (void**) &choices[k]);
  }

  for (k = 0; k < slaves_count - 1; ++k) {
      if (choices[k] != choices[k + 1]) {
          printf("Consensus not reached: found [");

          for (k = 0; k < slaves_count - 1; ++k) {
              printf("%d, ", choices[k])
          }
          printf("%d]\n", choices[slaves_count - 1]);

          broken = 1;
          break;
      }
  }

  if (!broken) {
      printf("Consensus reached on %d\n", choices[0]);
  }

  free(choices);
  return 0;
}
