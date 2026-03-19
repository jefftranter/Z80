#include <sys/types.h>
#include <stdlib.h>
#include "hdosutil.h"

#define TIKCNT 0x201B

/* Return clock ticks. Increases at the rate of CLOCKS_PER_SEC. Not
   guaranteed to be monotonic. */
clock_t clock()
{
    return wpeek(TIKCNT);
}
