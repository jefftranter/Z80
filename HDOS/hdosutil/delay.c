#include <arch/z80.h>
#include <stdlib.h>
#include "hdosutil.h"

#define TIKCNT 0x201B

// Disable/enable interrupts
#define di() __asm__("di")
#define ei() __asm__("ei")

// Delay for specified milliseconds. Uses 2 ms clock interrupt.
void delay(unsigned int ms)
{
    unsigned int t;

    ms >>= 1; // Divide by 2 to get interrupts
    t = wpeek(TIKCNT) + ms + 1;
    di();
    wpoke(TIKCNT, 0);
    ei();
    while (ms > wpeek(TIKCNT))
        ;
    di();
    wpoke(TIKCNT, t);
    ei();
}
