#include <stdio.h>
#include "hdosutil.h"

// Beep the H-89/H-19 speaker for ms milliseconds.
void beep(unsigned int ms)
{
    while (ms--) {
        putchar('\a');
    }
}
