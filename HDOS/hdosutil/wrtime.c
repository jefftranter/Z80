#include <stdio.h>
#include "hdosutil.h"

#define HDOS_TIME ((volatile unsigned char *)0x20CA)

// Convert integer (0-99) to packed BCD
static unsigned char int_to_bcd(unsigned char val)
{
    return ((val / 10) << 4) | (val % 10);
}

// Set HDOS time in format hh:mm:ss
void wrtime(char *time)
{
    unsigned int hh, mm, ss;

    if (sscanf(time, "%u:%u:%u", &hh, &mm, &ss) != 3) {
        printf("Error: time format must be hh:mm:ss\n");
        return;
    }

    if (hh > 23 || mm > 59 || ss > 59) {
        printf("Error: invalid time value\n");
        return;
    }

    HDOS_TIME[0] = int_to_bcd((unsigned char)hh);
    HDOS_TIME[1] = int_to_bcd((unsigned char)mm);
    HDOS_TIME[2] = int_to_bcd((unsigned char)ss);
}
