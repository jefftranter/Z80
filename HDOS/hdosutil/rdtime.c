#include <stdio.h>
#include "hdosutil.h"

#define HDOS_TIME ((volatile unsigned char *)0x20CA)

// Convert packed BCD byte to integer
static unsigned char bcd_to_int(unsigned char bcd)
{
    return ((bcd >> 4) * 10) + (bcd & 0x0F);
}

// Return HDOS time in format hh:mm:ss
void rdtime(char *time)
{
    unsigned char hh, mm, ss;

    hh = bcd_to_int(HDOS_TIME[0]);
    mm = bcd_to_int(HDOS_TIME[1]);
    ss = bcd_to_int(HDOS_TIME[2]);

    sprintf(time, "%02d:%02d:%02d", hh, mm, ss);
}
