#include <stdint.h>
#include "hdosutil.h"

#define HDOS_DATE ((volatile unsigned char *)0x20BF)

// Return HDOS date in format dd-Mmm-yy
void rddate(char *date)
{
    int i;

    for (i = 0; i < 9; i++)
        date[i] = HDOS_DATE[i];
    date[9] = '\0';
}
