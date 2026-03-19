#include <stdio.h>
#include <string.h>
#include "hdosutil.h"

#define HDOS_DATE ((volatile unsigned char *)0x20BF)

// Set HDOS date in format dd-Mmm-yy
void wrdate(char *date)
{
    int i;

    if (strlen(date) != 9) {
        printf("Error: date must be exactly 9 characters.\n");
        return;
    }

    for (i = 0; i < 9; i++)
        HDOS_DATE[i] = date[i];
}
