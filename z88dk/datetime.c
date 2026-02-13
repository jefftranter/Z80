/*
 * HDOS Date/Time Display
 * For z88dk (Heath/Zenith HDOS target)
 *
 * Date: ASCII at 0x20BF-0x20C7 (9 characters)
 * Time: BCD at 0x20CA-0x20CC (HH, MM, SS packed BCD)
 */

#include <stdio.h>

/* Memory-mapped locations */
#define HDOS_DATE   ((volatile unsigned char *)0x20BF)
#define HDOS_TIME   ((volatile unsigned char *)0x20CA)

/* Convert packed BCD byte to integer */
static unsigned char bcd_to_int(unsigned char bcd)
{
    return ((bcd >> 4) * 10) + (bcd & 0x0F);
}

int main(void)
{
    char date[10];      /* 9 chars + null terminator */
    unsigned char hh, mm, ss;
    int i;

    /* Copy date string */
    for (i = 0; i < 9; i++) {
        date[i] = HDOS_DATE[i];
    }
    date[9] = '\0';

    /* Read BCD time */
    hh = bcd_to_int(HDOS_TIME[0]);
    mm = bcd_to_int(HDOS_TIME[1]);
    ss = bcd_to_int(HDOS_TIME[2]);

    /* Display */
    printf("Date: %s\n", date);
    printf("Time: %02u:%02u:%02u\n", hh, mm, ss);

    return 0;
}
