/*
 * HDOS Date/Time Display and Set Utility
 * For z88dk (HDOS target)
 *
 * Date: ASCII at 0x20BF-0x20C7 (9 characters)
 * Time: BCD at 0x20CA-0x20CC (HH, MM, SS packed BCD)
 *
 * Usage:
 *   datetime             -> display date and time
 *   datetime -d DATE     -> set date (exactly 9 chars, e.g. "13-Feb-26")
 *   datetime -t HH:MM:SS -> set time (24-hour)
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

/* Memory-mapped locations */
#define HDOS_DATE   ((volatile unsigned char *)0x20BF)
#define HDOS_TIME   ((volatile unsigned char *)0x20CA)

/* Convert packed BCD byte to integer */
static unsigned char bcd_to_int(unsigned char bcd)
{
    return ((bcd >> 4) * 10) + (bcd & 0x0F);
}

/* Convert integer (0-99) to packed BCD */
static unsigned char int_to_bcd(unsigned char val)
{
    return ((val / 10) << 4) | (val % 10);
}

/* Display current date and time */
static void show_datetime(void)
{
    char date[10];
    unsigned char hh, mm, ss;
    int i;

    for (i = 0; i < 9; i++)
        date[i] = HDOS_DATE[i];
    date[9] = '\0';

    hh = bcd_to_int(HDOS_TIME[0]);
    mm = bcd_to_int(HDOS_TIME[1]);
    ss = bcd_to_int(HDOS_TIME[2]);

    printf("Date: %s\n", date);
    printf("Time: %02u:%02u:%02u\n", hh, mm, ss);
}

/* Set date (expects exactly 9 characters) */
static int set_date(const char *s)
{
    int i;

    if (strlen(s) != 9) {
        printf("Error: date must be exactly 9 characters.\n");
        return 1;
    }

    for (i = 0; i < 9; i++)
        HDOS_DATE[i] = s[i];

    return 0;
}

/* Parse HH:MM:SS and set time */
static int set_time(char *s)
{
    unsigned int hh, mm, ss;

    if (sscanf(s, "%u:%u:%u", &hh, &mm, &ss) != 3) {
        printf("Error: time format must be HH:MM:SS\n");
        return 1;
    }

    if (hh > 23 || mm > 59 || ss > 59) {
        printf("Error: invalid time value\n");
        return 1;
    }

    HDOS_TIME[0] = int_to_bcd((unsigned char)hh);
    HDOS_TIME[1] = int_to_bcd((unsigned char)mm);
    HDOS_TIME[2] = int_to_bcd((unsigned char)ss);

    return 0;
}

int main(int argc, char *argv[])
{
    int i;

    if (argc == 1) {
        show_datetime();
        return 0;
    }

    for (i = 1; i < argc; i++) {
        if (strcasecmp(argv[i], "-d") == 0) {
            if (i + 1 >= argc) {
                printf("Error: -d requires a date string\n");
                return 1;
            }
            if (set_date(argv[i + 1]) != 0)
                return 1;
            i++;
        }
        else if (strcasecmp(argv[i], "-t") == 0) {
            if (i + 1 >= argc) {
                printf("Error: -t requires HH:MM:SS\n");
                return 1;
            }
            if (set_time(argv[i + 1]) != 0)
                return 1;
            i++;
        }
        else {
            printf("Usage:\n");
            printf("datetime (show date/time)\n");
            printf("datetime -d DD-Mmm-YY\n");
            printf("datetime -t HH:MM:SS\n");
            return 1;
        }
    }

    return 0;
}
