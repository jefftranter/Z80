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

#include <hdos.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>

/* Display current date and time */
static void show_datetime(void)
{
    char s[10];

    rddate(s);
    printf("Date: %s\n", s);
    rdtime(s);
    printf("Time: %s\n", s);
}

/* Set date (expects exactly 9 characters) */
static int set_date(char *s)
{
    int i;

    if (strlen(s) != 9) {
        printf("Error: date must be exactly 9 characters.\n");
        return 1;
    }

    /* Make sure month is properly capitalized (command line options
       get converted to upper case by HDOS). */
    s[3] = toupper(s[3]);
    s[4] = tolower(s[4]);
    s[5] = tolower(s[5]);

    wrdate(s);
    return 0;
}

/* Set time */
static int set_time(char *s)
{
    wrtime(s);
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
