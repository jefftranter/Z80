/* Dump HDOS directory contents.

  The directory is contained in the file DIRECT.SYS on each disk drive.
  Each entry is 23 bytes long.
  After each 22 entries, there is padding of 6 unused bytes.
  If the first character of the filename is 0xff or 0xfe then the file
  has been deleted or the entire entry following is empty if followed
  by zeroes.

  Entry format:

  Bytes Description
   8    Filename
   3    File type/extension
   1    Creation time (BCD hours)
   1    Creation time (BCD minutes)
   1    Number of accesses
   1    Flags (see below)
   1    User area mask
   1    First group number
   1    Last group number
   1    Last sector index
   2    Creation date
   2    Last access date

  Flags:
  0x80  S System file
  0x40  L Locked from flag changes
  0x20  W Write protected
  0x10  C Contiguous file
  0x08  A File archive attribute
  0x04  B File is damaged
  0x02  D Locked against delete
  0x01  U User-defined

  Dates:
  The encoding structure divides the 16-bit word into three components:
  Bits 15–9: Year (0-127, typically treated as 00-99).
  Bits 8–5: Month (1-12).
  Bits 4–0: Day (1-31). 

*/

#include <ctype.h>
#include <stdio.h>
#include <string.h>

const char months[12][4] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

/* Dump a directory entry */
void dump_entry(const char *entry) {
    char tmp[16];
    int i, w, y, m, d;

    /* Check for blank entry */
    if ((entry[0] == -1 || entry[0] == -2) && entry[1] == 0 && entry[2] == 0) {
        return;
    }

    /* Convert -1 or -2 to '?' and non-printable chars to spaces in filename */
    for (i = 0; i < 8; i++) {
        if (isprint(entry[i])) {
            tmp[i] = entry[i];
        } else if (entry[i] == -1 || entry[i] == -2) {
            tmp[i] = '?';
        } else {
            tmp[i] = ' ';
        }
    }
    tmp[8] = 0;
    printf("%8s ", tmp);

    /* Print extension. Convert non-printable chars to spaces. */
    for (i = 0; i < 3; i++) {
        if (isprint(entry[8+i])) {
            tmp[i] = entry[8+i];
        } else {
            tmp[i] = ' ';
        }
    }
    tmp[3] = 0;
    printf("%3s ", tmp);

    /* Creation time */
    printf("%02x:%02x ", entry[11], entry[12]);

    /* Number of accesses */
    printf("   %3d ", entry[13] & 0xff);

    /* Flags */
    strcpy(tmp, "");
    if (entry[14] & 0x80)
        strcat(tmp, "S");
    if (entry[14] & 0x40)
        strcat(tmp, "L");
    if (entry[14] & 0x20)
        strcat(tmp, "W");
    if (entry[14] & 0x10)
        strcat(tmp, "C");
    if (entry[14] & 0x08)
        strcat(tmp, "A");
    if (entry[14] & 0x04)
        strcat(tmp, "B");
    if (entry[14] & 0x02)
        strcat(tmp, "D");
    if (entry[14] & 0x01)
        strcat(tmp, "U");
    printf("%-8s ", tmp);

    /* User area mask */
    printf(" %3d ", entry[15] & 0xff);

    /* First group number */
    printf("   %3d ", entry[16] & 0xff);

    /* Last group number */
    printf("   %3d ", entry[17] & 0xff);

    /* Last sector index */
    printf("  %3d ", entry[18] & 0xff);

    /* Creation date:
       1111 1100 0000 0000
       5432 1098 7654 3210
       YYYY YYYM MMMD DDDD
    */
    w = (entry[19] & 0xff) + 256 * (entry[20] & 0xff);
    y = (w & 0b1111111000000000) >> 9;
    m = (w & 0b0000000111100000) >> 5;
    d = w & 0b0000000000011111;

    if (w == 0) {
        printf("<No-Date> ");
    } else {
        printf("%02d-%3s-%02d ", d, months[m-1], y);
    }

    /* Last access date */
    w = (entry[21] & 0xff) + 256 * (entry[22] & 0xff);
    y = (w & 0b1111111000000000) >> 9;
    m = (w & 0b0000000111100000) >> 5;
    d = w & 0b0000000000011111;

    if (w == 0) {
        printf("<No-Date> ");
    } else {
        printf("%02d-%3s-%02d ", d, months[m-1], y);
    }

    printf("\n");
}

/* Read a DIRECT.SYS file and dump each directory entry */
void dump_file(const char *filename) {
    char buffer[23];
    size_t bytes_read;
    int reads = 0;

    FILE *file = fopen(filename, "rb"); // Open the file in binary mode
    if (file == NULL) {
        char buf[30];
        sprintf(buf, "Failed to open '%s'", filename);
        perror(buf);
        return;
    }
    printf("Name     Ext Creat Access Flags    User FGroup LGroup LSect Creation  Access\n");
    printf("-------- --- ----- ------ -------- ---- ------ ------ ----- --------- --------\n");

    while ((bytes_read = fread(buffer, 1, 23, file)) > 0) {
        dump_entry(buffer);
        reads++;
        if (reads % 22 == 0) {
            /* Read 6 padding chars */
            fread(buffer, 1, 6, file);
        }
    }

    fclose(file);
}

int main(int argc, char *argv[]) {
    char direct[16];

    /* argc is not yet working on z88dk for HDOS */
    argc = 1;

    if (argc == 1) {
#ifdef __linux__
        strcpy(direct, "DIRECT.SYS");
#else
        strcpy(direct, "SY0:DIRECT.SYS");
#endif
        dump_file(direct);
    } else if (argc == 2) {
#ifdef __linux__
        strcpy(direct, argv[1]);
#else
        strcat(direct, ":DIRECT.SYS");
#endif
        dump_file(direct);
    } else {
#ifdef __linux__
        fprintf(stderr, "Usage: %s <direct.sys>\n", argv[0]);
#else
        fprintf(stderr, "Usage: DIR.ABS <drive>\n");
#endif
        return 1;
    }

    return 0;
}
