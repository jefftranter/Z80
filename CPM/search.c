/*
 * Simple search program - display lines in file(s) matching a string.
 *
 * Intended to be compiled using Hi-Tech C for CP/M. Also compiles
 * with any standard C compiler (e.g. gcc on Linux). Note that Hi-Tech
 * C on CP/M has a nice feature of expanding wildcards in filenames so
 * you can do, for example, search foo *.txt
 *
 * Jeff Tranter <tranter@pobox.com>
 */

#include <ctype.h>
#include <stdio.h>
#include <string.h>

char *strtoupper(char *s)
{
    char *p = s;

    while (*p) {
        *p = toupper(*p);
        p++;
    }
    return s;
}

int main(int argc, char *argv[])
{
    int i;
    int line;
    FILE *f;
    char *filename;
    char buff[132];
    char s[132];

    if (argc < 3) {
        printf("usage: search <pattern> <filename(s)>\n");
        return 1;
    }

    for (i = 2; i < argc; i++) {
        filename = argv[i];
        f = fopen(argv[i], "r");
        if (f == NULL) {
            printf("error: unable to open '%s'\n", filename);
            return 1;
        }

        line = 0;
        while (!feof(f)) {
            buff[0] = '\0';
            line++;
            fgets(buff, sizeof(buff)-1, f);

/* CP/M converts command line arguments to all upper case so we need
   to do a case insensitive compare. */
            strcpy(s, buff);
            if (strstr(strtoupper(s), strtoupper(argv[1])) != NULL) {
                printf("%s %d: %s", filename, line, buff);
            }
        }

        fclose(f);
    }

    return 0;
}
