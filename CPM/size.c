/*
 * Simple size program - display text file size in lines, words, characters.
 *
 * Intended to be compiled using Hi-Tech C for CP/M. Also compiles
 * with any standard C compiler (e.g. gcc on Linux). Note that Hi-Tech
 * C on CP/M has a nice feature of expanding wildcards in filenames so
 * you can do, for example, size *.txt
 *
 * Jeff Tranter <tranter@pobox.com>
 */

#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[])
{
    int lines;
    int words;
    int chars;
    int inWord;
    int i;
    FILE *f;
    char *filename;
    char c;

    if (argc < 2) {
        printf("usage: size <filename(s)>\n");
        return 1;
    }

    for (i = 1; i < argc; i++) {
        filename = argv[i];
        f = fopen(argv[i], "r");
        if (f == NULL) {
            printf("error: unable to open '%s'\n", filename);
            return 1;
        }

        lines = 0;
        words = 0;
        chars = 0;
        inWord = 0;

        while (!feof(f)) {

            c = getc(f);

            if (c == EOF) {
                break;
            }

            chars++;

            if (c == '\n') {
                lines++;
                if (inWord) {
                    words++;
                    inWord = 0;
                }
            } else if (c == ' ' || c == '\t') {
                if (inWord) {
                    words++;
                    inWord = 0;
                }
            } else {
                inWord = 1;
            }
        }

        printf("%12s lines: %d words: %d chars: %d\n", filename, lines, words, chars);
        fclose(f);
    }

    return 0;
}
