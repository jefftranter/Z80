/*

  Simple file unarchiver. Extracts files created by arch.c. Runs on
  Linux, CP/M, and HDOS.

  To Do:
  - Add option -a, Attempt to automatically handle text and binary
    files based in file extension (e.g. binary for .ABS .COM .SYS .OBJ
    .REL).
*/

#include <stdio.h>
#include <string.h>
#ifdef linux
#include <unistd.h>
#endif
#ifdef CPM
#include <stdlib.h>
#endif

/* Maximum number of files supported */
#ifdef CPM
#define MAX_FILES 16
#else
#define MAX_FILES 100
#endif

int main(int argc, char **argv)
{
    int numfiles;
    int i, n;
    FILE *afp;
    FILE *fp;
    char buffer[256];
    char filename[MAX_FILES][13];
    int size[MAX_FILES];
    int opt;
    int opt_w = 0;
    int opt_l = 0;
    int opt_t = 0;
    int opt_d =0;
    int opt_v = 0;

    /* Parse command line options. Will always be uppercase on
       CP/M. */
    while ((opt = getopt(argc, argv, "wltdvWLTDV")) != -1) {
        switch (opt) {
        case 'w':
        case 'W':
            opt_w = 1;
            break;
        case 'l':
        case 'L':
            opt_l = 1;
            break;
        case 't':
        case 'T':
            opt_t = 1;
            break;
        case 'd':
        case 'D':
            opt_d = 1;
            break;
        case 'v':
        case 'V':
            opt_v = 1;
            break;
        default: /* '?' */
            fprintf(stderr, "Usage: unarch [-w][-l][-t][-d][-v] <archive file>\n"
                    "  -w Allow overwriting existing files\n"
                    "  -l List archive contents only\n"
                    "  -t Handle as text files (default is binary)\n"
                    "  -d Show additional debug output\n"
                    "  -v Show program version\n");
            return 1;
        }
    }

    /* Handle -v option (show version and exit). */
    if (opt_v) {
        fprintf(stderr, "unarch version 1.0.0\n");
        return 1;
    }

    /* Check for filename argument. */
    if (optind != argc - 1) {
        fprintf(stderr, "Error: must specify one archive file argument\n");
        return 1;
    }

    /* Open archive file. */
    if (opt_d)
        fprintf(stderr, "Debug: opening archive file %s\n", argv[optind]);
    afp = fopen(argv[optind], "rb");
    if (afp == NULL) {
        fprintf(stderr, "Error: unable to open %s\n", argv[optind]);
        return 1;
    }

    /* Read and verify magic number. */
    if (opt_d)
        fprintf(stderr, "Debug: checking magic number\n");
    fread(buffer, 1, 3, afp);
    if (buffer[0] != 'J' && buffer[1] != 'J' && buffer[2] != 'T') {
        fprintf(stderr, "Error: %s is not a valid archive file\n", argv[optind]);
        return 1;
    }

    /* Read number of files in archive. */
    fread(buffer, 1, 1, afp);
    numfiles = buffer[0];
    if (numfiles > MAX_FILES) {
        fprintf(stderr, "Error: too many files in archive\n");
        return 1;
    }
    if (opt_d)
        fprintf(stderr, "Debug: number of files in archive is %d\n", numfiles);

    /* For each file: read filename and length  */
    for (i = 0; i < numfiles; i++) {
        fread(buffer, 1, 12, afp);
        strcpy(filename[i], buffer);
        fread(buffer, 1, 4, afp);
        filename[i][12] = 0;
        size[i] = ((unsigned char)buffer[0] << 24)
            + ((unsigned char)buffer[1] << 16)
            + ((unsigned char)buffer[2] << 8)
            + (unsigned char)buffer[3];
        if (opt_d)
            fprintf(stderr, "Debug: archive contains %s with size %d bytes\n", filename[i], size[i]);
    }

    /* for each file:
         open output file
         read raw data based on length
         write data to file
         close file
    */
    for (i = 0; i < numfiles; i++) {

        /* With -l option, just list the files in the archive. */
        if (opt_l) {
            fprintf(stderr, "%s\n", filename[i]);
            continue;
        }

        /* Unless -w option is set, make sure file does not already exist. */
        if (!opt_w) {
            fp = fopen(filename[i], "r");
            if (fp != NULL) {
                fprintf(stderr, "Error: file %s already exists\n", filename[i]);
                fprintf(stderr, "Error: Remove file or use -w option to overwrite\n");
                fclose (fp);
                return 1;
            }
        }

        if (opt_t) {
            if (opt_d)
                fprintf(stderr, "Debug: opening %s in text mode\n", filename[i]);
            fp = fopen(filename[i], "w");
        } else {
            if (opt_d)
                fprintf(stderr, "Debug: opening %s in binary mode\n", filename[i]);
            fp = fopen(filename[i], "wb");
        }
        if (fp == NULL) {
            fprintf(stderr, "Error: unable to create %s\n", filename[i]);
            return 1;
        }

        fprintf(stderr, "Extracting %s (%d bytes)\n", filename[i], size[i]);

        n = size[i];
        do {
            if (n > 256) {
                fread(buffer, 1, 256, afp);
                fwrite(buffer, 1, 256, fp);
                n = n - 256;
            } else {
                fread(buffer, 1, n, afp);
                fwrite(buffer, 1, n, fp);
                n = 0;
            }

        } while (n > 0);

        fclose(fp);
    }

    /* Verify end of archive marker */
    if (!opt_l) {
        if (opt_d)
            fprintf(stderr, "Debug: checking for end of archive marker\n");
        fread(buffer, 1, 3, afp);
        if (buffer[0] != 'E' && buffer[1] != 'N' && buffer[2] != 'D') {
            fprintf(stderr, "Warning: No end of archive file marker found\n");
        }
    }

    if (opt_d)
        fprintf(stderr, "Debug: closing file\n");

    /* Close archive file. */
    fclose(afp);

    return 0;
}
