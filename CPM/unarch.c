/*

  Simple file unarchiver. Extracts files created by arch.c. Runs on
  Linux, CP/M, and HDOS.

  TODO: Consider adding command line options:
  -w Allow overwriting existing files
  -l List archive contents only
  -t Handle as text files
  -b Handle as binary files (default)
  -a Attempt to automatically handle text and binary files (e.g. binary for .ABS .COM .SYS .OBJ .REL)
  -d Show additional debug output
  -v Show program version
*/

#include <stdio.h>
#include <string.h>

/* Maximum number of files supported */
#define MAX_FILES 16

int main(int argc, char **argv)
{
    int numfiles;
    int i, n;
    FILE *afp;
    FILE *fp;
    char buffer[256];
    char filename[MAX_FILES][12];
    int size[MAX_FILES];

    /* Check command line options. */
    if (argc !=2) {
        fprintf(stderr, "Usage: %s <archive file>\n", argv[0]);
        return 1;
    }

    /* Open archive file. */
    afp = fopen(argv[1], "rb");
    if (afp == NULL) {
        fprintf(stderr, "Error: open of %s failed\n", argv[1]);
        return 1;
    }

    /* Read and verify magic number. */
    fread(buffer, 1, 3, afp);
    if (buffer[0] != 'J' && buffer[1] != 'J' && buffer[2] != 'T') {
        fprintf(stderr, "Error: %s is not a valid archive file\n", argv[1]);
        return 1;
    }

    /* Read number of files in archive. */
    fread(buffer, 1, 1, afp);
    numfiles = buffer[0];
    if (numfiles > MAX_FILES) {
        fprintf(stderr, "Error: too many files in archive\n");
        return 1;
    }

    /* For each file: read filename and length  */
    for (i = 0; i < numfiles; i++) {
        fread(buffer, 1, 12, afp);
        strcpy(filename[i], buffer);
        fread(buffer, 1, 4, afp);
        size[i] = ((unsigned char)buffer[0] << 24)
            + ((unsigned char)buffer[1] << 16)
            + ((unsigned char)buffer[2] << 8)
            + (unsigned char)buffer[3];
    }

    /* for each file:
         open output file
         read raw data based on length
         write data to file
         close file
    */
    for (i = 0; i < numfiles; i++) {
        fp = fopen(filename[i], "wb");
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
    fread(buffer, 1, 3, afp);
    if (buffer[0] != 'E' && buffer[1] != 'N' && buffer[2] != 'D') {
        fprintf(stderr, "Warning: No end of archive file marker found\n");
    }

    /* Close archive file. */
    fclose(afp);

    return 0;
}
