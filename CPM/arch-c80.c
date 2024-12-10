/*

  Simple file archiver. Combines multiple files into a single file to
  simplify uploads, etc. Runs on Linux, CP/M, and HDOS. A
  corresponding program can extract them. This version compiles under
  HDOS or CP/M using the Software Toolworks C/80 compiler.

  To build on HDOS:
  c80 -d1000 sy4:arch=sy4:arch
  a80 sy4:arch=sy4:arch

  To build on CP/M:
  c80 arch=arch
  a80 arch=arch

  TODO: Should use long type to handle larger files sizes, but not
  supported by standard C/80 compiler.

*/

/* Define one of the two symbols below depending on the platform to
   build for. */
#define CPM
/*#define HDOS*/

#include "stdio.h"

/*
  File format:

  3 byte magic number ("JJT")
  # of files in archive (unsigned byte)
  directory: for each file:
    filename (12 chars, padded with nulls)
    length of file in bytes (4 byte unsigned long integer)
  data for each file:
    data for file 1 (as per file length)
    data for file 2 (as per file length)
    data for file 3 (as per file length)
    ...
  3 byte end of archive file marker ("END")

*/

/*
   These two functions are not available under HDOS or CP/M so are
   implemented here.
*/
int fread(ptr, size, nmemb, fp)
    char *ptr; int size; int nmemb; FILE *fp;
{
    int i, c;

    for (i = 0; i < size * nmemb; i++) {
        c = getc(fp);
        if (c == -1)
            break;
        ptr[i] = c;
    }

    return i;
}

int fwrite(ptr, size, nmemb, fp)
    char *ptr; int size; int nmemb; FILE *fp;
{
    int i;

    for (i = 0; i < size * nmemb; i++) {
        putc(ptr[i], fp);
    }

    return i;
}

int main(argc, argv)
    int argc; char **argv;
{
    int rc;
    int numfiles;
    int i, j, n, c;
    int size;
    FILE *afp;
    FILE *fp;
    char buffer[128];
    char fcb[36];

    /* Expand any wildcards on command line. */
    command(&argc,&argv);

    /* Check command line options */
    if (argc < 3) {
        fprintf(stderr, "Usage: %s <archive file> <filename>...\n", argv[0]);
        return 1;
    }
    numfiles = argc - 2;

    /* Make sure that output file does not already exist, otherwise
       refuse to continue. We could use stat() for this, but it is not
       available under CP/M or HDOS so we try to open it for read instead. */
    afp = fopen(argv[1], "rb");
    if (afp != NULL) {
        fclose(afp);
        fprintf(stderr,
            "Error: output file %s already exists.\n",
             argv[1]);
        return 1;
    }

    /* Open output file. */
    afp = fopen(argv[1], "wb");

    if (afp == NULL) {
        fprintf(stderr, "Error: fopen of %s failed\n", argv[1]);
        return 1;
    }

    /* Write out magic number. */
    fprintf(afp, "JJT");

    /* Write out number of files in archive (unsigned byte). */
    fprintf(afp, "%c", numfiles);

    /* Write file directory */
    for (i = 2; i < numfiles + 2; i++) {

#ifdef CPM
        /* Get file size using CP/M BDOS call. */
        makfcb(argv[i], fcb); /* Make an FCB for the file */
        bdos(35, fcb); /* Call BDOS function 35 (F_SIZE) */

        /* Returns size in 128 byte records as a 16-bit value in
           fcb[33] (low) and fcb[34] (high). */
        size = (fcb[33] + 256 * fcb[34]) * 128;
#endif

#ifdef HDOS
        /* For HDOS: See p.582 of HDOS manual for method of getting
           file size. Have not been able to get this to work, so we
           read the file instead. */
        fp = fopen(argv[i], "rb");
        if (fp == 0 ) {
            fprintf(stderr, "unable to open %s\n", argv[i]);
            return 1;
        }

        size = 0;
        do {
            c = getc(fp);
            if (c == -1)
                break;
            size++;
        } while (TRUE);

        fclose(fp);
#endif

        /* Write filename, padded with zeroes to 12 characters. */
        for (j = 0; j < 12; j++) {
            fprintf(afp, "%c", (j < strlen(argv[i]) ? argv[i][j] : 0));
        }

        /* Write size as four byte integer. */
        fprintf(afp, "%c%c%c%c",
                (char)((size >> 24) & 0xff),
                (char)((size >> 16) & 0xff),
                (char)((size >> 8) & 0xff),
                (char)((size) & 0xff)
                );
    }

    /* Now write raw contents of each file. */
    for (i = 2; i < numfiles + 2; i++) {

        fp = fopen(argv[i], "rb");

        if (fp == NULL) {
            fprintf(stderr, "Error: open of %s failed\n", argv[i]);
            return 1;
        }

        fprintf(stderr, "Adding file %s\n", argv[i]);

        do {
            n = fread(buffer, 1, 128, fp);
            fwrite(buffer, 1, n, afp);
        } while (n > 0);

        fclose(fp);
    }

    /* Write end of archive file marker */
    fprintf(afp, "%s", "END");

    fclose (afp);

    return 0;
}

#include "printf.c"
#include "stdlib.c"
#include "command.c"
