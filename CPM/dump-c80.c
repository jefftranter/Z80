/*
  To build on HDOS using Software Toolworks C/80 compiler:
  c80 -d1000 sy4:dump=sy4:dump
  a80 sy4:dump=sy4:dump
*/

#include "stdio.h"

#define BYTES_PER_LINE 16

/* Function to print a byte in hexadecimal format */
int print_hex(byte)
    char byte;
{
    printf("%02x ", byte & 0xff);
}

/* Function to print a byte in ASCII format, or a dot if not printable */
int print_ascii(byte)
    char byte;
{
    if (isprint(byte)) {
        printf("%c", byte);
    } else {
        printf(".");
    }
}

/*
   This function is not available under HDOS or CP/M so is
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

int dump_file(filename)
    char *filename;
{
    FILE *file;
    char buffer[BYTES_PER_LINE];
    int bytes_read;
    int offset;
    int i;

    offset = 0;

    file = fopen(filename, "rb"); /* Open the file in binary mode */
    if (file == NULL) {
        printf("Failed to open file: %s", filename);
        return;
    }

    /* Read and process the file in chunks of 16 bytes */
    while ((bytes_read = fread(buffer, 1, BYTES_PER_LINE, file)) > 0) {
        /* Print the offset (address) in the first column */
        printf("%04x  ", offset);

        /* Print the hexadecimal representation */
        for (i = 0; i < bytes_read; i++) {
            print_hex(buffer[i]);
        }

        /* Fill in the rest of the hex section with spaces if less than 16 bytes */
        for (i = bytes_read; i < BYTES_PER_LINE; i++) {
            printf("   ");
        }

        /* Print the ASCII representation */
        printf(" |");
        for (i = 0; i < bytes_read; i++) {
            print_ascii(buffer[i]);
        }

        printf("|\n");

        offset += bytes_read;
    }

    fclose(file);
}

int main(argc, argv)
    int argc; char **argv;
{
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    dump_file(argv[1]);
    return 0;
}

#include "printf.c"
#include "stdlib.c"
#include "adlib.c"
