#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#define BYTES_PER_LINE 16

// Function to print a byte in hexadecimal format
void print_hex(unsigned char byte) {
    printf("%02x ", byte);
}

// Function to print a byte in ASCII format, or a dot if not printable
void print_ascii(unsigned char byte) {
    if (isprint(byte)) {
        printf("%c", byte);
    } else {
        printf(".");
    }
}

void dump_file(const char *filename) {
    FILE *file = fopen(filename, "rb"); // Open the file in binary mode
    if (file == NULL) {
        perror("Failed to open file");
        return;
    }

    unsigned char buffer[BYTES_PER_LINE];
    size_t bytes_read;
    size_t offset = 0;

    // Read and process the file in chunks of 16 bytes
    while ((bytes_read = fread(buffer, 1, BYTES_PER_LINE, file)) > 0) {
        // Print the offset (address) in the first column
        printf("%08lx  ", offset);

        // Print the hexadecimal representation
        for (size_t i = 0; i < bytes_read; i++) {
            print_hex(buffer[i]);
        }

        // Fill in the rest of the hex section with spaces if less than 16 bytes
        for (size_t i = bytes_read; i < BYTES_PER_LINE; i++) {
            printf("   ");
        }

        // Print the ASCII representation
        printf(" |");
        for (size_t i = 0; i < bytes_read; i++) {
            print_ascii(buffer[i]);
        }

        printf("|\n");

        offset += bytes_read;
    }

    fclose(file);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    dump_file(argv[1]);
    return 0;
}
