/*
 Show info about CP/M. Typical output:

OS: CP/M
Machine Type: 8080/Z80
Version: 2.2
Current User: 0
Current Disk: C
Login Vector: A B C D
Read Only Vector: D
IOBYTE: 0000
Allocation Vector: 00E4
Disk Parameter Block: 0073

 Jeff Tranter <tranter@pobox.com>

*/

#include <stdio.h>
#include <cpm.h>

int main()
{
    int i;
    int v;

    v = bdos(CPMVERS);
    printf("OS: ");
    if (v & MPM) {
        printf("MP/M\n");
    } else if (v & CCPM) {
        printf("Concurrent CP/M\n");
    } else {
        printf("CP/M\n");
    }

    printf("Machine Type: ");
    if (v & 0x2000) {
        printf("68000/Z8000\n");
    } else if (v & 0x1000) {
        printf("8086\n");
    } else {
        printf("8080/Z80\n");
    }

    printf("Version: %d.%d\n", v / 16, v % 16);
    printf("Current User: %d\n", getuid());
    printf("Current Disk: %c\n", bdos(CPMIDRV) + 'A');

    v = bdos(CPMILOG);
    printf("Login Vector:");
    for (i = 0; i < 32; i++) {
        if (v & (1 << i)) {
            printf(" %c", i+'A');
        }
    }
    printf("\n");

    v = bdos(CPMGROV);
    printf("Read Only Vector:");
    for (i = 0; i < 32; i++) {
        if (v & (1 << i)) {
            printf(" %c", i+'A');
        }
    }
    printf("\n");

    /* Note: There appears to be a bug in the Hi-Tech C printf hex
       format output which causes additional incorrect leading digits
       to be displayed (e.g "220073" rather than "0073". I have not
       found any workaround for this. */

    printf("IOBYTE: %2X\n", bdos(CPMGIOB));
    printf("Allocation Vector: %4X\n", bdos(CPMGALL));
    printf("Disk Parameter Block: %4X\n", bdos(CPMDPB));
}
