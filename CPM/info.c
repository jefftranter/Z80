/*
 Show info about CP/M. Typical output:

OS: CP/M
Machine Type: 8080/Z80
Version: 2.2
Current User: 0
Current Disk: C
Login Vector: A B C D
Read Only Vector: D
IOBYTE:
CON: is CRT:
RDR: is TTY:
PUN: is TTY:
LST: is TTY:
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

    v = bdos(CPM_VERS, 0);
    printf("OS: ");
    if (v & 0x0100) {
        printf("MP/M\n");
    } else if (v & 0x0200) {
        printf("CP/Net\n");
    } else if (v & 0x0400) {
        printf("16-bit multi-user\n");
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
    printf("Current Disk: %c\n", bdos(CPM_IDRV, 0) + 'A');

    v = bdos(CPM_ILOG, 0);
    printf("Login Vector:");
    for (i = 0; i < 32; i++) {
        if (v & (1 << i)) {
            printf(" %c", i+'A');
        }
    }
    printf("\n");

    v = bdos(29 /*CPMGROV*/, 0);
    printf("Read Only Vector:");
    for (i = 0; i < 32; i++) {
        if (v & (1 << i)) {
            printf(" %c", i+'A');
        }
    }
    printf("\n");

    v = bdos(CPM_GIOB, 0);
    printf("IOBYTE:\nCON: is ");
    switch (v & 3) {
    case 0:
        printf("TTY:");
        break;
    case 1:
        printf("CRT:");
        break;
    case 2:
        printf("BAT:");
        break;
    case 3:
        printf("UC1:");
        break;
    }
    printf("\nRDR: is ");
    switch ((v & 12) >> 2) {
    case 0:
        printf("TTY:");
        break;
    case 1:
        printf("PTR:");
        break;
    case 2:
        printf("UR1:");
        break;
    case 3:
        printf("UR2:");
        break;
    }
    printf("\nPUN: is ");
    switch ((v & 48) >> 4) {
    case 0:
        printf("TTY:");
        break;
    case 1:
        printf("PTP:");
        break;
    case 2:
        printf("UP1:");
        break;
    case 3:
        printf("UP2:");
        break;
    }
    printf("\nLST: is ");
    switch ((v & 192) >> 6) {
    case 0:
        printf("TTY:");
        break;
    case 1:
        printf("CRT:");
        break;
    case 2:
        printf("LPT:");
        break;
    case 3:
        printf("UL1:");
        break;
    }
    printf("\n");

    printf("Allocation Vector: %4X\n", bdos(27 /*CPMGALL*/, 0));
    printf("Disk Parameter Block: %4X\n", bdos(31 /*CPM_DPB*/, 0));
}
