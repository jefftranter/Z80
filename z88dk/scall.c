/*

  Example of performing HDOS system calls from C using z88dk.

  Jeff Tranter <tranter@pobox.com>

  To see disassembly (useful for debugging), do:
  ~/git/z88dk/bin/z88dk-dis -x a.map -o 0x2280 a.bin  | less -i

*/

#include <stdio.h>
#include "hdos.h"

int main()
{
    int i, rc;
    uint8_t a;
    uint16_t bc;
    uint16_t de;
    uint16_t hl;
    char *s;

    printf("\nSystem Call Demo\n");

    // Call .VERS - HDOS Version Number (Octal 11Q)
    a = 0;
    bc = 0;
    de = 0;
    hl = 0;

    printf("\nCalling .VERS\n");
    rc = scall(SYSCALL_VERS, &a, &bc, &de, &hl);

    printf("Returned %d\n", rc);
    printf("a  = %02x\n", a);
    printf("bc = %04x\n", bc);
    printf("de = %04x\n", de);
    printf("hl = %04x\n", hl);

    s = "\nThis is a test.\n\x80";
    a = 0;
    bc = 0;
    de = 0;
    hl = s;

    printf("\nCalling .PRINT\n");
    rc = scall(SYSCALL_PRINT, &a, &bc, &de, &hl);

    printf("Returned %d\n", rc);
    printf("a  = %02x\n", a);
    printf("bc = %04x\n", bc);
    printf("de = %04x\n", de);
    printf("hl = %04x\n", hl);

    a = 0;
    bc = 0;
    de = 0;
    hl = 0;

    printf("\nCalling .ERROR\n");
    for (i = 1 ; i <= 56; i++) {
        a = i;
        hl = 0;
        scall(SYSCALL_ERROR, &a, &bc, &de, &hl);
        printf("\n");
    }

    a = 0;
    bc = 0;
    de = 0;
    hl = s;

    printf("\nCalling .SCIN\n");
    printf("Press a key to continue...");
    do {
        rc = scall(SYSCALL_SCIN, &a, &bc, &de, &hl);
    } while (rc == 1);

    printf("\nReturned %d\n", rc);
    printf("a  = %02x\n", a);
    printf("bc = %04x\n", bc);
    printf("de = %04x\n", de);
    printf("hl = %04x\n", hl);

    printf("\nExiting\n");

    return 0;
}
