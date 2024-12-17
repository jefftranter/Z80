/*

  Example of performing HDOS system calls from C using z88dk.

  Jeff Tranter <tranter@pobox.com>

  To see disassembly, do:
  ~/git/z88dk/bin/z88dk-dis -x a.map -o 0x2280 a.bin  | less -i

*/

#include <stdio.h>
#include <arch/z80.h>

extern uint8_t *CALLNO;

/* Wrapper for HDOS system call (scall). Pass in scall number and register values.
   Returns carry status (normally set for error, clear for success. */
int scall(uint8_t request, uint8_t *a, uint16_t *bc, uint16_t *de, uint16_t *hl)
{
    Z80_registers regs;

    // Write system call number after RST instruction
    z80_bpoke(&CALLNO, request);

    // TODO: We could pass NULL to indicate parameters that are not needed?

    // Set up register parameters.
    regs.Bytes.A = *a;
    regs.UWords.BC = *bc;
    regs.UWords.DE = *de;
    regs.UWords.HL = *hl;

    // Call SYSCALL routine below.
    AsmCall(&SYSCALL, &regs, REGS_MAIN, REGS_MAIN);

    // Get back register values to return.
    *a = regs.Bytes.A;
    *bc = regs.Words.BC;
    *de = regs.Words.DE;
    *hl = regs.Words.HL;

    // Return carry status
    return regs.Flags.C;
}

#asm
_SYSCALL:
        rst     $38     ; System call
_CALLNO:
        db      $00     ; Call # goes here
        ret
#endasm

int main()
{
    int rc;
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
    rc = scall(011, &a, &bc, &de, &hl);

    printf("Returned %d\n", rc);
    printf("a  = %02x\n", a);
    printf("bc = %04x\n", bc);
    printf("de = %04x\n", de);
    printf("hl = %04x\n", hl);

    s = "\nThis is a test.\n\xcrc180";
    a = 0;
    bc = 0;
    de = 0;
    hl = s;

    printf("\nCalling .PRINT\n");
    rc = scall(3, &a, &bc, &de, &hl);

    printf("Returned %d\n", rc);
    printf("a  = %02x\n", a);
    printf("bc = %04x\n", bc);
    printf("de = %04x\n", de);
    printf("hl = %04x\n", hl);

    printf("\nExiting\n");

    return 0;
}
