#include <stdint.h>
#include <arch/z80.h>

/* Wrapper for HDOS system call (scall). Pass in scall number and
   register values. Returns carry status (normally 1 for error, 0 for
   success.
*/
int scall(uint8_t request, uint8_t *a, uint16_t *bc, uint16_t *de, uint16_t *hl)
{
    Z80_registers regs;
    extern uint8_t CALLNO[];

    CALLNO[0] = request;

    uint8_t a_val = *a;
    uint16_t bc_val = *bc;
    uint16_t de_val = *de;
    uint16_t hl_val = *hl;

    regs.Bytes.A = a_val;
    regs.UWords.BC = bc_val;
    regs.UWords.DE = de_val;
    regs.UWords.HL = hl_val;

    AsmCall(&SYSCALL, &regs, REGS_MAIN, REGS_MAIN);

    *a = regs.Bytes.A;
    *bc = regs.Words.BC;
    *de = regs.Words.DE;
    *hl = regs.Words.HL;

    return regs.Flags.C;
}

#asm
_SYSCALL:
        rst     $38     ; System call
_CALLNO:
        db      $00     ; Call # goes here
        ret
#endasm
