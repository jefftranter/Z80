#include <arch/z80.h>
#include <stdint.h>
#include "hdosutil.h"

/* Wrapper for HDOS system call (scall). Pass in scall number and
   register values. Returns carry status (normally 1 for error, 0 for
   success.
*/
int scall(uint8_t request, uint8_t *a, uint16_t *bc, uint16_t *de, uint16_t *hl)
{
    static Z80_registers regs;
    extern uint8_t *CALLNO;

    // Write system call number after RST instruction
    z80_bpoke(&CALLNO, request);

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
