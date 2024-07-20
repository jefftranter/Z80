/*

  Input/output Routines for demo.

  Jeff Tranter <tranter@pobox.com>

*/

#include <stdio.h>

/*

Character input/output using different methods:

1. Direct hardware access to 8250 UART i/o ports. Works somewhat, but
   input does not play well with CP/M or HDOS as they use
   interrupt-driven input and fight for access.

2. Call monitor ROM routines. Not available under CP/M as the boot ROM
   is not normally mapped into memory. Works under HDOS.

3. Call CP/M BDOS functions. Already supported by z88dk, so not
   implemented here.

4. Call HDOS system calls.

To Do: Add file i/o routines.

*/

__asm
ACE equ $E8         ; 8250 ACE i/o address
THR equ ACE+0       ; THR register
RBR equ ACE+0       ; RBR register
LSR equ ACE+5       ; LSR register
DR  equ %00000001   ; DR bit
THE equ %00100000   ; THE bit
__endasm

int fputc_cons_native(char c) __naked
{
__asm
    pop     bc      ; Return address
    pop     hl      ; Character to print in l
    push    hl
    push    bc
l1: in      LSR     ; Read status register
    ani     THE     ; Look at THE bit
    jr      z,l1    ; Branch until empty

    ld      a,l     ; Get char to print
    ani     %01111111 ; Convert to 7-bit ASCII
    out     THR     ; Output to UART
    ret
__endasm
}

int fgetc_cons() __naked
{
__asm

l2: in      LSR     ; Read status register
    ani     DR      ; See if there is data ready
    jr      z,l2    ; Repeat if not
    in      RBR     ; Get character
    ani     %01111111 ; Convert to 7-bit ASCII
    ld      l,a     ; Return the result in hl
    ld      h,0
    ret
__endasm
}
