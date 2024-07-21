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
   is not normally mapped into memory. Works under HDOS 1 where ROM is
   still mapped in but not in HDOS 2 fi 64K. Also suffers from same
   interrupt driven issue on input as method 1.

3. Call CP/M BDOS functions. Already supported by z88dk, but also
   implemented here for example purposes.

4. Call HDOS system calls.

To Do: Add file i/o routines.

*/

// Define to one of 1, 2, or 4 as above.

#define METHOD 4

/********************* METHOD 1 ***************************************/

#if METHOD == 1

__asm
ACE equ $E8         ; 8250 ACE i/o address
THR equ ACE+0       ; THR register
RBR equ ACE+0       ; RBR register
LSR equ ACE+5       ; LSR register
DR  equ %00000001   ; DR bit
THE equ %00100000   ; THE bit
__endasm

void init() { }

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

#endif

/********************* METHOD 2 ***************************************/

#if METHOD == 2

__asm
WCC equ     $03C2  ; Write char in A to console
RCC equ     $03B2  ; Read console, return char in A
__endasm

void init() { }

int fputc_cons_native(char c) __naked
{
__asm
    pop     bc      ; Return address
    pop     hl      ; Character to print in l
    push    hl
    push    bc
    ld      a,l     ; Get char to print
    call    WCC     ; Write character to console
    ret
__endasm
}

int fgetc_cons() __naked
{
__asm
    call    RCC     ; Read console character
    ld      l,a     ; Return the result in hl
    ld      h,0
    ret
__endasm
}

#endif

/********************* METHOD 3 ***************************************/

#if METHOD == 3

__asm
BOOT equ    $0000  ; Return to CP/M
BDOS equ    $0005  ; CP/M BDOS call
DIO equ     $06    ; BDOS call for Direct Console i/o
__endasm

void init() { }

int fputc_cons_native(char c) __naked
{
__asm
    pop     bc      ; Return address
    pop     hl      ; Character to print in l
    push    hl
    push    bc
    push    af      ; Save registers
    push    bc
    push    de
    push    hl
    ld      c,DIO   ; BDOS call
    ld      e,l     ; Char is passed in e
    call    BDOS    ; Call BDOS to output char
    pop     hl      ; Restore registers
    pop     de
    pop     bc
    pop     af
    ret
__endasm
}

int fgetc_cons() __naked
{
__asm
    push    bc      ; Save registers
    push    de
l1: ld      c,DIO   ; BDOS call
    ld      e,0FFH  ; Indicates to read
    call    BDOS    ; Call BDOS to input char
    cp      0       ; Char returned?
    jr      z,l1    ; If not, try again
    pop     de
    pop     bc
    ld      l,a     ; Return the result in hl
    ld      h,0
    ret
__endasm
}

#endif

/********************* METHOD 4 ***************************************/

#if METHOD == 4

__asm
SCALL   macro   call            ; SYSCALL macro
        rst     $38
        db      call
        endm
EXIT    equ     0               ; HDOS System calls
SCIN    equ     1
SCOUT   equ     2
CONSL   equ     6
CSLMD   equ     0               ; Index for console mode
CSLECH  equ     %10000000       ; Bit for suppress echo
CSLCHR  equ     %00000001       ; Bit for update in character mode
__endasm

void init()
{
__asm
                                ; Set console mode under HDOS
        ld      a,CSLMD         ; Index
        ld      b,CSLECH|CSLCHR ; Suppress echo and update in character mode
        ld      c,CSLECH|CSLCHR ; Mask
        SCALL    CONSL          ; Initialize HDOS console
__endasm
}

int fputc_cons_native(char c) __naked
{
__asm
        pop     bc              ; Return address
        pop     hl              ; Character to print in l
        push    hl
        push    bc
        ld      a,l             ; Get char to print
        cmp     '\n'            ; Is it newline?
        jr      nz,pr1          ; Branch if not
        ld      a,'\r'          ; If so, first print return
        SCALL   SCOUT
        ld      a,'\n'          ; Now print newline
pr1:    SCALL   SCOUT           ; System call for System Console Output
        ret
__endasm
}

int fgetc_cons() __naked
{
__asm
nr:     SCALL   SCIN            ; System call for System Console Input
        jr      c,nr            ; Try again if no character ready
        ld      l,a             ; Return the result in hl
        ld      h,0
        ret
__endasm
}

#endif
