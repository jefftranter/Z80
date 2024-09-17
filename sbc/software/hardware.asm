;
; Z88DK input/output routines for Z80 SBC.
;
; 8K ROM  from $0000 - $1FFF
; 32K RAM from $8000 - $FFFF
; 6850 UART at i/o ports $80,81

; You typically want to build it with a command line like this:
;   zcc +z80 -clib=classic \
;    -pragma-define:CRT_MODEL=1 \
;    -pragma-define:CRT_ORG_CODE=0x8000 \
;    -pragma-define:CRT_ON_EXIT=0x00000 \
;    -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 \
;    hello.c hardware.asm -create-app -m -Cz--ihex
; The resulting .hex file can be loaded from JMON and run at address
; 8000.
;
; You can disassemble the binary using a command like:
;   z88dk-dis -x hello.map -o 0x8000 hello.bin
;
; To Do:
;   Write custom crt0.asm.
;
; See:
;   https://z88dk.org/site/
;   https://github.com/z88dk/z88dk/wiki/Classic--Homebrew
;
; Copyright 2024 by Jeff Tranter <tranter@pobox.com>

SECTION code_user

PUBLIC fputc_cons_native
PUBLIC _fputc_cons_native

PUBLIC fgetc_cons
PUBLIC _fgetc_cons

; 6580 UART defines
SREG equ    $80
CREG equ    $80
DREG equ    $81

fputc_cons_native:
_fputc_cons_native:
        pop     bc              ; Return address
        pop     hl              ; Character to print in L
        push    hl
        push    bc

        ld      a,l             ; Get char to print
        cmp     '\n'            ; Is it newline?
        jr      nz,prnt         ; Branch if not
        ld      l,'\r'          ; If so, first print return
        call    prnt
        ld      l,'\n'          ; Now print newline
prnt:   push    af              ; Save A register
loop1:  in      a,(SREG)        ; Read status register
        bit     1,A             ; Test TDRE bit
        jr      z,loop1         ; Repeat until TDRE is set
        ld      a,l             ; Put char to output in A
        out     (DREG),a        ; Output it to data register
        pop     af              ; Restore A
        ret

fgetc_cons:
_fgetc_cons:
        in      a,(SREG)        ; Read status register
        bit     0,A             ; Test RDRF bit
        jr      z,_fgetc_cons   ; Repeat until RDRF is set
        in      a,(DREG)        ; Read character from data register
        ld      l,a             ; Return the result in HL
        ld      h,0
        ret                     ; And return
