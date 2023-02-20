; Simple program to echo command line arguments to console.
; Note that CP/M converts command line to all uppercase.
; Also assumes first character, if present, is a space and skips it.
; Jeff Tranter <tranter@pobox.com>

        cpu     8080
        org     100h
bdos    equ     05h             ; BDOS entry point
conout  equ     02h             ; Console out function
CR      equ     0dh             ; Carriage return
LF      equ     0ah             ; Line feed
cmdline equ     0080h           ; Command line options stored here (length byte first)

start:  mvi     c,conout        ; BDOS function
        lxi     h,cmdline       ; Address of command line
        mov     d,m             ; Get length
        mov     a,d
        cpi     0
        rz                      ; Done if zero
        inx     h               ; Advance pointer past length byte

loop:   inx     h               ; Advance pointer to next char
        dcr     d               ; Decrement character count
        jz      eos             ; Done if reached zero
        mov     e,m             ; Get next character
        push    h               ; Save registers
        push    d
        push    b
        call    bdos            ; Print it
        pop     b               ; Restore registers
        pop     d
        pop     h
        jp     loop             ; Repeat

eos:    mvi     e,CR            ; Print CRLF
        call    bdos
        mvi     e,LF
        call    bdos
        ret                     ; Return to CCP
        end
