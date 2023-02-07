; Example of using CP/M Search for First and Search for Next BDOS
; calls to match filename patterns.
; e.g.
; A> glob *.c
; HELLO.C
; SEARCH.C
; GLOB.C
;
; Jeff Tranter <tranter@pobox.com>

        cpu     8080
        org     100h
bdos    equ     05h             ; BDOS entry point
fcb     equ     5ch             ; Default FCB
dma     equ     0080h           ; Default DMA address
conout  equ     02h             ; Console out function
searchfirst equ 11h             ; Search for First function
searchnext  equ 12h             ; Search for Next function
CR      equ     0dh             ; Carriage return
LF      equ     0ah             ; Line feed

; On entry, filename pattern is stored in default FCB.

start:  mvi     c,searchfirst   ; BDOS function
loop:   lxi     d,fcb           ; Address of FCB
        call    bdos            ; Call Search

; If A is FF, no match found and we exit

        cpi     0ffh            ; Is it FF?
        rz                      ; If so, return

; Print the filename from the buffer.
; FCB address is DMA address + (reg A * 32)

        add     a               ; A times 2
        add     a               ; A times 4
        add     a               ; A times 8
        add     a               ; A times 16
        add     a               ; A times 32
        aci     80h             ; Add DMA address
        mvi     h,0             ; Set HL to point to filename
        mov     l,a

; Print drive letter
        lda     fcb             ; Get drive code
        cpi     0               ; Is it 0?
        jz      default         ; Yes, so default drive
        adi     'A'-1           ; Convert to drive letter (1=A, 2=B, etc.)
        mov     e,a             ; Put in E
        call    printchar       ; And print
        mvi     e,':'           ; Print colon
        call    printchar

; Print filename, less extension
default:
        inx     h               ; Advance HL to point to start of filename
        mvi     a,8             ; Length is 8
        call    printstring     ; Print filename

; Print "."
        mvi     e,'.'
        call    printchar

; Print file extension
        lxi     d,8             ; Put 8 in DE
        dad     d               ; Add 8 to HL
        mvi     a,3             ; Length is 3
        call    printstring     ; Print filename

; Print CRLF
        mvi     e,CR
        call    printchar
        mvi     e,LF
        call    printchar

; Now call Search for Next and repeat.

        mvi     c,searchnext    ; BDOS function
        jp      loop            ; Go back and repeat

; Print the character stored in E to the console.
printchar:
        push    h               ; Save registers
        push    d
        push    b
        mvi     c,conout
        call    bdos
        pop     b               ; Restore registers
        pop     d
        pop     h
        ret

; Print a string to the console given address in HL and length in A.
; Based on code from Kathe Spracklen 8080/Z80 book.
printstring:
        push    psw             ; Save registers
        push    b
        push    d
        push    h
        mov     b,a             ; Save count
        mvi     c,conout        ; BDOS function
outlop: mov     e,m             ; Fetch character
        push    h               ; Save registers
        push    d
        push    b
        call    bdos
        pop     b               ; Restore registers
        pop     d
        pop     h
        inx     h               ; Next character
        dcr     b               ; Decrement count
        jnz     outlop          ; Repeat until done
        pop     h               ; Restore registers
        pop     d
        pop     b
        pop     psw
        ret                     ; Done

        end
