; Example of using CP/M Search for First and Search for Next BDOS
; calls to match filename patterns.
; e.g.
; A> glob ????????.c
; HELLO.C
; SEARCH.C
; GLOB.C
; Jeff Tranter <tranter@pobox.com>

        cpu     8080
        org     100h
bdos    equ     05h             ; BDOS entry point
fcb     equ     5ch             ; Default FCB
dma     equ     0080h           ; Default DMA address
searchfirst equ 11h             ; Search for First function
searchnext  equ 12h             ; Search for Next function
printstring equ 09h             ; Print string
CR      equ     0dh             ; Carriage return
LF      equ     0ah             ; Line feed

; On entry, filename pattern is stored in default FCB.

start:  mvi     c,searchfirst   ; BDOS function
        lxi     d,fcb           ; Address of FCB
        call    bdos            ; Call Search for First

; If A is FF, no match found and we exit

        cpi     0ffh            ; Is it FF?
        rz                      ; If so, return

; TODO: Show drive letter (if any)

; Print the filename from the buffer.
; FCB address is DMA address + (reg A * 32)

        add     a               ; A time 2
        add     a               ; A times 4
        add     a               ; A times 8
        add     a               ; A times 16
        add     a               ; A times 32
        aci     81h             ; Add DMA address + 1

        mvi     d,0             ; Set DE to point to filename
        mov     e,a

        mov     h,d             ; Put DE in HL
        mov     l,e

        lxi     b,12            ; Add 12 to it to get to end of filename
        dad     b

        mvi     a,'$'           ; Add terminating '$'
        mov     m,a

        mvi     c,printstring   ; BDOS function
        call    bdos            ; Call Print String

; TODO: Now call Search for Next until there are no more matchs.

ret:    ret                     ; Return to CCP
        end
