; Simple clear screen program. Assumes ANSI terminal.
; Jeff Tranter <tranter@pobox.com>

        cpu     8080
        org     100h
bdos    equ     0005h           ; BDOS entry point
start:  mvi     c,9             ; BDOS function: output string
        lxi     d,msg           ; Address of msg
        call    bdos
        ret                     ; Return to CCP

; VT100/ANSI clear screen, cursor home
msg:    db      01BH,"[2J",01BH,"[H$"
        end
