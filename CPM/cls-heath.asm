; Simple clear screen program. Assumes Heathkit terminal.
; Jeff Tranter <tranter@pobox.com>

        cpu     8080
        org     100h
bdos:   equ     0005h           ; BDOS entry point
start:  mvi     c,9             ; BDOS function: output string
        lxi     d,msg           ; Address of msg
        call    bdos
        ret                     ; Return to CCP

; Heathkit H19/H88/H89 clear display
msg:    db      01BH,"E$"
        end
