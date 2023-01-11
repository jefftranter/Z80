        cpu     8080
        org     100h
bdos    equ     0005h           ; BDOS entry point
start:  mvi     c,9             ; BDOS function: output string
        lxi     d,msg           ; Address of msg
        call    bdos
        ret                     ; Return to CCP
msg:    db      0DH,0AH,"Hello, world!",0DH,0AH,'$'
        end
