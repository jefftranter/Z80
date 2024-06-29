; Simple screen clear program that runs on HDOS.

        ORG     02280H

; Clear screen of Heath H89.

        LXI     H,MSG           ; (HL) = ADDRESS OF MSG
        RST     7               ; SCALL
        DB      3               ; .PRINT

;       RETURN CONTROL TO HDOS OPERATING SYSTEM

        XRA     A               ; EXIT TO OPERATING SYSTEM
        RST     7               ; .EXIT
        DB      0

;       MESSAGES FOR .PRINT SCAL'S

MSG     DB      033Q,'E'+200Q
