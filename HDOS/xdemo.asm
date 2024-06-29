        ORG     02280H

;;;     DEMO.ASM - HEATH HDOS ASSEMBLY LANGUAGE
;
;       DEMO IS A SHORT AND SIMPLE PROGRAM USED
;       TO DEMONSTRATE THE HDOS ASSEMBLER AND
;       THE HDOS OPERATING SYSTEM
;
;       THIS PROGRAM SIMPLY PRINTS TWO CODED
;       LINES ON THE SYSTEM CONSOLE TERMINAL.

ENTRY   LXI     H,MESA          ; (HL)=ADDRESS OF 1ST MSG
        RST     7               ; SCALL
        DB      3               ; .PRINT
        LXI     H,MESB          ; (HL)=ADDRESS OF 2ND MSG
        RST     7               ; SCALL
        DB      3               ; .PRINT

;       SEND A BELL TO THE TERMINAL

        MVI     A,07Q           ; (A)=ASCII BELL
        RST     7               ; SCALL   .SCOUT  RING TERMINAL'S BELL
        DB      2

;       RETURN CONTROL TO HDOS OPERATING SYSTEM

        XRA     A               ; EXIT TO OPERATING SYSTEM
        RST     7               ; .EXIT
        DB      0

;       MESSAGES FOR .PRINT SCAL'S

MESA    DB      12Q,'HI THERE, SPORTS FANS','!'+200Q
;       LON     G LIST THE BYTES OF THE NEXT MESSAGE
MESB    DB      12Q,'YOUR SYSTEM WORKS FINE','!'+200Q

        END     ENTRY           ; START EXECUTING AT 'ENTRY' LABEL
