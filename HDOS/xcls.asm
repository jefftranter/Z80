; Simple H89 screen clear program that runs on HDOS.

; SYSCALL macro
SCALL   MACRO   call
        RST     7
        DB      call
        ENDM

; System calls
EXIT   EQU     0
PRINT  EQU     3

; Start address of user programs
USERFWA EQU     2280H

; ------------------------------------------------------------------------

        ORG     USERFWA

        LXI     H,MSG           ; (HL) = ADDRESS OF MSG
        SCALL   PRINT

;       RETURN CONTROL TO HDOS OPERATING SYSTEM

        XRA     A               ; EXIT TO OPERATING SYSTEM
        SCALL   EXIT

;       MESSAGES FOR .PRINT SCALLS

MSG     DB      033Q,'E'+200Q
