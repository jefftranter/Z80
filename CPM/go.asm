; From the book "8080/Z80 Assembly Language - Techniques for Improved
; Programming" by Alan R. Miller. Adapted to the ASL compiler by
; Jeff Tranter <tranter@pobox.com>
;
; Listing 10.1 A program to go anywhere in memory.
;
;       (date goes here)
;
        TITLE   "GO (Jump anywhere)"
;
; USAGE: TYPE GO F800 TO JUMP TO F800 HEX
;
        ORG     100H
;
BDOS    EQU     5               ;DOS ENTRY POINT
FCB     EQU     5CH             ;FILE CONTROL BLOCK
PBUF    EQU     9               ;PRINT BUFFER
RDBUF   EQU     10              ;READ CONSOLE BUFFER
CR      EQU     0DH             ;CARRIAGE RETURN
LF      EQU     0AH             ;LINE FEED
;
START:
        LXI     H,FCB+1         ;GET ARGUMENT IF ANY
        MOV     A,M             ;FIRST BYTE
        CPI     ' '             ;BLANK?
        JZ      ERROR           ;NO ARGUMENT
AGAIN:  SHLD    RBUFP           ;SAVE POINTER
        CALL    READHL          ;GET ADDRESS
        PCHL                    ;GO TO ADDRESS
;
; CONVERT ASCII-HEX CHARACTERS
; TO 16-BIT BINARY NUMBER IN H,L
;
READHL: LXI     H,0             ;START WITH 0
RDHL2:  CALL    GETCH           ;GET A BYTE
        CPI     ' '             ;END?
        RZ                      ;YES
        CALL    NIB             ;TO BINARY
        JC      RDHL4           ;NOT HEX
        DAD     H               ;TIMES 2
        DAD     H               ;TIMES 4
        DAD     H               ;TIMES 8
        DAD     H               ;TIMES 16
        ORA     L               ;COMBINE NEW
        MOV     L,A             ;PUT BACK
        JMP     RDHL2           ;NEXT
;
; CONVERT ASCII TO BINARY
;
NIB:    SUI     '0'             ;ASCII BIAS
        RC                      ; < 0
        CPI     'F'-'0'+1
        CMC
        RC                      ; > F
        CPI     10
        CMC
        RNC                     ;A NUMBER 0-9
        SUI     'A'-'9'-1
        CPI     10
        RET
;
; BLANK AT END OF LINE IS OK
; ELSE AND ERROR
;
RDHL4:  CPI     ' '-'0'
        RZ
;
; IMPROPER ARGUMENT, TRY AGAIN
;
ERROR:  LXI     D,MESG          ;POINT TO MESSAGE
        CALL    PRINT           ;SEND IT
        LXI     D,RBUFM         ;INPUT BUFFER
        CALL    READB           ;GET A LINE
        MVI     D,0
        LDA     RBUFL           ;BUFFER LENGTH
        MOV     E,A
        LXI     H,RBUF
        DAD     D               ;PAST BUFFER
        MVI     M,' '           ;PUT IN BLANK
        LXI     H,RBUF
        JMP     AGAIN           ;TRY AGAIN
;
; PRINT CHARACTERS UNTIL $ IS FOUND
;
PRINT:  MVI     C,PBUF          ;SET FOR PRINT
        JMP     BDOS
;
; INPUT A LINE FROM CONSOLE
;
READB:  MVI     C,RDBUF         ;READ INPUT BUFFER
        JMP     BDOS
;
; GET A CHARACTER FROM THE INPUT BUFFER
;
GETCH:  PUSH    H
        LHLD    RBUFP           ;GET POINTER
        MOV     A,M             ;GET NEXT CHAR
        INX     H               ;INCREMENT POINTER
        SHLD    RBUFP           ;SAVE POINTER
        CPI     'Z'+7           ;UPPER CASE?
        JC      GETC2           ;NO
        ANI     5FH             ;MAKE UPPER CASE
GETC2:  POP     H
        RET
MESG:
        DB      "GO error. Input "
        DB      "the address again."
        DB      CR,LF,"*$"
;
RBUFP:  DW      RBUF            ;BUFFER POINTER
RBUFM:  DB      10              ;MAX SIZE
RBUFL   DS      1               ;ACTUAL SIZE
RBUF:   DS      1               ;INPUT BUFFER
;
        END
