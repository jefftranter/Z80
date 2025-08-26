; CP/M file viewer program example taken from the article "Getting
; Started With Assembly Language Part VII" by Pat Swayne in issue 46
; (November 1983) of REMark magazine. It can be assembled on CP/M
; using:
;   ASM VIEW=VIEW
;   LOAD VIEW
; Tested under CP/M 2.2.
; Jeff Tranter <tranter@pobox.com>

;       VIEW -- VIEW FILES ON SCREEN
;
;       TO USE THIS PROGRAM, ENTER
;
;       A>VIEW d:FILENAME.TYP
;
;       THE FILE WILL BE LISTED ON THE SCREEN.
;
;       BY  P.SWAYNE, HUG  8-OCT-83

;       CP/M SYSTEM CALLS, ETC.

CONIN   EQU     1               ;CONSOLE INPUT
CONOUT  EQU     2               ;CONSOLE OUTPUT
PRINT   EQU     9               ;PRINT STRING
CONST   EQU     11              ;CHECK CONSOLE STATUS
OPEN    EQU     15              ;OPEN FILE
READ    EQU     20              ;READ FILE (SEQUENTIAL)
BDOS    EQU     5               ;BDOS VECTOR
DFCB    EQU     5CH             ;DEFAULT FCB
DMA     EQU     80H             ;DMA AREA

        ORG     100H            ;ALWAYS START HERE

START   LXI     H,0
        DAD     SP              ;LOCATE STACK
        LXI     SP,STACK        ;SET OUR OWN
        PUSH    H               ;SAVE CP/M'S STACK
        LXI     D,FCB+1         ;POINT TO FCB SECOND CHAR
        LDAX    D               ;GET IT
        CPI     ' '             ;SPACE?
        JNZ     GOTFILE         ;NO, WE HAVE A FILE
        LXI     D,NOFLE
        MVI     C,PRINT
        CALL    BDOS            ;PRINT NO FILE MESSAGE
        JMP     EXIT1           ;RETURN TO CP/M

GOTFILE DCX     D               ;BACK UP TO FCB START
        PUSH    D               ;SAVE THIS ADDRESS
        MVI     C,OPEN
        CALL    BDOS            ;TRY TO OPEN FILE
        INR     A               ;TEST RESULT
        JNZ     RLOOP           ;GOOD OPEN
        LXI     D,NOOPEN
        MVI     C,PRINT
        CALL    BDIS            ;SAY CAN'T OPEN FILE
        JMP     EXIT            ;RETURN TO CP/M
RLOOP   POP     D               ;GET FCB ADDRESS
        PUSH    D               ;SAVE IT AGAIN
        MVI     C,READ
        CALL    BDOS            ;TRY TO READ A RECORD
        ORA     A               ;READ OK?
        JNZ     EXIT            ;NO, END OF FILE
        LXI     H,DMA           ;ELSE POINT TO DMA AREA
        MVI     B,128           ;PRINT 128 CHARACTERS
PLOOP   MOV     A,M             ;GET A CHARACTER
        CPI     'Z'-40H         ;CONTROL-Z?
        JZ      EXIT            ;YES, END
        MOV     E,A             ;ELSE PUT CHARACTER IN E
        PUSH    H               ;SAVE POINTER
        PUSH    B               ;SAVE COUNTER
        MVI     C,CONOUT
        CALL    BDOS            ;PUT CHARACTER ON SCREEN
        MVI     C,CONST
        CALL    BDOS            ;CHECK CONSOLE STATUS
        POP     B               ;GET COUNTER
        POP     H               ;GET POINTER
        ORA     A               ;ANY KEY HIT?
        JNZ     CEXIT           ;YES, EXIT
        INX     H               ;ELSE INCREMENT POINTER
        DCR     B               ;DECREMENT COUNTER
        JNZ     PLOOP           ;LOOP UNTIL 128 CHARS PRINTED
        JMP     RLOOP
CEXIT   MVI     C,CONIN
        CALL    BDOS            ;ABSORB ABORT CHARACTER
EXIT    POP     D               ;FIX STACK
EXIT1   POP     H               ;GET CP/M STACK
        SPHL                    ;SET IT
        RET                     ;RETURN TO CP/M

;       MESSAGES AND STACK AREA

NOFILE  DB      13,10,'The correct use of this program is'
        DB      13,10,10,'A>d:VIEW d:FILENAME.TYPE',13,10,10
        DB      'where d: is a drive designation '
        DB      '(A:, B:, etc.)',13,10
        DB      'and FILENAME.TYPE is the file to be VIEWed.'
        DB      13,10,'$'
NOOPEN  DB      13,10,'ERROR -- File not found.',13,10,'$'

        DS      32             ;SPACE FOR STACK
STACK   EQU     $              ;PUT OUR STACK HERE
        END     START
