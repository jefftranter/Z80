; TypeIt program example taken from the article "Getting Started With
; Assembly Language Part V" by Pat Swayne in issue 42 (July 1983) of
; REMark magazine. It can be assembled on CP/M using:
; ASM TYPEIT=TYPEIT
; LOAD TYPEIT
; If you do not have a printer configured you can assign the console
; to the listing device using the command: "STAT LST:=CRT:".
; Tested under CP/M 2.2.
; Jeff Tranter <tranter@pobox.com>

;       TYPEIT.ASM
;       THIS PROGRAM IS AN ASSEMBLY VERSION OF
;       THE FOLLOWING BASIC PROGRAM
;
;       10 PRINT "TYPE LINES AT YOUR CONSOLE.  THEY"
;       20 PRINT "WILL BE PRINTED (ON YOUR PRINTER)"
;       30 PRINT "WHEN YOU HIT RETURN.  TYPE A PERIOD"
;       40 PRINT "AT THE BEGINNING OF A LINE TO STOP."
;       50 LINE INPUT L$
;       60 IF LEFT$(L$,1)="." THEN END
;       70 LPRINT L$
;       80 GOTO 50
;
;       THIS VERSION IS FOR CP/M, AND UTILIZES SOME
;       BUILT-IN CP/M ROUTINES
;
;       BY P. SWAYNE, HUG  4-MAY-83

;       DEFINE CP/M ROUTINES, ETC.

BOTTOM  EQU     0               ;BOTTOM OF USER MEMORY
BDOS    EQU     BOTTOM+5        ;JUMP INTO CP/M BDOS
TPA     EQU     BOTTOM+100H     ;TRANSIENT PROGRAM AREA
CONOUT  EQU     2               ;CONSOLE OUTPUT FUNCTION
LSTOUT  EQU     5               ;LIST DEVICE (PRINTER) OUTPUT
TYPE    EQU     9               ;BLOCK TEXT TYPE FUNCTION
LINPUT  EQU     10              ;LINE INPUT FUNCTION
STACK   EQU     100H            ;PUT STACK BELOW TPA

        ORG     TPA

;       SET UP STACK

START   LXI     H,0             ;ZERO HL REGISTER
        DAD     SP              ;ADD STACK POINTER VALUE
        LXI     SP,STACK        ;SET OUR STACK POINTER
        PUSH    H               ;SAVE CP/M'S STACK ON OURS

;       10 PRINT "TYPE LINES ... ETC.

        LXI     D,TYPEL         ;POINT TO "TYPE LINES" STRING
        MVI     C,TYPE          ;GET TYPE FUNCTION
        CALL    BDOS            ;TYPE THE MESSAGE

;       50 LINE INPUT L$

LOOP    LXI     D,LINE          ;POINT TO LINE BUFFER
        MVI     C,LINPUT        ;GET LINE INPUT FUNCTION
        CALL    BDOS            ;INPUT THE LINE
        LXI     H,LINE+1        ;POINT TO NO. OF CHARS TYPES
        MOV     E,M             ;GET THE COUNT
        MVI     D,0             ;DE = COUNT
        INX     H               ;MOVE TO THE START OF THE LINE
        PUSH    H               ;SAVE THIS ADDRESS
        DAD     D               ;ADD CHARACTER COUNT
        MVI     M,0DH           ;INSERT A CR
        INX     H               ;MOVE TO NEXT LOCATION
        MVI     M,8AH           ;INSERT LINE FEED + 80H
        MVI     E,0AH           ;GET A LINE FEED
        MVI     C,CONOUT        ;GET CONSOLE OUTPUT FUNCTION
        CALL    BDOS            ;PRINT LINE FEED ON CONSOLE

;       60 IF LEFT$(L$,1)="." THEN END

        POP     H               ;GET LINE START ADDRESS
        MOV     A,M             ;GET FIRST CHARACTER
        CPI     '.'             ;IS IT A PERIOD?
        JNZ     PRINTL          ;IF NOT, PRINT THE LINE
        POP     H               ;ELSE, RESTORE CP/M'S STACK
        SPHL                    ;SET IT
        RET                     ;RETURN TO CP/M

;       70 LPRINT L$

PRINTL  ANI     7FH             ;STRIP 8TH BIT FROM CHARACTER
        MVI     C,LSTOUT        ;GET LIST OUTPUT FUNCTION
        PUSH    PSW             ;SAVE CHARACTER
        PUSH    H               ;SAVE POINTER
        MOV     E,A             ;PUT CHARACTER IN E
        CALL    BDOS            ;PRINT THE CHARACTER
        POP     H               ;RESTORE ADDRESS
        POP     PSW             ;RESTORE CHARACTER
        CMP     M               ;COMPARE WITH ORIGINAL CHARACTER
        INX     H               ;MOVE TO NEXT LOCATION
        MOV     A,M             ;GET NEXT CHARACTER
        JZ      PRINTL          ;END NOT FOUND, CONTINUE

;       80 GOTO 50

        JMP     LOOP            ;LOOP UNTIL USER QUITS

;       DATA AND STORAGE

TYPEL   DB      'TYPE LINES AT YOUR CONSOLE.  THEY',13,10
        DB      'WILL BE PRINTED (ON YOUR PRINTER)',13,10
        DB      'WHEN YOU HIT RETURN.  TYPE A PERIOD',13,10
        DB      'AT THE BEGINNING OF A LINE TO STOP.',13,10,'$'

LINE    DB      80,0            ;LINE INPUT BUFFER

        END     START
