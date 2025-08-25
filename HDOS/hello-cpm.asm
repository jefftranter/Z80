; Hello program example taken from the article "Getting Started With
; Assembly Language Part III" by Pat Swayne in issue 40 (May 1983) of
; REMark magazine. It can be assembled on CP/M using:
;   ASM HELLO=HELLO
;   LOAD HELLO
; Tested under CP/M 2.2.
; Jeff Tranter <tranter@pobox.com>

;       THIS PROGRAM IS AN ASSEMBLY VERSION OF
;       THE FOLLOWING BASIC PROGRAM
;
;       10 PRINT "HELLO, WHAT'S YOUR NAME" ";
;       20 LINE INPUT "";N$
;       30 FOR I = 1 TO 10
;       40 PRINT N$
;       50 NEXT I
;       60 END
;
;       THIS VERSION IS FOR CP/M, AND UTILIZES SOME
;       BUILT-IN CP/M ROUTINES
;
;       BY P. SWAYNE, HUG  20-MAY-82

;       DEFINE CP/M ROUTINES, ETC.

BOTTOM  EQU     0               ;BOTTOM OF USER MEMORY
BDOS    EQU     BOTTOM+5        ;JUMP INTO CP/M BDOS
TPA     EQU     BOTTOM+100H     ;TRANSIENT PROGRAM AREA
CONOUT  EQU     2               ;CP/M CONSOLE SINGLE CHARACTER OUTPUT
TYPE    EQU     9               ;CP/M TYPE FUNCTION
LINPUT  EQU     10              ;CP/M LINE INPUT FUNCTION

        ORG     TPA             ;MOST CP/M PROGRAMS START HERE

;       10 PRINT "HELLO, WHAT'S YOUR NAME" ";

START   LXI     H,0             ;ZERO HL REGISTER
        DAD     SP              ;ADD STACK POINTER VALUE
        LXI     SP,STACK        ;SET OUR STACK POINTER
        PUSH    H               ;SAVE CP/M'S STACK ON OURS
        LXI     D,HELLO         ;POINT TO "HELLO" STRING
        MVI     C,TYPE          ;GET TYPE FUNCTION
        CALL    BDOS            ;PRINT "HELLO" MESSAGE

;       20 LINE INPUT "";N$

        LXI     D,NAME          ;POINT TO WHERE NAME GOES
        MVI     C,LINPUT        ;GET LINE INPUT FUNCTION
        CALL    BDOS            ;INPUT THE NAME
        LXI     H,NAME+1        ;POINT TO NO. OF CHARACTERS TYPED
        MOV     E,M             ;GET THE COUNT
        MVI     D,0             ;DE = COUNT
        INX     H               ;MOVE TO THE START OF THE NAME
        DAD     D               ;ADD COUNT TO GET TO THE END
        MVI     M,0DH           ;INSERT A CR
        INX     H               ;MOVE TO NEXT LOCATION
        MVI     M,0AH           ;INSERT A LINE FEED
        INX     H
        MVI     M,'$'           ;INSERT A "$' (STRING TERMINATOR)
        MVI     E,0AH           ;GET A LINE FEED
        MVI     C,CONOUT        ;GET CONSOLE OUTPUT FUNCTION
        CALL    BDOS            ;PRINT LINE FEED

;       30 FOR I = 1 TO 10

        MVI     B,10            ;SET UP A COUNTER

;       40 PRINT N$

PRINT   LXI     D,NAME+2        ;POINT TO NAME
        MVI     C,TYPE          ;GET TYPE FUNCTION
        PUSH    B               ;SAVE THE COUNT
        CALL    BDOS            ;PRINT THE NAME
        POP     B               ;RESTORE THE COUNT

;       50 NEXT I

        DCR     B               ;DECREMENT COUNTER
        JNZ     PRINT           ;LOOP UNTIL IT'S ZERO

;       60 END

        POP     H               ;RESTORE CP/M'S STACK
        SPHL                    ;SET IT
        RET                     ;RETURN TO CP/M

;       PROMPT STRING

HELLO   DB      'HELLO, WHAT''S YOUR NAME? $'

;       PLACE TO STORE NAME (VARIABLE N$)

NAME    DB      30,0            ;MAX ALLOWED CHARACTERS, COUNT
        DB      33              ;ALLOW 30 CHARACTER NAME + CR, LF, "$"

;       OUR PRIVATE STACK

        DS      32              ;ALLOW 16 "PUSHES" (2 BYTES EACH)
STACK   EQU     $               ;START STACK HERE

        END     START           ;TELL ASSEMBLER WHERE PROGRAM STARTS
