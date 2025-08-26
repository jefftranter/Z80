* TypeIt program example taken from the article "Getting Started With
* Assembly Language Part IV" by Pat Swayne in issue 41 (June 1983) of
* REMark magazine. It can be assembled on HDOS using:
* ASM TYPEIT=TYPEIT
* If you do not have a printer configured as device LP: you can change
* the definition of LPNAME from 'LP:' to 'TT:' and it will send output
* to the console.
* Tested under HDOS 2.0 and 3.02.
* Jeff Tranter <tranter@pobox.com>

*       TYPEIT.ASM
*       THIS PROGRAM IS AN ASSEMBLY VERSION OF
*       THE FOLLOWING BASIC PROGRAM
*
*       10 PRINT "TYPE LINES AT YOUR CONSOLE.  THEY"
*       20 PRINT "WILL BE PRINTED (ON YOUR PRINTER)"
*       30 PRINT "WHEN YOU HIT RETURN.  TYPE A PERIOD"
*       40 PRINT "AT THE BEGINNING OF A LINE TO STOP."
*       50 OPEN "O",1,"LP:"                     (IF MBASIC)
*       50 OPEN "LP:" FOR WRITE AS FILE #1      (IF BH BASIC)
*       60 LINE INPUT "";L$
*       70 IF LEFT$(L$,1)="." THEN 100
*       80 PRINT #1,L$
*       90 GOTO 60
*       100 CLOSE #1
*       110 END

*       THIS VERSION IS FOR HDOS, AND UTILIZES SOME
*       BUILT-IN HDOS ROUTINES
*
*
*       BY P. SWAYNE, HUG  27-APR-83

*       DEFINE HDOS ROUTINES, ETC.

$TYPTX  EQU     31136A          TYPE TEXT THAT FOLLOWS CALL
.SCIN   EQU     1               INPUT ONE CHARACTER FROM KEYBOARD
.WRITE  EQU     5               WRITE TO A DEVICE
.OPENW  EQU     43Q             OPEN FOR WRITE
.CLOSE  EQU     46Q             CLOSE DEVICE
.ERROR  EQU     57Q             PRINT ERROR MESSAGE
.EXIT   EQU     0               EXIT TO HDOS
NL      EQU     12Q             HDOS NEW-LINE CHARACTER
ENL     EQU     212Q            HDOS END+NEW-LINE CHARACTER

        ORG     42200A          USUAL STARTING PLACE

*       10 PRINT "TYPE LINES AT YOUR CONSOLE,  THEY"

START   CALL     $TYPTX
        DB      'TYPE LINES AT YOUR CONSOLE.  THEY',NL
        DB      'WILL BE PRINTED (ON YOUR PRINTER)',NL
        DB      'WHEN YOU HIT RETURN.  TYPE A PERIOD',NL
        DB      'AT THE BEGINNING OF A LINE TO STOP.',ENL

*       50 OPEN "O",1,"LP:"                     (IF MBASIC)
*       50 OPEN "LP:" FOR WRITE AS FILE #1      (IF BH BASIC)

        LXI     H,LPNAME        POINT TO PRINTER DEVICE NAME
        LXI     D,DEFALT        ADD DEFAULT DEFINITIONS
        MVI     A,1             USE FILE CHANNEL NO. 1
        SCALL   .OPENW          OPEN THE PRINTER DEVICE
        JC      ERROR           COULD NOT OPEN IT

*       60 LINE INPUT "";L$

LOOP    LXI     H,BUFFER        POINT TO BUFFER
        MVI     C,0             CLEAR A COUNTER
INPUT   SCALL   .SCIN           CALL HDOS INPUT ROUTINE
        JC      INPUT           WAIT FOR INPUT
        INR     C               COUNT CHARACTER
        MOV     M,A             STORE CHARACTER
        CPI     NL              END OF LINE?
        JZ      PRINT           IF SO, PRINT LINE
        INX     H               ELSE, INCREMENT BUFFER POINTER
        JMP     INPUT           AND GET ANOTHER CHARACTER

*       70 IF LEFT$(L$,1)="." THEN 100

PRINT   LXI     D,BUFFER        POINT TO BUFFER
        LDAX    D               GET FIRST CHARACTER THERE
        CPI     '.'             IS IT A PERIOD?
        JZ      CLOSE           IF SO, CLOSE FILE AND QUIT

*       80 PRINT #1,L$

        MVI     B,0             BC = CHARACTER COUNT
        MVI     A,1             USE CHANNEL 1
        SCALL   .WRITE          PRINT THE BUFFER'S CONTENTS
        JC      ERROR           SOMETHING IS WRONG!

*       90 GOTO 60

        JMP     LOOP

*       100 CLOSE #1

CLOSE   MVI      A,1            WE USED CHANNEL 1
        SCALL    .CLOSE         CLOSE THAT CHANNEL
        JC       ERROR          SOMETHING IS WRONG!

*       110 END

        XRA      A              SET UP NORMAL EXIT
        SCALL    .EXIT          RETURN TO HDOS

*       IN CASE OF TROUBLE, WE GO HERE

ERROR   PUSH    PSW             SAVE ERROR CODE
        CALL    $TYPTX
        DB      NL,'ERROR - ',ENL
        POP     PSW             GET ERROR CODE
        MVI     H,7             RING THE BELL
        SCALL   .ERROR          LET HDOS PRINT ERROR MESSAGE
        XRA     A               SET UP EXIT
        SCALL   .EXIT           RETURN TO HDOS

*       DATA AND STORAGE AREA

LPNAME  DB      'LP:',0         PRINTER DEVICE NAME
DEFALT  DB      0,0,0,0,0,0
BUFFER  DS      110             MAIN INPUT-OUTPUT BUFFER

        END     START
