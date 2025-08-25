* Hello program example taken from the article "Getting Started With
* Assembly Language Part I and II" by Pat Swayne in issue 39 (April
* 1983) of REMark magazine. It can be assembled on HDOS using: ASM
* HELLO=HELLO
* Tested under HDOS 2.0 and 3.02.
* Jeff Tranter <tranter@pobox.com>

*       THIS PROGRAM IS AN ASSEMBLY VERSION OF
*       THE FOLLOWING BASIC PROGRAM
*
*       10 PRINT "HELLO, WHAT'S YOUR NAME" ";
*       20 LINE INPUT "";N$
*       30 FOR I = 1 TO 10
*       40 PRINT N$
*       50 NEXT I
*       60 END
*
*       THIS VERSION IS FOR HDOS, AND UTILIZES SOME
*       BUILT-IN HDOS ROUTINES
*
*
*       BY P. SWAYNE, HUG  20-MAY-82

*       DEFINE HDOS ROUTINES

$TYPTX  EQU     31136A          TYPE THE TEXT THAT FOLLOWS CALL
.SCIN   EQU     1               INPUT ONE CHARACTER FROM KEYBOARD
.PRINT  EQU     3               PRINT TEXT POINTED TO BY HL
.EXIT   EQU     0               EXIT TO HDOS FROM PROGRAM

        ORG     42200A          MOST HDOS PROGRAMS START HERE

*       10 PRINT "HELLO, WHAT'S YOUR NAME" ";

START   CALL    $TYPTX          THIS IS AN HDOS RESIDENT ROUTINE
        DB      'HELLO, WHAT''S YOUR NAME?',' '+200Q

*       20 LINE INPUT "";N$

        LXI     H,NAME          POINT TO WHERE NAME GOES
        MVI     B,31            ALLOW 30 CHARACTERS + RETURN
INPUT   SCALL   .SCIN           CALL HDOS INPUT ROUTINE
        JC      INPUT           WAIT FOR INPUT
        MOV     M,A             STORE ONE CHARACTER
        DCR     B               USED UP CHARACTER LIMIT?
        JZ      DUMP            IF SO, DUMP THE EXTRAS
        INX     H               MOVE TO NEXT LOCATION
        CPI     12Q             END OF ENTRY?
        JNZ     INPUT           IF NOT, GET MORE
        DCX     H               BACK UP TO LAST ENTRY
DUMP    SCALL   .SCIN           DUMP EXTRA CHARACTERS
        JNC     DUMP
        MVI     M,12Q           END ENTRY (IN CASE OF OVERFLOW)
TERM    MOV     A,M             GET LAST CHARACTER
        ORI     200Q            SET 8TH BIT (MARK END)
        MOV     M,A             REPLACE CHARACTER

*       30 FOR I = 1 TO 10

        MVI     B,10            SET UP A COUNTER

*       40 PRINT N$

PRINT   LXI     H,NAME          POINT TO NAME
        SCALL   .PRINT          PRINT IT

*       50 NEXT I

        DCR     B               DECREMENT COUNTER
        JNZ     PRINT           LOOP UNTIL IT'S ZERO

*       60 END

        XRA     A               PUT ZERO IN THE A REGISTER
        SCALL   .EXIT           RETURN TO HDOS

*       PLACE TO STORE VARIABLE NAME (VARIABLE A$)

NAME    DS      31              ALLOW 30 CHARACTERS + END CHAR

        END     START           TELL ASSEMBLER WHERE PROGRAM STARTS
