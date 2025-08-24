* HDOS file viewer program example taken from the article "Getting
* Started With Assembly Language Part VI" by Pat Swayne in issue 45
* (October 1983) of REMark magazine. It can be assembled on HDOS
* using: ASM VIEW=VIEW
* Tested under HDOS 2.0 and 3.02.
* Jeff Tranter <tranter@pobox.com>

*       VIEW -- VIEW FILES ON SCREEN
*
*       TO USE THIS PROGRAM, ENTER
*
*       >VIEW dev:FILENAME.EXT
*
*       THE FILE WILL BE LISTED ON THE SCREEN.
*
*       BY  P.SWAYNE, HUG   18-JUN-82

*       HDOS SYSTEM CALLS, ETC.

.EXIT   EQU     0
.SCIN   EQU     1
.SCOUT  EQU     2
.READ   EQU     4
.CTLC   EQU     41Q
.OPENR  EQU     42Q
.CLOSE  EQU     46Q
.ERROR  EQU     57Q
$TYPTX  EQU     31136A          TYPE TEXT FUNCTION (ROM)
EC.EOF  EQU     1               END OF FILE READ

*       MAIN PROGRAM

START   LXI     H,0
        DAD     SP              LOCATE STACK
        MOV     A,L
        CPI     200Q            HAS IT MOVED?
        JNZ     GOTFILE         IF SO, CONTINUE
        CALL    $TYPTX
        DB      10,'The correct use of this program is',10,10
        DB      '>dev:VIEW dev:FILENAME.EXT',10,10
        DB      'where dev: is a drive designation '
        DB      '(SY1:, DK0:, etc.)',10
        DB      'and FILENAME.EXT is the file to be VIEWed.'
        DB      10+80H
        XRA     A
        SCALL   .EXIT           EXIT TO HDOS
GOTFILE MOV     A,M             GET A CHARACTER
        CPI     ' '             SPACE?
        INX     H               INCREMENT POINTER
        JZ      GOTFILE         SKIP SPACES
        DCX     H               FIX POINTER
        LXI     D,DEFALT        POINT TO DEFAULTS
        XRA     A               USE CHANNEL 0
        SCALL   .OPENR          OPEN THE FILE
        JC      ERROR           CAN'T DO IT
        LXI     H,END
        MVI     A,3
        SCALL   .CTLC           SET UP CONTROL-C EXIT
RLOOP   LXI     D,BUFFER        PUT FILE HERE
        LXI     B,256           READ 1 SECTOR
        XRA     A               CHANNEL 0
        SCALL   .READ           READ THE SECTOR
        JNC     GOODRD
        CPI     EC.EOF          END OF FILE?
        JNZ     ERROR           NO, MUST BE ERROR
        JMP     END             YES, FOUND END
GOODRD  CALL    TYPBUF          TYPE CONTENTS OF BUFFER
        JMP     RLOOP           READ ANOTHER SECTOR
END     XRA     A
        SCALL   .CLOSE          CLOSE THE FILE
        XRA     A
        SCALL   .EXIT           LEAVE
TYPBUF  LXI     H,BUFFER        POINT TO BUFFER
        MVI     B,0             SET UP COUNTER
TYLOOP  MOV     A,M             GET A CHARACTER
        SCALL   .SCOUT          PRINT IT ON SCREEN
        INX     H               INCREMENT POINTER
        DCR     B               DONE?
        JNZ     TYLOOP
        RET
ERROR   PUSH    PSW             SAVE ERROR CODE
        CALL    $TYPTX
        DB      10,'Error --',' '+80H
        POP     PSW
        MVI     H,7             ASCII BELL
        SCALL   .ERROR
        JMP     END

*       DATA AREA

DEFALT  DB      'SY0DOC'
BUFFER  DS      256             DATA BUFFER
        END     START
