* File transfer program example taken from the article "Getting
* Started With Assembly Language Part VIII" by Pat Swayne in issue 47
* (December 1983) of REMark magazine. It can be assembled on HDOS
* using: ASM XFER=XFER Jeff Tranter <tranter@pobox.com>

*       XFER - FILE TRANSFER PROGRAM
*
*       THIS PROGRAM DEMONSTRATES READING AND WRITING
*       DISK FILES UNDER HDOS.  FILES ARE READ AND
*       WRITTEN IN LARGE BLOCKS.

*       >XFER dev:FILENAME.EXT TO dev:
*
*       WHERE dev: IS A DRIVE DESIGNATIONS.

*       BY P. SWAYNE, HUG  26-OCT-83

*       HDOS DEFINITIONS

.EXIT   EQU     0
.READ   EQU     4
.WRITE  EQU     5
.LOADO  EQU     10Q
.OPENR  EQU     42Q
.OPENW  EQU     43Q
.CLOSE  EQU     46Q
.SETTP  EQU     52Q
.DECODE EQU     53Q
.ERROR  EQU     57Q
$TYPTX  EQU     31136A
EC.EOF  EQU     1               END OF FILE FLAG
EC.NEM  EQU     21Q             NOT ENOUGH MEMORY

*       MAIN PROGRAM

START   LXI     H,0             LOCATE STACK
        DAD     SP
        MOV     A,L
        CPI     200Q            HAS IT MOVED?
        JNZ     GOTFILE         IF SO, CONTINUE
ERREX   CALL    $TYPTX
        DB      10,'The correct use of this program is',10,10
        DB      '>XFER dev:FILENAME.EXT TO dev:',10,10
        DB      'where dev: is a drive designation '
        DB      '(SY1:, DK0:, etc.),',10
        DB      'and FILENAME.EXT is file you want to XFER.'
        DB      10+80H
        XRA     A
        SCALL   .EXIT           EXIT TO HDOS
GOTFILE PUSH    H               SAVE ARGUMENT ADDRESS
        XRA     A
        SCALL   .LOADO          LOAD FIRST HDOS OVERLAY
        JC      ERROR
        LXI     H,-1
        SCALL   .SETTP          FIND MEMORY LIMIT
        PUSH    H               SAVE IT
        LXI     D,-BUFFER
        DAD     D               CALCULATE FREE SPACE
        MVI     L,0             MAKE IT A PAGE MULTIPLE
        MOV     A,H
        ORA     A               ANY MEMORY THERE?
        MVI     A,EC.NEM        ASSUME NOT
        JZ      ERROR           NO MEMORY, EXIT
        SHLD    MSIZE           ELSE, SAVE MEMORY SIZE
        POP     H               RESTORE MEMORY TOP
        SCALL   .SETTP          SET IT
        JC      ERROR
        POP     H               GET ARGUMENT ADDRESS
        CALL    SOS             SKIP OVER SPACES
        PUSH    H               SAVE INPUT FILE NAME
FNS     MOV     A,M             GET A CHARACTER
        CPI     ' '             SPACE?
        JZ      FNDSP           FOUND SPACE
        INX     H               ELSE, INCREMENT POINTER
        JMP     FNS             AND FIND NEXT SPACE
FNDSP   CALL    SOS             SKIP OVER IT
        MOV     A,M
        CPI     'T'             LOOK FOR 'TO'
        JNZ     ERREX           EXIT IF NOT FOUND
        INX     H
        MOV     A,M
        CPI     'O'
        JNZ     ERREX
        INX     H
        MOV     A,M
        CPI     ' '
        JNZ     ERREX
        CALL    SOS             FOUND "TO", SKIP TO DEV.
        LXI     D,OUTDEF        POINT TO OUTPUT DEFAULTS
        CALL    MOVE3           MOVE DEVICE NAME THERE
        POP     H               GET FILE NAME ADDRESS
        LXI     B,DECTBL        POINT TO DECODE TABLE
        LXI     D,INDEF         AND DEFAULT BLOCK
        SCALL   .DECODE         DECODE FILE NAME
        JC      ERROR           BAD NAME
        LDA     DEV+2           GET UNIT NO.
        ORI     '0'             MAKE IT ASCII
        STA     DEV+2
        LXI     H,DEV           POINT TO DECODED DEVICE
        LXI     D,INDEF         PUT IT HERE
        CALL    MOVE3
        LXI     H,EXT           POINT TO DECODED EXTENSION
        PUSH    H               SAVE ADDR
        LXI     D,INDEF+3       PUT IT HERE
        CALL    MOVE3
        POP     H               GET DECODED EXTENSION
        LXI     D,OUTDEF+3      PUT IT HERE, TOO
        CALL    MOVE3
        XRA     A
        STA     EXT             ENSURE NAME TERMINATED
        XRA     A
        LXI     H,NAME
        LXI     D,INDEF
        SCALL   .OPENR          OPEN INPUT FILE
        JC      ERROR
        MVI     A,1
        LXI     H,NAME
        LXI     D,OUTDEF
        SCALL   .OPENW          OPEN OUTPUT FILE
        JC      ERROR

*       COPY LOOP

LOOP    LHLD    MSIZE           GET MEMORY SIZE
        MOV     B,H
        MOV     C,L             INC BC
        LXI     D,BUFFER        POINT TO BUFFER
        XRA     A               CHANNEL ZERO
        SCALL   .READ           READ INPUT FILE
        JNC     GOODRD          READ OK
        CPI     EC.EOF          END OF FILE?
        JNZ     ERROR           NO, ERROR
        STA     LASTFLG         ELSE, FLAG LAST SEGMENT
        XCHG                    HL = NEXT UNUSED ADDRESS
        LXI     D,-BUFFER
        DAD     D               CALCULATE AMOUNT READ
        SHLD    MSIZE           FIX MSIZE
GOODRD  LHLD    MSIZE           GET AMOUNT TO WRITE
        MOV     B,H
        MOV     C,L             IN BC
        LXI     D,BUFFER        POINT TO BUFFER
        MVI     A,1             CHANNEL 1
        SCALL   .WRITE          WRITE OUTPUT FILE
        JC      ERROR
        LDA     LASTFLG
        ORA     A               LAST SEGMENT?
        JZ      LOOP            IF NOT LOOP

*       COPY DONE, CLOSE FILES

        XRA     A
        SCALL   .CLOSE          CLOSE INPUT FILE
        MVI     A,1
        SCALL   .CLOSE          CLOSE OUTPUT FILE
        JC      ERROR
        CALL    $TYPTX
        DB      10,'DONE!',10+80H
        XRA     A
        SCALL   .EXIT

*       PROCESS ERRORS

ERROR   PUSH    PSW             SAVE ERROR CODE
        CALL    $TYPTX
        DB      10,'ERROR --',' '+80H
        POP     PSW
        MVI     H,7             LET .ERROR RING BELL
        SCALL   .ERROR
        XRA     A
        SCALL   .EXIT

*       SUBROUTINES

*       SKIP OVER SPACES

SOS     MOV     A,M             GET A CHARACTER
        CPI     ' '             SPACE?
        RNZ                     IF NOT, RETURN
        INX     H               ELSE, MOVE TO NEXT CHAR
        JMP     SOS             TRY AGAIN

*       MOVE THREE BYTES FROM (HL) TO (DE)
*       IF ZERO ENCOUNTERED, TERMINATE MOVE

MOVE3   MVI     B,3             SET A COUNTER
MOVE31  MOV     A,M             GET A BYTE
        ORA     A               ZERO?
        RZ                      IF SO, RETURN
        STAX    D               ELSE, STORE BYTE
        INX     H               INCREMENT POINTERS
        INX     D
        DCR     B
        JNZ     MOVE31          LOOP UNTIL DONE
        RET

*       DATA AREA

MSIZE   DW      0               BUFFER MEMORY SIZE
LASTFLG DB      0               LAST SEGMENT FLAG
INDEF   DB      'SY0',0,0,0     INPUT DEFAULT
OUTDEF  DB      'SY0',0,0,0     OUTPUT DEFAULT
DECTBL  DB      0               DECODE TABLE START
DEV     DS      3               DECODED DEVICE
NAME    DS      8               DECODED NAME
EXT     DS      3               DECODED EXTENSION
        DS      4               RESERVED DECODE BYTES
BUFFER  EQU     *               BUFFER STARTS HERE

        END     START
