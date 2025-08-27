* File copy program example taken from the article "Getting Started
* With Assembly Language Part IX" by Pat Swayne in volume 5 issue 2
* (February 1984) of REMark magazine. It can be assembled on HCP/M
* using:
* ASM COPY=COPY
* LOAD COPY
* Tested under CP/M 2.2.
* Jeff Tranter <tranter@pobox.com>

*       COPY - FILE TRANSFER PROGRAM
*
*       THIS PROGRAM DEMONSTRATES READING AND WRITING
*       DISK FILES UNDER CP/M.  FILES ARE READ AND
*       WRITTEN IN LARGE BLOCKS.
*
*       THIS PROGRAM COPIES FILES USING THE FOLLOWING
*       SYNTAX:

*       A>COPY d:FILENAME.EXT TO d:
*
*       WHERE d: IS A DRIVE DESIGNATION.

*       BY P. SWAYNE, HUG  14-NOV-83

*       CP/M DEFINITIONS

CONIN   EQU     1
CONOUT  EQU     2
OPEN    EQU     15
CLOSE   EQU     16
DELETE  EQU     19
READ    EQU     20
WRITE   EQU     21
MAKE    EQU     22
SETDMA  EQU     26
CURDSK  EQU     4
BDOS    EQU     5
DFCB    EQU     5CH
DMA     EQU     80H

        ORG     100H

*       MAIN PROGRAM

START   LXI     H,0
        DAD     SP              ;LOCATE STACK
        LXI     SP,STACK        ;SET NEW ONE
        PUSH    H               ;SAVE OLD ONE
        LDA     DFCB+1
        CPI     ' '             ;CHECK FOR ENTRY
        JNZ     GOTFILE         ;IF SO, CONTINUE
ERREX   CALL    TYPTX
        DB      13,10,'The correct use of this program is',13
        DB      10,10,'A>COPY d:FILENAME.TYPE TO d:',13,10,10
        DB      'where d: is a drive designation '
        DB      '(A:, B:, etc.),',13,10
        DB      'and FILENAME.TYPE is a file you want to COPY.'
        DB      13,10+80H
EXIT    POP     H               ;GET OLD STACK
        SPHL                    ;SET IT
        RET                     ;RETURN TO CP/M
GOTFILE LHLD    BDOS+1          ;GET BDOS ADDRESS
        LXI     D,-886H         ;SUBTRACT CCP, SER., AND REC. SIZE
        DAD     D               ; TO FIND MEMORY LIMIT
        SHLD    MEMTOP          ;SAVE MEMORY TOP
        MVI     A,BUFFER/256    ;GET BUFFER ADDR HIGH
        SUB     H               ;ANY MEMORY AVAILABLE?
        JC      GOTMEM          ;YES, GO AHEAD
        CALL    TYPTX
        DB      13,10,'ERROR - Not enough memory.',13,10+80H
        JMP     EXIT            ;RETURN TO CP/M
GOTMEM  LXI     H,DFCB+1        ;POINT TO FCB
        LXI     D,OUTFCB+1      ;AND OUTPUT FCB
        MVI     B,15            ;MOVE 15 CHARACTERS
MOVFCB  MOV     A,M
        STAX    D               ;MOVE THE FILE NAME
        INX     H
        INX     D
        DCR     B
        JNZ     MOVFCB
        LXI     H,DMA+1         ;POINT TO COMMAND ARGUMENT
        CALL    SOS             ;SKIP OVER SPACES
FNS     MOV     A,M             ;GET A CHARACTER
        CPI     ' '             ;SPACE?
        JZ      FNDSP           ;FOUND SPACE
        INX     H               ;ELSE, INCREMENT POINTER
        JMP     FNS             ;AND FIND NEXT SPACE
FNDSP   CALL    SOS             ;SKIP OVER IT
        CPI     'T'             ;LOOK FOR 'TO'
        JNZ     ERREX           ;EXIT IF NOT FOUND
        INX     H
        MOV     A,M
        CPI     'O'
        JNZ     ERREX
        INX     H
        MOV     A,M
        CPI     ' '
        JNZ     ERREX
        CALL    SOS             ;FOUND "TO", SKIP TO DRIVE NAME
        SUI     '@'             ;REMOVE ASCII FROM DRIVE CODE
        STA     OUTFCB          ;AND SET UP OUTPUT FCB
        MOV     B,A             ;SAVE DRIVE CODE
        LXI     D,DFCB
        LDAX    D
        ORA     A               ;INPUT DRIVE DEFAULT?
        JNZ     NOTDEF          ;NO
        LDA     CURDSK          ;ELSE GET CURRENT DISK
        INR     A               ;MAKE IT START WITH 1
NOTDEF  CMP     B               ;COMPARE INPUT AND OUTPUT DRIVES
        JNZ     NOTSAM          ;NOT THE SAME
        CALL    TYPTX
        DB      13,10,'ERROR - Same drives.',13,10+80H
        JMP     EXIT
NOTSAM  MVI     C,OPEN
        CALL    BDOS            ;OPEN INPUT FILE
        INR     A
        JNZ     GDOPEN          ;OPEN OK
        CALL    TYPTX
        DB      13,10,'ERROR - File not found.',13,10+80H
        JMP     EXIT            ;RETURN TO CP/M
GDOPEN  LXI     D,OUTFCB
        MVI     C,OPEN
        CALL    BDOS            ;TRY TO OPEN OUTPUT
        INR     A               ;ANY FILE?
        JZ      GDOUT           ;NO, GO AHEAD
        CALL    TYPTX
        DB      13,10,'File exists, erase? (Y/N) <N>'
        DB      ' '+80H
        MVI     C,CONIN
        CALL    BDOS            ;GET INPUT
        CPI     'Y'             ;CHECK FOR 'Y'
        JZ      ERAFIL          ;GOT IT, ERASE FILE
        JMP     EXIT            ;ELSE, RETURN TO CP/M
ERAFIL  LXI     D,OUTFCB
        MVI     C,DELETE
        CALL    BDOS            ;DELETE OLD FILE
GDOUT   LXI     D,OUTFCB
        MVI     C,MAKE
        CALL    BDOS            ;MAKE NEW DIRECTORY ENTRY
        INR     A               ;TEST
        JNZ     LOOP            ;OK
        CALL    TYPTX
        DB      13,10,'ERROR - No directory space.',13,10+80H
        JMP     EXIT

*       COPY LOOP

LOOP    LXI     H,0
        SHLD    RECCNT          ;CLEAR RECORD COUNTER
        LXI     D,BUFFER        ;POINT TO BUFFER
READLP  PUSH    D               ;SAVE POINTER
        MVI     C,SETDMA
        CALL    BDOS            ;SET DMA ADDRESS
        LXI     D,DFCB
        MVI     C,READ
        CALL    BDOS            ;READ FROM FILE
        POP     D               ;RESTORE POINTER
        ORA     A               ;TEST READ OPERATION
        JZ      NOTEND          ;END NOT FOUND YET
        STA     LASTFLG         ;ELSE, SET LAST PART FLAG
        JMP     WRFILE          ;AND WRITE THE FILE
NOTEND  LXI     H,80H
        DAD     D               ;UPDATE POINTER
        XCHG                    ;RETURN IT TO DE
        LHLD    RECCNT
        INX     H               ;COUNT RECORD READ
        SHLD    RECCNT
        LHLD    MEMTOP          ;GET MEMORY TOP
        MOV     A,L
        SUB     E               ;SUBTRACT POINTER FROM IT
        MOV     A,H
        SBB     D
        JC      WRFILE          ;NO ROOM, WRITE FILE
        JMP     READLP          ;ELSE READ MORE
WRFILE  LHLD    RECCNT
        MOV     A,H
        ORA     L               ;EMPTY FILE COPIED?
        JZ      DONE            ;IF SO, WE'RE DONE!
        LXI     D,BUFFER        ;RESET POINTER
WRITLP  PUSH    D               ;SAVE POINTER
        MVI     C,SETDMA
        CALL    BDOS            ;SET DMA ADDRESS
        LXI     D,OUTFCB
        MVI     C,WRITE
        CALL    BDOS            ;WRITE FILE
        POP     D               ;RESTORE POINTER
        ORA     A               ;GOOD WRITE
        JZ      GDWRIT          ;YES
        CALL    TYPTX
        DB      13,10,'ERROR -- No disk space.',13,10+80H
        JMP     EXIT
GDWRIT  LXI     H,80H
        DAD     D               ;ELSE, UPDATE POINTER
        XCHG
        LHLD    RECCNT
        DCX     H               ;DECREMENT RECORD COUNTER
        SHLD    RECCNT
        MOV     A,H
        ORA     L               ;ALL DONE?
        JNZ     WRITLP          ;IF NOT, CONTINUE
        LDA     LASTFLG
        ORA     A               ;END OF FILE?
        JZ      LOOP            ;IF NOT, GET MORE

*       COPY DONE, CLOSE OUTPUT FILE

DONE    LXI     D,OUTFCB
        MVI     C,CLOSE
        CALL    BDOS            ;CLOSE FILE
        INR     A
        JNZ     GDCLOSE         ;GOOD CLOSE OPERATION
        CALL    TYPTX
        DB      10,10,'ERROR -- Can''t close file.',13,10+80H
        JMP     EXIT
GDCLOSE CALL    TYPTX
        DB      13,10,'DONE!',13,10+80H
        JMP     EXIT

*       SUBROUTINES

*       SKIP OVER SPACES

SOS     MOV     A,M             ;GET A CHARACTER
        CPI     ' '             ;SPACE?
        RNZ                     ;IF NOT, RETURN
        INX     H               ;ELSE, MOVE TO NEXT CHAR
        JMP     SOS             ;TRY AGAIN

*       TYPE TEXT FOLLOWING CALL

TYPTX   XTHL                    ;SAVE HL, GET TEXT ADDR
TYPTX1  MOV     A,M             ;GET CHARACTER
        PUSH    H               ;SAVE POINTER
        MVI     C,CONOUT
        ANI     7FH             ;STRIP MARKER BIT
        MOV     E,A             ;CHARACTER TO E
        CALL    BDOS            ;PRINT IT
        POP     H               ;RESTORE POINTER
        MOV     A,M             ;GET CHARACTER AGAIN
        INX     H               ;INCREMENT POINTER
        ORA     A               ;TEST FOR END
        JP      TYPTX1          ;NOT THERE, YET
        XTHL                    ;ELSE, FIX STACK, GET HL
        RET

*       DATA AREA

MEMTOP  DW      0               ;MEMORY TOP
RECCNT  DW      0               ;RECORD COUNTER
LASTFLG DB      0               ;LAST SEGMENT FLAG
OUTFCB  DB      0,'           ',0,0,0,0
        DS      16
        DB      0
        DS      32              ;RESERVE STACK SPACE
STACK   EQU     $
BUFFER  EQU     $               ;BUFFER STARTS HERE

        END     START
