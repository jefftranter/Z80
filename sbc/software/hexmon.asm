; This code came from chapter 9, "Paper Tape and Magnetic Tape
; Routines" from the book "8080/Z80 Assembly Language - Techniques for
; Improved Programming" by Alan R. Miller
;
; It was modified by Jeff Tranter <tranter@pobox.com> to assemble with
; the z80asm cross-assembler and to run on my Z80-based Single Board
; Computer and converted from 8080 to Z80 mnemonics.

; HEXMON: A MONITOR TO DUMP, LOAD, AND
;      VERIFY INTEL HEX CHECKSUM TAPES
;      WITH TAPE LABEL FOR HEADER
;
;       TITLE   'hexmon with tlabel'
;
 ORG 100H
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
RLEN:   EQU     16              ;RECORD LENGTH
;
CSTAT:  EQU     10H             ;CONSOLE STATUS
CDATA:  EQU     CSTAT+1         ;CONSOLE DATA
CIMSK:  EQU     1               ;IN MASK
COMSK:  EQU     2               ;OUT MASK
PSTAT:  EQU     6               ;PUNCH STATUS
PDATA:  EQU     PSTAT+1         ;PUNCH DATA
PIMSK:  EQU     1               ;PUNCH IN MASK
POMSK:  EQU     80H             ;PUNCH OUT MASK
;
CR:     EQU     13              ;CARRIAGE RETURN
LF:     EQU     10              ;LINE FEED
DEL:    EQU     127             ;DELETE
CTRH:   EQU     8               ;^H CONSOLE BACKUP
NNULS:  EQU     0               ;CONSOLE NULLS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
START:  JP      CONTIN
;
; INPUT A BYTE FROM TAPE CONSOLE
;
INPUTT: IN      A,(CSTAT)
        AND     CIMSK
        JP      Z,INPUTT
        IN      A,(CDATA)
        AND     7FH             ;STRIP PARITY
        RET
;
OUTT:   PUSH    PSW
OUTW:   IN      CSTAT
        ANI     COMSK
        JZ      OUTW
        POP     PSW
        OUT     CDATA
        RET
; OUTPUT H,L TO CONSOLE
; 16-BT BINARY TO HEX
;
OUTHL:  MOVE    C,H             ;FETCH H
        CALL    OUTHX           ;PRINT IT
        MOVE    C,L             ;FETCH L, PRINT IT
;
; CONVERT A BINARY NUMBER TO TWO
; HEX CHARACTERS, AND PRINT THEM
OUTHX:  MOVE    A,C
        RAR                     ;ROTATE
        RAR                     ; UPPER
        RAR                     ; CHARACTER
        RAR                     ; TO LOWER
        CALL    HEX1            ;OUTPUT UPPER
        MOVE    A,C             ;OUTPUT LOWER
;
; OUTPUT A HEX CHARACTER
; FROM LOWER FOUR BITS
;
HEX1:   ANI     0FH             ;TAKE 4 BITS
        ADDI    144
        DAA                     ;DAA TRICK
        ACI     64
        DAA                     ;ONCE AGAIN
        JMP     OUTT
;
; CONVERT ASCII CHARACTER FROM CONSOLE
; TO 4 BINARY BITS
NIB:    SUI     '0'             ;ASCII BIAS
        RC                      ;< '0'
        CPI     'F'-'0'+1
        CMC                     ;INVERT
        RC                      ;ERROR, IF > 'F'
        CPI     10
        CMC                     ;INVERT
        SUI     'A'-'9'-1
        CPI     10
        RET                     ;CHARACTER IS A-F
;
; INPUT H,L FROM CONSOLE
;
READHL: PUSH    D
        PUSH    B
        LXI     H,0             ;START WITH 0
RDHL2:  CALL    GETCH
        JC      RDHL5           ;END OF LINE
        CALL    NIB             ;CONVERT TO BINARY
        JC      RDHL4           ;NOT HEX
        DAD     H               ; SHIFT LEFT
        DAD     H
        DAD     H
        DAD     H
        ORA     L               ;COMBINE NEW
        MOVE    L,A
        JMP     RDHL2           ;NEXT
;
; CHECK FOR COMMA OR BLANK
; AT END OF ADDRESS
;
RDHL4:  CPI     ','-'0'         ;COMMA?
        JZ      RDHL5           ;YES, OK
        CPI     ' ' -'0'        ;BLANK?
        JNZ     ERROR           ;NO
RDHL5:  POP     B
        POP     D
        RET
;
ERROR:  MVI     A,'?'           ;IMPROPER INPUT
        CALL    OUTT
        JMP     START           ;TELL HOW AGAIN
;
; SEND CHARACTERS POINTED TO BY D,E
; UNTIL A BINARY ZERO IS FOUND
;
SENDM:  LDAX    D               ;NEXT BYTE
        ORA     A               ;SEE IF ZERO
        RZ
        CALL    OUTT
        INX     D
        JMP     SENDM
;
CONTIN: LXI     SP,STACK
        LXI     D,SIGN          ;MESSAGE
        CALL    SENDM           ;SEND IT
;
RSTRT:  LXI     SP,STACK
        CALL    INPLN           ;GET A LINE
        CALL    GETCH           ;INPUT THE TASK
        CPI     'W'             ;DUMP
        JZ      PDUMP
        CPI     'R'             ;READ,NO AUTOSTART
        JZ      PLOAD
        CPI     'E'             ;LOAD AND EXECUTE
        JZ      PLOAD
        CPI     'V'             ;VERIFY
        JZ      PLOAD
        CPI     'G'             ;GO SOMEWHERE
        JNZ     ERROR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; JUMP TO ANOTHER PROGRAM
;
        CALL    READHL          ;JUMP ADDRESS
JPCHL:  PCHL                    ;OK, GOODBYE
;
; INPUT A LINE FROM THE CONSOLE
; AND PUT IT INTO A BUFFER
;
INPLN:  CALL    CRLF
        MVI     A,'>'           ;COMMAND PROMPT
        CALL    OUTT            ;SEND TO CONSOLE
INPL2:  LXI     H,IBUFF         ;BUFFER ADDRESS
        SHLD    IBUFP           ;INITIALIZE POINTER
        MVI     C,0             ;INITIALIZE COUNT
INPLI:  CALL    INPUTT          ;CHAR FROM CONSOLE
        CPI     ' '             ;CONTROL CHAR?
        JC      INPLC           ;YES, GO PROECSS
        CPI     DEL             ;DELETE CHAR?
        JZ      INPLB           ;YES
        CPI     'Z'+1           ;UPPER CASE?
        JC      INPL3           ;NO
        ANI     5FH             ;MAKE UPPER
INPL3:  MOVE    M,A             ;PUT IN BUFFER
        MVI     A,32            ;GET BUFFER SIZE
        CMP     C               ;TEST IF FULL
        JZ      INPLI           ;YES, LOOP
        MOV     A,M             ;RECALL CHARACTER
        INX     H               ;INCR POINTER
        INR     C               ;AND INCR COUNT
INPLE:  CALL    OUTT            ;OUTPUT CHARACTER
        JM      INPL            ;GET NEXT CHAR
;
; PROCESS CONTROL CHARACTER
;
INPLC:  CPI     CTRH            ;^H?
        JZ      INPLB           ;YES
        CPI     CR              ;TEST IF RETURN
        JNZ     INPLI           ;NO, IGNORE CHAR
;
; END OF INPUT LINE
;
        MOV     A,C             ;LINE COUNT
        STA     IBUFC           ;SAVE
;
; CARRIAGE RETURN, LINE FEED
;
CRLF:   MVI     A,CR
        CALL    OUTT
        MVI     A,LF
;
; USE NULLS REQUIRED AFTER  CR, LF
; USE REPEAT MACRO
        IF NULS > 0             ;ASSEMBLE
        CALL    OUTT
        XRA     A               ;GET A NULL
        REPT    NNULS-1
        CALL    OUTT
        ENDM
        ENDIF
;                               ;NULLS
        JMP     OUTT
;
; DELETE ANY PRIOR CHARACTER
;
INPLS:  MOV     A,C             ;CHAR COUNT
        ORA     A               ;ZERO?
        JZ      INPLI           ;YES
        DCX     H               ;BACK POINTER
        DCR     C               ;AND COUNT
        MVI     A,CTRH          ;BACK CURSOR
        JMP     INPLE           ;PRINT IT
;
; OBTAIN A CHARACTER FROM THE CONSOLE
; BUFFER. SET CARRY IF EMPTY.
;
GETCH:  PUSH    H               ;SAVE REGS
        LHLD    IBUFP           ;GET POINTER
GETCH2: LDA     IBUFC           ;GET COUNT
        SUI     1               ;DECR WITH CARRY
        JC      GETC4           ;NO CHARACTERS
        STA     IBUFC           ;REPLACE COUNT
        MOV     A,M             ;GET CHARACTER
GETCH3: INX     H               ;INCR POINTER
        SHLD    IBUFP           ;REPLACE POINTER
GETCH4: POP     H               ;RESTORE REGS
        RET                     ;CARRY IF NO CHAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PUNCH A PAPER TAPE
;
PDUMP:  CALL    READHL          ;START ADDRESS
        JC      ERROR           ;TOO FEW PARAM
        XCHG
        CALL    READHL          ;STOP ADDRESS
        XCHG
        INX     D
        PUSH    H
        CALL    READHL          ;AUTOSTART ADDR
        XTHL                    ;PUT ON STACK
        CALL    LEADR           ;PUNCH LEADER
        CALL    LABEL           ;PUNCH LABEL
;
; START NEW RECORD, ZERO THE CHECKSUM
; PUNCH CR, LF, 2 NULLS AND COLON
;
NEWREC: CALL    PCRLF           ;CR, LF, NULLS
;
; FIND THE RECORD LENGTH
;
        MOV     A,E             ;COMPARE LOW STOP
        SUB     L               ; TO LOW POINTER
        MOV     C,A             ;DIFFERENCE IN C
        MOV     A,D             ;COMPARE HIGH STOP
        SBB     H               ; TO HIGH POINTER
        MOV     B,A             ;DIFFERENCE IN B
        JC      ERROR           ;IMPROPER H,L > D,E
        MVI     A,RLEN          ;FULL RECORD
        JNZ     NEW2
        CMP     C               ;COMPARE TO E-L
        JC      NEW2            ;FULL RECORD LENGTH
        MOV     A,B             ;ARE BOTH E-L AND
        ORA     C               ; D-E ZERO?
        JZ      DONE            ;YES, REC LENGTH = 0
        MOV     A,C             ;SHORT RECORD
NEW2:   MOV     C,A             ;RECORD LENGTH TO C
        MVI     B0              ;ZERO THE CHECKSUM
        CALL    PNHEX           ;PUNCH RECORD LENGTH
        CALL    PUNHL           ;PUNCH HL
        XRA     A
        CALL    PNHEX           ;PUNCH RECORD TYPE 0
PMEM:   MOV     A,M
        CALL    PNHEX           ;PUNCH MEMORY BYTE
        INX     H               ;INCR. MEMORY POINTER
        DCR     C               ;DECR RECORD LENGTH
        JNZ     PMEM
        CALL    CSUM            ;PUNCH CHECKSUM
        JMP     NEWREC          ;NEXT RECORD
;
; FINSIHED, PUNCH LAST RECORD, RECORD
; LENGTH 00, THE START ADDRESS,
; AND A RECORD TYPE OF 01
;
DONE:   XRA     A
        MOV     B,A             ;ZERO CHECKSUM
        CALL    PNHEX           ;ZERO RECORD LEN.
        POP     H
        CALL    PUNHL           ;AUTOSTART H/L
        MOV     A,H             ;CHECK FOR
        ORA     L               ; AUTOSTART
        MV      A,0             ;0 WITH CARRY
        JZ      DON2            ;NO AUTOSTART
        INR     A
DON2:   CALL    PNHEX           ;RECORD TYPE 1
        CALL    CSUM            ;PUNCH CHECKSUM
        CALL    LEADR           ;PUNCH TRAILER
        JMP     RSTRT           ;NEXT JOB
;
; PUNCH BLANK HEADER AND TRAILER
;
LEADR:  XRA     A
        MVI     B,60            ;TAPE NULLS
NLDR:   CALL    POUT
        DCR     B
        JNZ     NLDR
        RET
;
; PUNCH THE H,L REGISTER PAIR
;
PUNHL:  MOV     A,H             ;FETCH H
        CALL    PNHEX           ;PUNCH IT
        MOV     A,L             ;GET L, PUNCH IT
;
; CONVERT A BINARY NUMBER TO TWO HEX
; CHARACTERS, PUNCH THEM, ADD TO CHECKSUM
;
PNHEX:  PUSH    PSW             ;SAVE ON STACK
        ADD     B               ;ADD TO CHECKSUM
        MOV     B,A             ;SAVE IT IN B
        POP     PSW             ;RETRIEVE BYTE
        PUSH    PSW
        RAR
        RAR                     ;ROTATE UPPER
        RAR
        RAR                     ;TO LOWER
        CALL    PHEX1           ;LEFT CHARACTER
        POP     PSW             ;RIGHT CHARACTER
;
; PUNCH A HEX CHARACTER FROM
; LOWER FOUR BITS
;
PHEX1:  ANI     OFH             ;MASK UPPER 4 BITS
        ADI     144
        DAA
        ACI     64
        DAA
        JMP     POUT
;
; INPUT A HEX CHARACTER FROM TAPE
;
HEX:    CALL    PIN
        SUI     '0'
        CPI     10
        RC                      ;0-9
        SUI     7               ;A-F
        RET
;
; OUTPUT A BYTE TO THE PUNCH
;
POUT:   PUSH    PSW
PUTW:   IN      PSTAT
        ANI     POMSK
        JNZ     POUTW
        POP     PSW
        OUT     PDATA
        RET
;
; INPUT A BYTE FROM PAPER TAPE
;
PIN:    IN      PSTAT
        ANI     PIMSK
        JNZ     PIN
        IN      PDAT
        ANI     7FH             ;STRIP PARITY
        RET
;
; PUNCH CR, LF, NULLS AND COLON
;
PCRLF:  MVI     A,CR
        CALL    POUT
        MOVI    A,LF
        CALL    POUT
        XRA     A
        CALL    POUT
        XRA     A
        CALL    POUT            ;TWO NULLS
        CALL    POUT
        MVI     A,':'           ;COLON
        JMP     POUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ENTRY FOR LOAD, EXECUTE AND VERIFY
;
PLOAD:  STA     TASK
        MVI     A,17            ;TAPE READER ON
        CALL    POUT
        CALL    READHL          ;OFFSET
        SHLD    OFSET           ;SAVE IT
;
; PROCESS THE RECORD HEADING ON INPUT
;
HEAD:   CALL    PIN             ;INPUT FROM TAPE
        CPI     ':'             ;COLON?
        JNZ     HEAD            ;NO, TRY AGAIN
        MVI     B,0             ;ZERO THE CHECKSUM
        CALL    PHEX            ;RECORD LENGTH
        ORA     A               ;IS IT ZERO?
        JZ      ENDFL           ;YES, DONE
        MOV     C,A             ;SAVE REC.LEN.
        CALL    TAPEHL          ;GET H/L
        XCHG                    ;ADDR TO D,E
        LHLD    OFSET           ;GET OFFSET
        DAD     D               ;ADD
LOOP:   CALL    PHEX            ;INPUT DATA BYTE
        MOV     E,A             ;SAVE BYTE
        LDA     TASK            ;GET TASK
        CPI     'V'             ;SEE IF VERIFYING
        MOV     A,E             ;MOVE BACK
        JZ      SKIP            ;JUMP IF VERIFYING
        MOV     M,A             ;DATA TO MEMORY
SKIP:   CMP     M               ;CHECK MEMORY
        JNZ     MERROR          ;BAD MEMORY
        INX     H               ;INCREMENT POINTER
        DCR     C               ;DECR RECORD LEN
        JNZ     LOOP            ;NOT YET ZERO
        CALL    CHECK           ;PROCESS CHECKSUM
        JMP     HEAD            ;START NEXT RECORD
;
; INPUT H,L AND RECORD TYPE FROM TAPE
;
TAPEHL: CALL    PHEX            ;READ H
        MOV     H,A
        CALL    PHEX            ;READ L
        MOV     L,A             ;READ RECORD TYPE
;
; CONVERT 2 CHAR FROM TAPE TO ONE BINARY
; WORD, STORE IN A AND ADD TO CHECKSUM
;
PHEX:   CALL    HEX             ;UPPER CHARACTER
        RLC
        RAL                     ;MOVE TO UPPER
        RAL
        RAL
        MOV     E,A             ;SAVE IT
        CALL    HEX             ;LOWER CHARACTER
        ADD     E               ;COMBINE BOTH
        MOV     E,A             ;SAVE IT
        ADD     B               ;ADD TO CHECKSUM
        MOV     B,A             ;SAVE T
        MOV     A,E             ;RETRIEVE DATA
        RET
;
; ROUTINE TO CHECK FOR AUTOSTART
;
ENDFL:  CALL    TAPEHL          ;AUTOSTART ADDRESS
                                ;AND RECORD TYPE
        PUSH    PSW             ;SAVE RECORD TYPE
        CALL    PHEX            ;INPUT CHECKSUM
        CALL    TOFF            ;TAPE READER OFF
        POP     PSW             ;RETRIEVE REC TYPE
        CPI     1               ;AUTOSTART?
        JNZ     RSTRT           ;NO
        LDA     TASK            ;CHECK TASK
        CPI     'E'             ;EXECUTE?
        JZ      JPCHL           ;YES, GO THERE
        CALL    OUTHL           ;NO, PRINT HL
        JMP     RSTRT           ;NEXT TASK
;
;TURN OFF TAPE READER
;
TOFF:   MVI     A,19
        JMP     POUT
;
; CALCULATE AND PUNCH THE CHECKSUM
;
CSUM:   MOVE    A,B             ;CHECKSUM TO A
        CMA                     ;ONE'S COMPLEMENT
        INR     A               ;TWO'S COMPLEMENT
        JMP     PNHEX           ;PUNCH CHECKSUM
;
; SEE IF CHECKSUM IS CORRECT (ZERO)
;
CHECK:  CALL    PHEX            ;INPUT CHECKSUM
        XRA     A
        ADD     B               ;IS CHECKSUM ZERO?
        RZ                      ;YES, RETURN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERROR MESSAGES
;
        MVI     A,'C'           ;CHECKSUM ERROR
        DB      1               ;DB TRICK TO SKIP
MERROR: MVI     A,'M'           ;M FOR BAD MEMORY
        PUSH    PSW
        CALL    TOFF            ;TAPE READER OFF
        POP     PSW
        CALL    OUTT            ;PRINT ERROR TYPE
        CALL    OUTHL           ;PRINT H/L
        JMP     RSTR
;
SGN:    DB      CR,LF
        DB      'Hex paper-tape program'
        DB      CR,LF,LF
        DB      'E - load and execute'
        DB      CR,LF
        DB      'G - go to address given'
        DB      CR,LF
        DF      'R - read tape into memory'
        DB      CR,LF
        DB      '    (with optional offset)"
        DB      CR,LF
        DB      'V - verify tape against'
        DB      ' memory',CR,LF
        DB      'W - write paper tape'
        DB      ' (and label)',CR,LF
        DB      CR,LF,0
MESG:   DB      CR,LF,'Enter leader message'
        DB      CR,LF,0
;
; PUNCH READABLE LABELS ON PAPER TAPE
;
LABEL:  PUSH    H
        PUSH    D
        LXI     D,LMESG         ;LABEL MESS.
        CALL    SENDM           ;SEND IT
        CALL    INPLN           ;GET A LINE
LABL1:  CALL    GETCH           ;GET CHARACTER
        JC      LABL2           ;DONE ON CARRY
        SBI     20H             ;ASCII BIAS
        JC      LABL1           ;< SPACE
        CPI     63
        JNC     LABL1           ;TOO BIG
        MOV     L,A
        MOV     E,A
        MVI     H,0
        MVI     D,0
        DAD     H               ;DOUBLE IT
        DAD     H               ;TIMES 4
        DAD     H               ;TIMES 5
        XCHG
        LXI     H,TABL
        DAD     D
        MVI     C,5
NEXTC:  MOV     A,M
        CALL    POUT
        INX     H
        DCR     C
        JNZ     NEXTC
        XRA     A
        CALL    POUT
        JMP     LABL1           ;NEXT CHARACTER
        CALL    LEADR
        POP     D
        POP     H
        RET
;
        DB      0,  0,  0,  0,  0       ; SPACE
        DB      0,  0,  207,207,0       ; !
        DB      0,  7,  0,  7,  0       ; "
        DB      40, 254,40, 254,40      ; #
        DB      70, 137,255,137,114     ; $
        DB      70, 38, 16, 200,196     ; %
        DB      108,146,172,64, 160     ; &
        DB      0,  4,  3,  3,  0       ; '
        DB      0,  60, 66, 129,0       ; (
        DB      0,  129,66, 60, 9       ; )
        DB      136,80, 248,80, 136     ; *
        DB      8,  8,  126,8,  8       ;+
        DB      0,  128,112,48, 0       ; ,
        DB      8,  8,  8,  8,  8       ;-
        DB      0,  192,192,0,  0       ; .
        DB      64, 32, 16, 8,  4       ; /
        DB      126,161,137,133,126     ; 0
        DB      132,130,255,128,128     ; 1
        DB      194,161,145,137,134     ; 2
        DB      66, 137,137,137,118     ; 3
        DB      12, 10, 137,255,136     ; 4
        DB      103,137,137,137,113     ; 5
        DB      126,137,137,137,114     ; 6
        DB      1,  1,  249,5,  3       ; 7
        DB      118,137,137,137,118     ; 8
        DB      70, 137,137,137,126     ; 9
        DB      0,  216,216,0,  0       ; :
        DB      0,  128,118,54, 0       ; ;
        DB      16, 40, 68, 130,0       ; <
        DB      40, 40, 40, 40, 40      ; =
        DB      130,68, 40, 16, 0       ; >
        DB      6,  1,  15, 9,  6       ; ?
        DB      126,129,157,154,14      ;
        DB      254,9,  9,  9,  254     ; A
        DB      129,255,137,137,118     ; B
        DB      126,129,129,129,66      ; C
        DB      129,255,129,129,126     ; D
        DB      255,137,137,137,137     ; E
        DB      255,9,  9,  9,  1       ; F
        DB      126,129,145,145,114     ; G
        DB      255,8,  8,  8,  255     ; H
        DB      0,  129,255,129,0       ; I
        DB      96, 128,129,127,1       ; J
        DB      255,8,  20, 34, 193     ; K
        DB      255,128,128,128,128     ; L
        DB      255,2,  12, 2,  255     ; M
        DB      255,2,  60, 64, 255     ; N
        DB      255,129,129,129,255     ; O
        DB      5,  9,  9,  9,  6       ; P
        DB      126,129,161,65, 190     ; Q
        DB      255,25, 41, 73, 134     ; R
        DB      70, 137,137,137,114     ; S
        DB      1,  1,  255,1  ,1       ; T
        DB      127,128,128,128,127     ; U
        DB      15, 48, 192,48, 15      ; V
        DB      127,128,112,128,127     ; W
        DB      195,36, 24, 36, 195     ; X
        DB      3,  4,  248,3,  3       ; Y
        DB      193,161,145,137,135     ; Z
        DB      0,  255,129,129,129     ; [
        DB      4,  8,  16, 32, 64      ; \
        DB      129,129,129,255,9       ; [
        DB      12, 2,  1,  2,  12      ; ^
;
TASK:   DS      1               ;SAVE IT
OFSET:  DW      0               ;LOAD OFFSET
        DS      30              ;STACK SPACE
STACK:
IBUFP:  DS      2               ;BUFFER POINTER
IBUFC:  DS      1               ;BUFFER COUNT
IBUFF:  DS      20              ;INPUT BUFFER
;
        END
