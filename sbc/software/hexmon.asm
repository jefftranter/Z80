; This code came from chapter 9, "Paper Tape and Magnetic Tape
; Routines" from the book "8080/Z80 Assembly Language - Techniques for
; Improved Programming" by Alan R. Miller
;
; It was modified by Jeff Tranter <tranter@pobox.com> to assemble with
; the z80asm cross-assembler and to run on my Z80-based Single Board
; Computer and converted from 8080 to Z80 mnemonics.
;
; Commands:
;
; E                             Load and execute
; G<addr>                       Go to address given
; R[<offset>]                   Read tape into memory (with optional offset)
; V                             Verify tape against memory
; W<start>,<end>[,<autostart>]  Write and label paper tape (with optional autostart address)

; HEXMON: A MONITOR TO DUMP, LOAD, AND
;      VERIFY INTEL HEX CHECKSUM TAPES
;      WITH TAPE LABEL FOR HEADER
;
;       TITLE   'hexmon with tlabel'
;
        ORG 1000H
;
;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
RLEN:   EQU     16              ;RECORD LENGTH
;
CSTAT:  EQU     80H             ;CONSOLE STATUS
CDATA:  EQU     CSTAT+1         ;CONSOLE DATA
CIMSK:  EQU     1               ;IN MASK
COMSK:  EQU     2               ;OUT MASK
PSTAT:  EQU     80H             ;PUNCH STATUS
PDATA:  EQU     PSTAT+1         ;PUNCH DATA
PIMSK:  EQU     1               ;PUNCH IN MASK
POMSK:  EQU     2               ;PUNCH OUT MASK
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
OUTT:   PUSH    AF
OUTW:   IN      A,(CSTAT)
        AND     COMSK
        JP      Z,OUTW
        POP     AF
        OUT     (CDATA),A
        RET
;
; OUTPUT H,L TO CONSOLE
; 16-BIT BINARY TO HEX
;
OUTHL:  LD      C,H             ;FETCH H
        CALL    OUTHX           ;PRINT IT
        LD      C,L             ;FETCH L, PRINT IT
;
; CONVERT A BINARY NUMBER TO TWO
; HEX CHARACTERS, AND PRINT THEM
;
OUTHX:  LD      A,C
        RRA                     ;ROTATE
        RRA                     ; UPPER
        RRA                     ; CHARACTER
        RRA                     ; TO LOWER
        CALL    HEX1            ;OUTPUT UPPER
        LD      A,C             ;OUTPUT LOWER
;
; OUTPUT A HEX CHARACTER
; FROM LOWER FOUR BITS
;
HEX1:   AND     0FH             ;TAKE 4 BITS
        ADD     A,144
        DAA                     ;DAA TRICK
        ADC     A,64
        DAA                     ;ONCE AGAIN
        JP      OUTT
;
; CONVERT ASCII CHARACTER FROM CONSOLE
; TO 4 BINARY BITS
;
NIB:    SUB     '0'             ;ASCII BIAS
        RET     C               ;< '0'
        CP      'F'-'0'+1
        CCF                     ;INVERT
        RET     C               ;ERROR, IF > 'F'
        CP      10
        CCF                     ;INVERT
        RET     NC
        SUB     'A'-'9'-1
        CP      10
        RET                     ;CHARACTER IS A-F
;
; INPUT H,L FROM CONSOLE
;
READHL: PUSH    DE
        PUSH    BC
        LD      HL,0            ;START WITH 0
RDHL2:  CALL    GETCH
        JP      C,RDHL5         ;END OF LINE
        CALL    NIB             ;CONVERT TO BINARY
        JP      C,RDHL4         ;NOT HEX
        ADD     HL,HL           ; SHIFT LEFT
        ADD     HL,HL
        ADD     HL,HL
        ADD     HL,HL
        OR      L               ;COMBINE NEW
        LD      L,A
        JP      RDHL2           ;NEXT
;
; CHECK FOR COMMA OR BLANK
; AT END OF ADDRESS
;
RDHL4:  CP      ','-'0'         ;COMMA?
        JP      Z,RDHL5         ;YES, OK
        CP      ' ' -'0'        ;BLANK?
        JP      NZ,ERROR        ;NO
RDHL5:  POP     BC
        POP     DE
        RET
;
ERROR:  LD      A,'?'           ;IMPROPER INPUT
        CALL    OUTT
        JP      START           ;TELL HOW AGAIN
;
; SEND CHARACTERS POINTED TO BY D,E
; UNTIL A BINARY ZERO IS FOUND
;
SENDM:  LD      A,(DE)          ;NEXT BYTE
        OR      A               ;SEE IF ZERO
        RET     Z
        CALL    OUTT
        INC     DE
        JP      SENDM
;
CONTIN: LD      SP,STACK
        LD      DE,SIGN         ;MESSAGE
        CALL    SENDM           ;SEND IT
;
RSTRT:  LD      SP,STACK
        CALL    INPLN           ;GET A LINE
        CALL    GETCH           ;INPUT THE TASK
        CP      'W'             ;DUMP
        JP      Z,PDUMP
        CP      'R'             ;READ,NO AUTOSTART
        JP      Z,PLOAD
        CP      'E'             ;LOAD AND EXECUTE
        JP      Z,PLOAD
        CP      'V'             ;VERIFY
        JP      Z,PLOAD
        CP      'G'             ;GO SOMEWHERE
        JP      NZ,ERROR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; JUMP TO ANOTHER PROGRAM
;
        CALL    READHL          ;JUMP ADDRESS
JPCHL:  JP      (HL)            ;OK, GOODBYE
;
; INPUT A LINE FROM THE CONSOLE
; AND PUT IT INTO A BUFFER
;
INPLN:  CALL    CRLF
        LD      A,'>'           ;COMMAND PROMPT
        CALL    OUTT            ;SEND TO CONSOLE
INPL2:  LD      HL,IBUFF        ;BUFFER ADDRESS
        LD      (IBUFP),HL      ;INITIALIZE POINTER
        LD      C,0             ;INITIALIZE COUNT
INPLI:  CALL    INPUTT          ;CHAR FROM CONSOLE
        CP      ' '             ;CONTROL CHAR?
        JP      C,INPLC         ;YES, GO PROCESS
        CP      DEL             ;DELETE CHAR?
        JP      Z,INPLB         ;YES
        CP      'Z'+1           ;UPPER CASE?
        JP      C,INPL3         ;NO
        AND     5FH             ;MAKE UPPER
INPL3:  LD      (HL),A          ;PUT IN BUFFER
        LD      A,32            ;GET BUFFER SIZE
        CP      C               ;TEST IF FULL
        JP      Z,INPLI         ;YES, LOOP
        LD      A,(HL)          ;RECALL CHARACTER
        INC     HL              ;INCR POINTER
        INC     C               ;AND INCR COUNT
INPLE:  CALL    OUTT            ;OUTPUT CHARACTER
        JP      INPLI           ;GET NEXT CHAR
;
; PROCESS CONTROL CHARACTER
;
INPLC:  CP      CTRH            ;^H?
        JP      Z,INPLB         ;YES
        CP      CR              ;TEST IF RETURN
        JP      NZ,INPLI        ;NO, IGNORE CHAR
;
; END OF INPUT LINE
;
        LD      A,C             ;LINE COUNT
        LD      (IBUFC),A       ;SAVE
;
; CARRIAGE RETURN, LINE FEED
;
CRLF:   LD      A,CR
        CALL    OUTT
        LD      A,LF
;
; USE NULLS REQUIRED AFTER  CR, LF
; USE REPEAT MACRO
;       IF NULS > 0             ;ASSEMBLE
;       CALL    OUTT
;       XRA     A               ;GET A NULL
;       REPT    NNULS-1
;       CALL    OUTT
;       ENDM
;       ENDIF
;                               ;NULLS
        JP      OUTT
;
; DELETE ANY PRIOR CHARACTER
;
INPLB:  LD      A,C             ;CHAR COUNT
        OR      A               ;ZERO?
        JP      Z,INPLI         ;YES
        DEC     HL              ;BACK POINTER
        DEC     C               ;AND COUNT
        LD      A,CTRH          ;BACK CURSOR
        JP      INPLE           ;PRINT IT
;
; OBTAIN A CHARACTER FROM THE CONSOLE
; BUFFER. SET CARRY IF EMPTY.
;
GETCH:  PUSH    HL              ;SAVE REGS
        LD      HL,(IBUFP)      ;GET POINTER
GETCH2: LD      A,(IBUFC)       ;GET COUNT
        SUB     1               ;DECR WITH CARRY
        JP      C,GETCH4        ;NO CHARACTERS
        LD      (IBUFC),A       ;REPLACE COUNT
        LD      A,(HL)          ;GET CHARACTER
GETCH3: INC     HL              ;INCR POINTER
        LD      (IBUFP),HL      ;REPLACE POINTER
GETCH4: POP     HL              ;RESTORE REGS
        RET                     ;CARRY IF NO CHAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; PUNCH A PAPER TAPE
;
PDUMP:  CALL    READHL          ;START ADDRESS
        JP      C,ERROR         ;TOO FEW PARAM
        EX      DE,HL
        CALL    READHL          ;STOP ADDRESS
        EX      DE,HL
        INC     DE
        PUSH    HL
        CALL    READHL          ;AUTOSTART ADDR
        EX      (SP),HL         ;PUT ON STACK
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
        LD      A,E             ;COMPARE LOW STOP
        SUB     L               ; TO LOW POINTER
        LD      C,A             ;DIFFERENCE IN C
        LD      A,D             ;COMPARE HIGH STOP
        SBC     A,H             ; TO HIGH POINTER
        LD      B,A             ;DIFFERENCE IN B
        JP      C,ERROR         ;IMPROPER H,L > D,E
        LD      A,RLEN          ;FULL RECORD
        JP      NZ,NEW2
        CP      C               ;COMPARE TO E-L
        JP      C,NEW2          ;FULL RECORD LENGTH
        LD      A,B             ;ARE BOTH E-L AND
        OR      C               ; D-E ZERO?
        JP      Z,DONE          ;YES, REC LENGTH = 0
        LD      A,C             ;SHORT RECORD
NEW2:   LD      C,A             ;RECORD LENGTH TO C
        LD      B,0             ;ZERO THE CHECKSUM
        CALL    PNHEX           ;PUNCH RECORD LENGTH
        CALL    PUNHL           ;PUNCH HL
        XOR     A
        CALL    PNHEX           ;PUNCH RECORD TYPE 0
PMEM:   LD      A,(HL)
        CALL    PNHEX           ;PUNCH MEMORY BYTE
        INC     HL              ;INCR. MEMORY POINTER
        DEC     C               ;DECR RECORD LENGTH
        JP      NZ,PMEM
        CALL    CSUM            ;PUNCH CHECKSUM
        JP      NEWREC          ;NEXT RECORD
;
; FINISHED, PUNCH LAST RECORD, RECORD
; LENGTH 00, THE START ADDRESS,
; AND A RECORD TYPE OF 01
;
DONE:   XOR     A
        LD      B,A             ;ZERO CHECKSUM
        CALL    PNHEX           ;ZERO RECORD LEN.
        POP     HL
        CALL    PUNHL           ;AUTOSTART H/L
        LD      A,H             ;CHECK FOR
        OR      L               ; AUTOSTART
        LD      A,0             ;0 WITH CARRY
        JP      Z,DON2          ;NO AUTOSTART
        INC     A
DON2:   CALL    PNHEX           ;RECORD TYPE 1
        CALL    CSUM            ;PUNCH CHECKSUM
        CALL    LEADR           ;PUNCH TRAILER
        JP      RSTRT           ;NEXT JOB
;
; PUNCH BLANK HEADER AND TRAILER
;
LEADR:  XOR     A
        LD      B,60            ;TAPE NULLS
NLDR:   CALL    POUT
        DEC     B
        JP      NZ,NLDR
        RET
;
; PUNCH THE H,L REGISTER PAIR
;
PUNHL:  LD      A,H             ;FETCH H
        CALL    PNHEX           ;PUNCH IT
        LD      A,L             ;GET L, PUNCH IT
;
; CONVERT A BINARY NUMBER TO TWO HEX
; CHARACTERS, PUNCH THEM, ADD TO CHECKSUM
;
PNHEX:  PUSH    AF              ;SAVE ON STACK
        ADD     B               ;ADD TO CHECKSUM
        LD      B,A             ;SAVE IT IN B
        POP     AF              ;RETRIEVE BYTE
        PUSH    AF
        RRA
        RRA                     ;ROTATE UPPER
        RRA
        RRA                     ;TO LOWER
        CALL    PHEX1           ;LEFT CHARACTER
        POP     AF              ;RIGHT CHARACTER
;
; PUNCH A HEX CHARACTER FROM
; LOWER FOUR BITS
;
PHEX1:  AND     0FH             ;MASK UPPER 4 BITS
        ADD     A,144
        DAA
        ADC     A,64
        DAA
        JP      POUT
;
; INPUT A HEX CHARACTER FROM TAPE
;
HEX:    CALL    PIN
        SUB     '0'
        CP      10
        RET     C               ;0-9
        SUB     7               ;A-F
        RET
;
; OUTPUT A BYTE TO THE PUNCH
;
POUT:   PUSH    AF
PUTW:   IN      A,(PSTAT)
        AND     POMSK
        JP      Z,PUTW
        POP     AF
        OUT     (PDATA),A
        RET
;
; INPUT A BYTE FROM PAPER TAPE
;
PIN:    IN      A,(PSTAT)
        AND     PIMSK
        JP      Z,PIN
        IN      A,(PDATA)
        AND     7FH             ;STRIP PARITY
        RET
;
; PUNCH CR, LF, NULLS AND COLON
;
PCRLF:  LD      A,CR
        CALL    POUT
        LD      A,LF
        CALL    POUT
        XOR     A
        CALL    POUT            ;TWO NULLS
        CALL    POUT
        LD      A,':'           ;COLON
        JP      POUT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ENTRY FOR LOAD, EXECUTE AND VERIFY
;
PLOAD:  LD      (TASK),A
        LD      A,17            ;TAPE READER ON
        CALL    POUT
        CALL    READHL          ;OFFSET
        LD      (OFSET),HL      ;SAVE IT
;
; PROCESS THE RECORD HEADING ON INPUT
;
HEAD:   CALL    PIN             ;INPUT FROM TAPE
        CP      ':'             ;COLON?
        JP      NZ,HEAD         ;NO, TRY AGAIN
        LD      B,0             ;ZERO THE CHECKSUM
        CALL    PHEX            ;RECORD LENGTH
        OR      A               ;IS IT ZERO?
        JP      Z,ENDFL         ;YES, DONE
        LD      C,A             ;SAVE REC.LEN.
        CALL    TAPEHL          ;GET H/L
        EX      DE,HL           ;ADDR TO D,E
        LD      HL,(OFSET)      ;GET OFFSET
        ADD     HL,DE           ;ADD
LOOP:   CALL    PHEX            ;INPUT DATA BYTE
        LD      E,A             ;SAVE BYTE
        LD      A,(TASK)        ;GET TASK
        CP      'V'             ;SEE IF VERIFYING
        LD      A,E             ;MOVE BACK
        JP      Z,SKIP          ;JUMP IF VERIFYING
        LD      (HL),A          ;DATA TO MEMORY
SKIP:   CP      (HL)            ;CHECK MEMORY
        JP      NZ,MERROR       ;BAD MEMORY
        INC     HL              ;INCREMENT POINTER
        DEC     C               ;DECR RECORD LEN
        JP      NZ,LOOP         ;NOT YET ZERO
        CALL    CHECK           ;PROCESS CHECKSUM
        JP      HEAD            ;START NEXT RECORD
;
; INPUT H,L AND RECORD TYPE FROM TAPE
;
TAPEHL: CALL    PHEX            ;READ H
        LD      H,A
        CALL    PHEX            ;READ L
        LD      L,A             ;READ RECORD TYPE
;
; CONVERT 2 CHAR FROM TAPE TO ONE BINARY
; WORD, STORE IN A AND ADD TO CHECKSUM
;
PHEX:   CALL    HEX             ;UPPER CHARACTER
        RLCA
        RLA                     ;MOVE TO UPPER
        RLA
        RLA
        LD      E,A             ;SAVE IT
        CALL    HEX             ;LOWER CHARACTER
        ADD     E               ;COMBINE BOTH
        LD      E,A             ;SAVE IT
        ADD     B               ;ADD TO CHECKSUM
        LD      B,A             ;SAVE T
        LD      A,E             ;RETRIEVE DATA
        RET
;
; ROUTINE TO CHECK FOR AUTOSTART
;
ENDFL:  CALL    TAPEHL          ;AUTOSTART ADDRESS
                                ;AND RECORD TYPE
        PUSH    AF              ;SAVE RECORD TYPE
        CALL    PHEX            ;INPUT CHECKSUM
        CALL    TOFF            ;TAPE READER OFF
        POP     AF              ;RETRIEVE REC TYPE
        CP      1               ;AUTOSTART?
        JP      NZ,RSTRT        ;NO
        LD      A,(TASK)        ;CHECK TASK
        CP      'E'             ;EXECUTE?
        JP      Z,JPCHL         ;YES, GO THERE
        CALL    OUTHL           ;NO, PRINT HL
        JP      RSTRT           ;NEXT TASK
;
;TURN OFF TAPE READER
;
TOFF:   LD      A,19
        JP      POUT
;
; CALCULATE AND PUNCH THE CHECKSUM
;
CSUM:   LD      A,B             ;CHECKSUM TO A
        CPL                     ;ONE'S COMPLEMENT
        INC     A               ;TWO'S COMPLEMENT
        JP      PNHEX           ;PUNCH CHECKSUM
;
; SEE IF CHECKSUM IS CORRECT (ZERO)
;
CHECK:  CALL    PHEX            ;INPUT CHECKSUM
        XOR     A
        ADD     B               ;IS CHECKSUM ZERO?
        RET     Z               ;YES, RETURN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ERROR MESSAGES
;
        LD      A,'C'           ;CHECKSUM ERROR
        DB      1               ;DB TRICK TO SKIP
MERROR: LD      A,'M'           ;M FOR BAD MEMORY
        PUSH    AF
        CALL    TOFF            ;TAPE READER OFF
        POP     AF
        CALL    OUTT            ;PRINT ERROR TYPE
        CALL    OUTHL           ;PRINT H/L
        JP      RSTRT
;
SIGN:   DB      CR,LF
        DB      'Hex paper-tape program'
        DB      CR,LF,LF
        DB      'E - load and execute'
        DB      CR,LF
        DB      'G - go to addresses given'
        DB      CR,LF
        DB      'R - read tape into memory'
        DB      CR,LF
        DB      '    (with optional offset)'
        DB      CR,LF
        DB      'V - verify tape against'
        DB      ' memory',CR,LF
        DB      'W - write paper tape'
        DB      ' (and label)',CR,LF,'    '
        DB      '(with optional autostart)'
        DB      CR,LF,0
LMESG:  DB      CR,LF,'Enter leader message'
        DB      CR,LF,0
;
; PUNCH READABLE LABELS ON PAPER TAPE
;
LABEL:  PUSH    HL
        PUSH    DE
        LD      DE,LMESG        ;LABEL MESS.
        CALL    SENDM           ;SEND IT
        CALL    INPLN           ;GET A LINE
LABL1:  CALL    GETCH           ;GET CHARACTER
        JP      C,LABL2         ;DONE ON CARRY
        SBC     A,20H           ;ASCII BIAS
        JP      C,LABL1         ;< SPACE
        CP      63
        JP      NC,LABL1        ;TOO BIG
        LD      L,A
        LD      E,A
        LD      H,0
        LD      D,0
        ADD     HL,HL           ;DOUBLE IT
        ADD     HL,HL           ;TIMES 4
        ADD     HL,DE           ;TIMES 5
        EX      DE,HL
        LD      HL,TABL
        ADD     HL,DE
        LD      C,5
NEXTC:  LD      A,(HL)
        CALL    POUT
        INC     HL
        DEC     C
        JP      NZ,NEXTC
        XOR     A
        CALL    POUT
        JP      LABL1           ;NEXT CHARACTER
LABL2:  CALL    LEADR
        POP     DE
        POP     HL
        RET
;
TABL:   DB      0,  0,  0,  0,  0       ; SPACE
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
        DB      6,  1,  185,9,  6       ; ?
        DB      126,129,157,145,14      ;
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
        DB      3,  4,  248,4,  3       ; Y
        DB      193,161,145,137,135     ; Z
        DB      0,  255,129,129,129     ; [
        DB      4,  8,  16, 32, 64      ; \
        DB      129,129,129,255,0       ; [
        DB      12, 2,  1,  2,  12      ; ^
;
; Fill rest of 8K ROM
;
        DS      $2000-$,$FF
;
; Variables in RAM
;
        ORG     $8000
TASK:   EQU     $               ;SAVE IT
        ORG     $+1
OFSET:  EQU     $               ;LOAD OFFSET
        ORG     $+32
STACK:  EQU     $               ;STACK SPACE
IBUFP:  EQU     $               ;BUFFER POINTER
        ORG     $+2
IBUFC:  EQU     $               ;BUFFER COUNT
        ORG     $+1
IBUFF:  EQU     $               ;INPUT BUFFER

;
        END
