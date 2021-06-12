; This code came from chapter 7, "A Z-80 System Monitor" from the book
; 8080/Z80 Assembly Language Techniques for Improved Programming by Alan
; R. Miller
;
; It was modified by Jeff Tranter <tranter@pobox.com> to assemble with
; the z80asm cross-assembler and to run on my Z80-based Single Board
; Computer.

;TITLE   Z-80 SYSTEM MONITOR
;
; 11 June 2020
;
; FOUR SECTIONS HAVE BEEN REMOVED:
;  VERS EQU     ...     (1 LINE)
;  SIGNON:      ...     (4 LINES)
;  LXI   D,SIGNUM       (2 LINES)
;  SENDM:       ...     (6 LINES)
;
; ONE SECTION HAS BEEN ADDED:
;  LIST OUTPUT ROUTINES
;

TOP:    EQU     24              ;MEMORY TOP, K BYTES
ORGIN:  EQU     (TOP-2)*1024    ;PROGRAM START
;ASEG                           ;ABSOLUTE CODE
;.Z80
 ORG     ORGIN
;
STACK:  EQU     ORGIN-60H
CSTAT:  EQU     10H             ;CONSOLE STATUS
CDATA:  EQU     CSTAT+1         ;CONSOLE DATA
INMSK:  EQU     1               ;INPUT MASK
OMSK:   EQU     2               ;OUTPUT MASK
LSTAT:  EQU     12H             ;LIST STATUS (18)
LDATA:  EQU     LSTAT+1         ;LIST DATA   (18)
LOMSK:  EQU     2               ;OUTPUT MAST (18)
NNULS:  EQU     4               ;LIST NULLS (18)
;
PORTN:  EQU     STACK           ;CONS=0,LIST=1
IBUFP:  EQU     STACK+3         ;BUFFER POINTER
IBUFC:  EQU     IBUFF+2         ;BUFFER COUNT
IBUFF:  EQU     IBUFF+3         ;INPUT BUFFER
;
CTRH:   EQU     8               ;^H BACKSPACE
TAB:    EQU     9               ;^I
CTRP:   EQU     16              ;^P (18)
CTRQ:   EQU     17              ;^Q
CTRS:   EQU     19              ;^S
CTRX:   EQU     24              ;^X, ABORT
BACKUP: EQU     CTRH            ;BACKUP CHAR
DEL:    EQU     127             ;RUBOUT
APOS:   EQU     (39-'Q') & 0FFH
CR:     EQU     13              ;CARRIAGE RET
LF:     EQU     10              ;LINE FEED
;
START:
        JP      COLD            ;COLD START
RESTRT: JP      WARM            ;WARM START
;
; VECTORS TO USEFUL ROUTINES
;
COUT:   JP      OUTT            ;OUTPUT CHAR
CIN:    JP      INPUTT          ;INPUT CHAR
INLN:   JP      INPLN           ;INPUT A LINE
GCHAR:  JP      GETCH           ;GET CHAR
OUTH:   JP      OUTH            ;BIN TO HEX
;
; CONSOLE INPUT ROUTINE
; CHECK FOR CONTROL-, LIST TOGGLE
;
INPUTT: CALL    INSTAT          ;CHECK STATUS
        JR      Z,INPUTT        ;NOT READY
INPUT2: IN      A,(CDATA)       ;GET BYTE
        AND     DEL
        CP      CTRX            ;ABORT?
        JR      Z,START         ;YES
        CP      CTRP            ;^P?
        JR      Z,SETLST        ;LIST
        RET
;
; GET CONSOLE-INPUT STATUS
;
INSTAT: IN      A,(CSTAT)
        AND     INMSK
        RET
;
; TOGGLE LIST OUTPUT WITH CONTROL-P
;
SETLST: LD      A,(PORTN)       ;CHECK FLAG
        CPL                     ;INVERT
        LD      (PORTN),A       ;SAVE
        JR      INPUTT          ;NEXT BYTE
;
; CONSOLE INPUT ROUTINE
;
OUTT:   PUSH    AF
        LD      A,(PORTN)       ;WHERE?
        OR      A               ;ZERO?
        JR      NZ,LOUT         ;LIST OUTPUT
OUT2:   CALL    INSTAT          ;INPUT?
        JR      Z,OUT4          ;NO
        CALL    INPUT2          ;GET INPUT
        CP      CTRS            ;FREEZE?
        JR      NZ,OUT2         ;NO
OUT3:   CALL    INPUTT          ;INPUT?
        CP      CTRQ            ;RESUME?
        JR      NZ,OUT3         ;NO
        JR      OUT2
;
OUT4:   IN      A,(CSTAT)       ;GET STATUS
        AND     OMSK
        JR      Z,OUT2          ;NOT READY
        POP     AF
        OUT     (CDATA),A       ;SEND DATA
        RET
;
; LIST OUTPUT ROUTINE
; SEND TO CONSOLE TOO
;
LOUT:   CALL    INSTAT          ;INPUT?
        CALL    NZ,INPUT2       ;YES, GET IT
;
        IN      A,(LSTAT)       ;CHECK STATUS
        AND     OMSK
        JR      Z,LOUT          ;NOT READY
        POP     AF
        OUT     (LDATA),A       ;SEND DATA
        OUT     (CDATA),A       ;CONSOLE TOO
        AND     7FH             ;MASK PARITY
;
; ADD TIME DELAY AFTER CARRIAGE RETURN
;
        CP      CR              ;CARRIAGE RET?
        RET     NZ              ;NO
        PUSH    DE              ;USE D,E
        LD      D,30 * NNULS
OUTCR:  LD      E,250
OUTCR2: DEC     E
        JR      NZ,OUTCR2       ;INNER LOOP
        DEC     D
        JR      NZ,OUTCR        ;OUTER LOOP
        POP     DE              ;RESTORE
        RET
;
; CONTINUATION OF COLD START
;
COLD:   LD      SP,STACK
;
; INITIALIZE I/O PORTS
;
        LD      A,3
        OUT     (CSTAT),A       ;RESET
        OUT     (LSTAT),A       ;RESET
        LD      A,15H
        OUT     (CSTAT),A       ;SET
        OUT     (LSTAT),A
        XOR     A               ;GET A ZERO
        LD      (PORTN),A       ;RESET
;
; WARM-START ENTRY
;
WARM:   LD      HL,WARM         ;RET TO
        PUSH    HL              ;HERE
;
; FIND TOP OF USABLE MEMORY.
; CHECK FIRST BYTE OF EACH PAGE OF MEMORY
; STARTING AT ADDRESS ZERO.  STOP AT STACK
; OR MISSING/DEFECTIVE/PROTECTED MEMORY.
; DISPLAY HIGH BYTE OF MEMPRY TOP.
;
        LD      HL,0            ;PAGE ZERO
        LD      B,STACK>>8      ;STOP HERE
NPAGE:  LD      A,(HL)          ;GET BYTE
        CPL                     ;COMPLEMENT
        LD      (HL),A          ;PUT IT BACK
        CP      (HL)            ;SAME?
        JR      NZ,MSIZE        ;NO, MEM TOP
        CPL                     ;ORIG BYTE
        LD      (HL),A          ;RESTORE IT
        INC     H               ;NEXT PAGE
        DJNZ    NPAGE           ;KEEP GOING
MSIZE:  LD      C,H             ;MEM TOP
        CALL    CRLF            ;NEW LINE
        CALL    OUTHX           ;PRINT MEM SIZE
        CALL    INPLN           ;CONSOLE LINE
        CALL    GETCH           ;FIRST CHAR
;
; MAIN COMMAND PROCESSOR
;
        SUB     'A'             ;CONVERT OFFSET
        JP      C,ERROR         ; < A
        CP      'Z'-'A'+1
        JP      NC,ERROR        ; > Z
        ADD     A,A             ;DOUBLE
        LD      HL,TABLE        ;START
        LD      D,0
        LD      E,A             ;OFFSEET
        ADD     HL,DE           ;ADD TO TABLE
        LD      E,(HL)          ;LOW BYTE
        INC     HL
        LD      D,(HL)          ;HIGH BYTE
        EX      DE,HL           ;INTO H,L
        JP      (HL)            ;GO THERE
;
; COMMAND TABLE
;
TABLE:  DW      ASCII           ;A, DUMP,LOAD
        DW      ERROR           ;B
        DW      CALLS           ;C, SUBROUTINE
        DW      DUMP            ;D, DUMP
        DW      ERROR           ;E
        DW      FILL            ;F, MEMORY
        DW      GO              ;G,GO
        DW      HMATH           ;H, HEX MATH
        DW      IPORT           ;I, PORT INPUT
        DW      JUST            ;J, MEMORY TEST
        DW      ERROR           ;K
        DW      LOAD            ;L, LOAD
        DW      MOVE            ;M, MEMORY
        DW      ERROR           ;N
        DW      OPORT           ;O, PORT OUTPUT
        DW      ERROR           ;P
        DW      ERROR           ;Q
        DW      REPL            ;R, REPLACE
        DW      SEARCH          ;S, MEMORY
        DW      ERROR           ;T
        DW      ERROR           ;U
        DW      VERM            ;V, VERIFY MEM
        DW      ERROR           ;W
        DW      REGS            ;X, STK PNTR
        DW      ERROR           ;Y
        DW      ZERO            ;Z, MEMORY
;
; INPUT A LINE FROM CONSOLE AND PUT IT
; INTO THE BUFFER. CARRIAGE RETURN ENDS
; THE LINE. RUBOUT OR ^H CORRECTS LAST
; ENTRY. CONTROL-X RESTARTS LINE.
; OTHER CONTROL CHARACTERS ARE IGNORED.
;
INPLN:  LD      A,'>'           ;PROMPT
        CALL    OUTT
INPL2:  LD      HL,IBUFF        ;BUFFER ADDR
        LD      (IBUFP),HL      ;SAVE POINTER
        LD      C,0             ;COUNT
INPLI:  CALL    INPUTT          ;CONSOLE CHAR
        CP      ' '             ;CONTROL?
        JR      C,INPLC         ;YES
        CP      DEL             ;DELETE?
        JR      Z,INPLB         ;YES
        CP      'Z'+1           ;UPPER CASE?
        JR      C,INPL3         ;YES
        AND     5FH             ;MAKE UPPER
INPL3:  LD      (HL),A          ;INTO BUFFER
        LD      A,32            ;BUFFER SIZE
        CP      C               ;FULL
        JR      Z,INPLI         ;YES, LOOP
        LD      A,(HL)          ;GET CHAR
        INC     HL              ;INCR POINTER
        INC     C               ;AND COUNT
INPLE:  CALL    OUT1            ;SHOW CHAR
        JR      INPLI           ;NEXT CHAR
;
; PROCESS CONTROL CHARACTER
;
INPLC:  CP      CTRH            ;^H?
        JR      Z,INPLB         ;YES
        CP      CR              ;RETURN?
        JR      NZ,INPLI        ;NO, IGNORE
;
; END OF INPUT LINE
;
        LD      A,C             ;COUNT
        LD      (IBUFC),A       ;SAVE
;
; CARRIAGE-RETURN, LINE-FEED ROUTINE
;
CRLF:   LD      A,CR
        CALL    OUTT            ;SEND CR
        LD      A,LF
        JP      OUTT            ;SEND LF
;
; DELETE PRIOR CHARACTER IF ANY
;
INPLB:  LD      A,C             ;CHAR COUNT
        OR      A               ;ZERO?
        JR      Z,IMNPLI        ;YES
        DEC     HL              ;BACK POINTER
        DEC     C               ;AND COUNT
        LD      A,BACKUP        ;CHARACTER
        JR      INPLE           ;SEND
;
; GET A CHARACTER FROM CONSOLE BUFFER
; SET CARRY IF EMPTY
;
GETCH:  PUSH    HL              ;SAVE REGS
        LD      HL,(IBUFP)      ;GET POINTER
        LD      A,(IBUFC)       ;AND COUNT
        SUB     1               ;DECR WITH CARRY
        JR      C,GETC4         ;NO MORE CHAR
        LD      (IBUFC),A       ;SAVE NEW COUNT
        LD      A,(HL)          ;GET CHARACTER
        INC     HL              ;INCR POINTER
        LD      (IBUFP),HL      ;AND SAVE
GETC4:  POP     HL              ;RESTORE REGS
        RET
;
; DUMP MEMORY IN HEXADECIMAL AND ASCII
;
DUMP:   CALL    RDHLDE          ;RANGE
DUMP2:  CALL    CRHL            ;NEW LINE
DUMP3:  LD      C,(HL)          ;GET BYTE
        CALL    OUTHX           ;PRINT
        INC     HL              ;POINTER
        LD      A,L
        AND     0FH             ;LINE END?
        JR      Z,DUMP4         ;YES, ASCII
        AND     3               ;SPACE
        CALL    Z,OUTSP         ; 4 BYTES
        JR      DUMP3           ;NEXT HEX
DUMP4:  CALL    OUTSP
        PUSH    DE
        LD      DE,-10H         ;RESET LINE
        ADD     HL,DE
        POP     DE
DUMP5:  CALL    PASCI           ;ASCII DUMP
        CALL    TSTOP           ;DONE?
        LD      A,L             ;NO
        AND     0FH             ;LINE END?
        JR      NZ,DUMP5        ;NO
        JR      DUMP2
;
; DISPLAY MEMORY BYTE IN ASCII IF
; POSSIBLE, OTHERWISE GIVE DECIMAL PNT
;
PASCI:  LD      A,(HL)          ;GET BYTE
        CP      DEL             ;HIGH BYTE ON?
        JR      NC,PASC2        ;YES
        CP      ' '             ;CONTROL CHAR?
        JR      NC,PASC3        ;NO
PASC2:  LD      A,'.'           ;CHANGE TO DOT
PASC3:  JP      OUTT            ;SEND
;
; GET H,L AND D,E FROM CONSOLE
; CHECK THAT D,E IS LARGER
RDHLDE: CALL    HHLDE
RDHLD2: LD      A,E
        SUB     L               ;E - L
        LD      A,D
        SBC     A,H             ;D - H
        JR      C,ERROR         ;H,L BIGGER
        RET
;
; INPUT H,L AND D,E
;
HHLDE:  CALL    READHL          ;H,L
        JR      C,ERROR         ;ONLY 1 ADDR
        EX      DE,HL           ;SAVE IN DE
        CALL    READHL          ;D,E
        EX      DE,HL           ;PUT BACK
        RET
;
;INPUT H,L FROM CONSOLE
;
READHL: PUSH    DE
        PUSH    BC              ;SAVE REGS
        LD      HL,0            ;CLEAR
RDHL2:  CALL    GETCH           ;GET CHAR
        JR      C,RDHL5         ;LINE END
        CALL    NIB             ;TO BINARY
        JR      C,RDHL4         ;NOT HEX
        ADD     HL,HL           ;SHIFT LEFT
        ADD     HL,HL           ; FOUR
        ADD     HL,HL           ; BYTES
        ADD     HL,HL
        OR      L               ;ADD NEW CHAR
        LD      L,A
        JR      RDHL2           ;NEXT
;
; CHECK FOR COMMA OR BLANK AT END
;
RDHL4:  CP      APOS            ;APOSTROPHE
        JR      Z,RDHL5         ;ASCII INPUT
        CP      (' '-'0') & 0FFH
        JR      NZ,ERROR        ;NOT BLANK
RDHL5:  POP     BC
        POP     DE              ;RESTORE
        RET
;
; CONVERT ASCII CHARACTERS TO BINARY
;
NIB:    SUB     '0'             ;ASCII BIAS
        RET     C               ; > 0
        CP      'F'-'0'+1
        CCF                     ;INVERT
        RET     C               ;ERROR, > F
        CP      10
        CCF                     ;INVERT
        RET     NC              ;NUMBER 0-9
        SUB     'A'-'9'-1
        CP      10              ;REMOVE :-
        RET                     ;LETTER A-F
;
; PRINT ? ON IMPROPER INPUT
;
ERROR:  LD      A,'?'
        CALL    OUTT
        JP      START           ;TRY AGAIN
;
; START NEW LINE, GIVE ADDRESS
;
CRHL:   CALL    CRLF            ;NEW LINE
;
; PRINT H,L IN HEX
;
OUTHL:  LD      C,H
        CALL    OUTHX           ;H
OUTLL:  LD      C,L
;
; OUTPUT HEX BYTE FROM C AND A SPACE
;
OUTHEX: CALL    OUTHX
;
; OUTPUT A SPACE
;
OUTSP:  LD      A,' '
        JP      OUTT
;
; OUTPUT A HEX BYTE FROM C
; BINARY TO ASCII HEX CONVERSION
;
OUTHX:  LD      A,C
        RRA                     ;ROTATE
        RRA                     ; FOUR
        RRA                     ; BITS TO
        RRA                     ; RIGHT
        CALL    HEX1            ;UPPER CHAR
        LD      A,C             ;LOWER CHAR
HEX1:   AND     OFH             ;TAKE 4 BITS
        ADD     A,90H
        DAA                     ;DAA TRICK
        ADC     A,40H
        DAA
        JP      QUIT
;
; CHECK FOR END. H,L MINUS D,E
; INCREMENT H,L
;
TSTOP:  INC     HL
        LD      A,E
        SUB     L               ; E - L
        LD      A,D
        SBC     A,H             ; D - H
        RET     NC              ;NOT DONE
        POP     HL              ;RAISE STACK
        RET
;
; ROUTINE TO GO ANYWHERE IN MEMORY
; FOR CALL ENTRY, ADDRESS OF WARM
; IS ON STACK, SO A SIMPLE RET
; WILL RETURN TO THIS MONITOR
;
GO:     POP     HL              ;RAISE STACK
CALLS:  CALL    READHL          ;GET ADDRESS
        JP      (HL)            ;GO THERE
;
; LOAD HEX OR ASCII CAHR INTO MEMORY
; FROM CONSOLE. CHECK TO SEE IF
; THE DATA ACTUALLY GOT THERE
; APOSTROPHE PRECEEDS ASCII CHAR
; CARRIAGE RET PASSES OVER LOCATION
;
LOAD:   CALL    READHL          ;ADDRESS
LOAD2:  CALL    OUTHL           ;PRINT IT
        CALL    PASCI           ;ASCII
        CALL    OUTSP
        LD      C,(HL)          ;ORIG BYTE
        CALL    OUTHEX          ;HEX
        PUSH    HL              ;SAVE PNTR
        CALL    INPL2           ;INPUT
        CALL    READHL          ; BYTE
        LD      B,L             ; TO B
        POP     HL
        CP      APOS
        JR      Z,LOAD6         ;ASCII INPUT
        LD      A,C             ;HOW MANY?
        OR      A               ;NONE?
        JR      Z,LOAD3         ;YES
LOAD4:  CALL    CHEKM           ;INTO MEMORY
LOAD3:  INC     HL              ;POINTER
        JR      LOAD2
;
; LOAD ASCII CHARACTER
;
LOAD6:  CALL    GETCH
        LD      B,A
        JR      LOAD4
;
; COPY BYTE FROM B TO MEMORY
; AND SEE THAT IT GOT THERE
;
CHECKM: LD      (HL),B          ;PUT IN MEM
        LD      A,(HL)          ;GET BACK
        CP      B               ;SAME?
        RET     Z               ;OK
ERRP:   POP     AF              ;RAISE STACK
ERRB:   LD      A,'B'           ;BAD
        CALL    OUTT
        CALL    OUTSP
        JP      OUTHL           ;POINTER
;
; DISPLAY STACK POINTER
;
REGS:   LD      HL,0
        ADD     HL,SP
        JP      OUTHL
;
; ZERO A PORTION OF MEMORY
;
ZERO:   CALL    RDHLDE          ;RANGE
        LD      B,0
        JR      FILL2
;
; FILL A PORTION OF MEMORY
;
FILL:   CALL    HLDEB           ;RANGE, BYTE
        CP      APOS            ;APOSTROPHE?
        JR      Z,FILL4         ;YES, ASCII
        LD      B,C
FILL2:  LD      A,H             ;FILL BYTE
        CP      STACK>>8        ;TOO FAR?
        JP      NC,ERROR        ;YES
FILL3:  CALL    CHEKM           ;PUT, CHECK
        CALL    TSTOP           ;DONE?
        JR      FILL2           ;NEXT
;
FILL4:  CALL    GETCH           ;ASCII CHAR
        LD      B,A
        JR      FILL3
;
; GET H,L D,E AND B,C
;
HLDEBC: CALL    HLDECK          ;RANGE
        JP      C,ERROR         ;NO BYTE
        PUSH    HL
