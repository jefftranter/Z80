; This code came from chapter 7, "A Z-80 System Monitor" from the book
; "8080/Z80 Assembly Language - Techniques for Improved Programming"
; by Alan R. Miller
;
; It was modified by Jeff Tranter <tranter@pobox.com> to assemble with
; the z80asm cross-assembler and to run on my Z80-based Single Board
; Computer.

; Commands:
;
; Dump:        D<start> <end>
; Call:        C<address>
; Go:          G<address>
; Load:        L<address>
; Stack:       X<addreess>
; Fill:        F<start> <end> <value>
; Move:        M<start> <end> <dest>
; Search:      S<start> <byte [<byte>]
; Input port:  I<addr>
; Output port: O<port> <data>
; Hex math:    H<value1> <value2>
; Memory test: J<start> <end>
; Replace:     R<start> <stop> <orig> <new>
; Verify:      V<start> <end> <compare>

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

TOP:    EQU     32              ;MEMORY TOP, K BYTES
ORGIN:  EQU     (TOP-2)*1024    ;PROGRAM START
;ASEG                           ;ABSOLUTE CODE
;.Z80
 ORG     ORGIN
;
STACK:  EQU     ORGIN-60H
CSTAT:  EQU     80H             ;CONSOLE STATUS
CDATA:  EQU     CSTAT+1         ;CONSOLE DATA
INMSK:  EQU     1               ;INPUT MASK
OMSK:   EQU     2               ;OUTPUT MASK
LSTAT:  EQU     80H             ;LIST STATUS (18)
LDATA:  EQU     LSTAT+1         ;LIST DATA   (18)
LOMSK:  EQU     2               ;OUTPUT MASK (18)
NNULS:  EQU     4               ;LIST NULLS (18)
;
PORTN:  EQU     STACK           ;CONS=0,LIST=1
IBUFP:  EQU     STACK+3         ;BUFFER POINTER
IBUFC:  EQU     IBUFP+2         ;BUFFER COUNT
IBUFF:  EQU     IBUFP+3         ;INPUT BUFFER
;
CTRH:   EQU     8               ;^H BACKSPACE
TAB:    EQU     9               ;^I
CTRP:   EQU     16              ;^P (18)
CTRQ:   EQU     17              ;^Q
CTRS:   EQU     19              ;^S
CTRX:   EQU     24              ;^X, ABORT
BACKUP: EQU     CTRH            ;BACKUP CHAR
DEL:    EQU     127             ;RUBOUT
APOS:   EQU     (39-'0') & 0FFH
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
OUTH:   JP      OUTHX           ;BIN TO HEX
;
; CONSOLE INPUT ROUTINE
; CHECK FOR CONTROL-P, LIST TOGGLE
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
; DISPLAY HIGH BYTE OF MEMORY TOP.
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
        LD      E,A             ;OFFSET
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
INPLE:  CALL    OUTT            ;SHOW CHAR
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
        JR      Z,INPLI         ;YES
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
HEX1:   AND     0FH             ;TAKE 4 BITS
        ADD     A,90H
        DAA                     ;DAA TRICK
        ADC     A,40H
        DAA
        JP      OUTT
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
; LOAD HEX OR ASCII CHAR INTO MEMORY
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
CHEKM:  LD      (HL),B          ;PUT IN MEM
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
FILL:   CALL    HLDEBC          ;RANGE, BYTE
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
        CALL    READHL          ;3RD INPUT
        LD      B,H             ;MOVE TO
        LD      C,L             ; B,C
        POP     HL
        RET
;
; GET 2 ADDRESSES, CHECK THAT
; ADDITIONAL DATA IS INCLUDED
;
HLDECK: CALL    HHLDE           ;2 ADDRS
        JP      C,ERROR         ;THAT'S ALL
        JP      RDHLD2          ;CHECK
;
; MOVE A BLOCK OF MEMORY H,L-D,E TO B,C
;
MOVE:   CALL    HLDEBC          ;3 ADDR
MOVDN:  CALL    MOVIN           ;MOVE/CHECK
        CALL    TSTOP           ;DONE
        INC     BC              ;NO
        JR      MOVDN
;
MOVIN:  LD      A,(HL)          ;BYTE
        LD      (BC),A          ;NEW LOCATION
        LD      A,(BC)          ;CHECK
        CP      (HL)            ;IS IT THERE?
        RET     Z               ;YES
        LD      H,B             ;ERROR
        LD      L,C             ;INTO H,L
        JP      ERRP            ;SHOW BAD
;
; SEARCH FOR 1 OR 2 BYTES OVER THE
; RANGE H,L D,E. BYTES ARE IN B,C
; B HAS CARRIAGE RETURN IF ONLY ONE BYTE
; PUT SPACE BETWEEN BYTES IF TWO
; FORMAT: START STOP BYTE1 BYTE1
;
SEARCH: CALL    HLDEBC          ;RANGE, 1ST BYTE
SEAR2:  LD      B,CR            ;SET FOR 1 BYTE
        JR      C,SEAR3         ;ONLY ONE
        PUSH    HL
        CALL    READHL          ;2ND BYTE
        LD      B,L             ;INTO C
        POP     HL
SEAR3:  LD      A,(HL)          ;GET BYTE
        CP      C               ;MATCH?
        JR      NZ,SEAR4        ;NO
        INC     HL              ;YES
        LD      A,B             ;ONLY 1?
        CP      CR
        JR      Z,SEAR5         ;YES
;
; FOUND FIRST MATCH, CHECK FOR SECOND
;
        LD      A,(HL)          ;NEXT BYTE
        CP      B               ;MATCH?
        JR      NZ,SEAR4        ;NO
;
SEAR5:  DEC     HL              ;A MATCH
        PUSH    BC
        CALL    CRHL            ;SHOW ADDR
        POP     BC
SEAR4:  CALL    TSTOP           ;DONE?
        JR      SEAR3           ;NO
;
; ASCII SUB-COMMAND PROCESSOR
;
ASCII:  CALL    GETCH           ;NEXT CHAR
        CP      'D'             ;DISPLAY
        JR      Z,ADUMP
        CP      'S'             ;SEARCH
        JR      Z,ASCS
        CP      'L'             ;LOAD
        JP      NZ,ERROR
;
; LOAD ASCII CHARACTERS INTO MEMORY
; QUIT ON CONTROL-X
;
        CALL    READHL          ;ADDRESS
        CALL    OUTHL           ;PRINT IT
ALOD2:  CALL    INPUTT          ;NEXT CHAR
        CALL    OUTT            ;PRINT IT
        LD      B,A             ;SAVE
        CALL    CHEKM           ;INTO MEMORY
        INC     HL              ;POINTER
        LD      A,L
        AND     7FH             ;LINE END?
        JR      NZ,ALOD2        ;NO
        CALL    CRHL            ;NEW LINE
        JR      ALOD2
;
; DISPLAY MEMORY IN STRAIGHT ASCII.
; KEEP CARRIAGE RETURN, LINE FEED, CHANGE
; TAB TO SPACE, REMOVE OTHER CONTROL CHAR.
;
ADUMP:  CALL    RDHLDE          ;RANGE
ADMP2:  LD      A,(HL)          ;GET BYTE
        CP      DEL             ;HIGH BIT ON?
        JR      NC,ADMP4        ;YES
        CP      ' '             ;CONTROL
        JR      NC,ADMP3        ;NO
        CP      CR              ;CARR RET?
        JR      Z,ADMP3         ;YES, OK
        CP      LF              ;LINE FEED?
        JR      Z,ADMP3         ;YES, OK
        CP      TAB
        JR      NZ,ADMP4        ;SKIP OTHER
        LD      A,' '           ;SPACE FOR TAB
ADMP3:  CALL    OUTT            ;SEND
ADMP4:  CALL    TSTOP           ;DONE?
        JR      ADMP2           ;NO
;
; SEARCH FOR 1 OR 2 ASCII CHARACTERS
; NO SPACE BETWEEN ASCII CHARS
; FORMAT: START STOP 1 OR 2 ASCII CHAR
;
ASCS:   CALL    RDHLDE          ;RANGE
        CALL    GETCH           ;FIRST CHAR
        LD      C,A
        CALL    GETCH           ;2ND OR CARR RET
        JP      C,SEAR2         ;ONLY ONE CHAR
        LD      B,A             ;2ND
        JP      SEAR3
;
; INPUT FROM ANY PORT (Z-80 VERSION)
;
IPORT:  CALL    READHL          ;PORT
        LD      C,L             ;PORT TO C
        IN      L,(C)           ;INPUT
        CALL     OUTLL          ;HEX VALUE
;
; PRINT L REGISTER IN BINARY (Z-80 VER)
;
BITS:   LD      B,8             ;8 BITS
BIT2:   SLA     L               ;SHIFT L LEFT
        LD      A,'0'/2         ;HALF OF 0
        ADC     A,A             ;DOUBLE+CARRY
        CALL    OUTT            ;PRINT BIT
        DJNZ    BIT2            ;8 TIMES
        RET
;
; OUTPUT BYTE FROM PORT (Z-80 VERSION)
;
OPORT:  CALL    READHL          ;PORT
        LD      C,L
        CALL    READHL          ;DATA
        OUT     (C),L           ;OUTPUT
        RET
;
; HEXADECIMAL MATH, SUM AND DIFFERENCE
;
HMATH:  CALL    HHLDE           ;TWO NUMBERS
        PUSH    HL              ;SAVE H,L
        ADD     HL,DE           ;SUM
        CALL    OUTHL           ;PRINT IT
        POP     HL
        OR      A               ;CLEAR CARRY
        SBC     HL,DE
        JP      OUTHL           ;DIFFERENCE
;
; MEMORY TEST THAT DOESN'T ALTER CURRENT BYTE
; INPUT RANGE OF ADDRESSES, ABORT WITH ^X
;
JUST:   CALL    RDHLDE          ;RANGE
        PUSH    HL              ;SAVE START ADDR
JUST2:  LD      A,(HL)          ;GET BYTE
        CPL                     ;COMPLEMENT IT
        LD      (HL),A          ;PUT IT BACK
        CP      (HL)            ;DID IT GO?
        JP      NZ,JERR         ;NO
        CPL                     ;ORIGINAL BYTE
        LD      (HL),A          ;PUT IT BACK
JUST3:  LD      A,L             ;PASS
        SUB     E               ; COMPLETED?
        LD      A,H
        SBC     A,D
        INC     HL
        JR      C,JUST2         ;NO
;
; AFTER EACH PASS,
; SEE IF ABORT WANTED
;
        CALL    INSTAT          ;INPUT
        CALL    NZ,INPUTT       ;YES, GET IT
        POP     HL              ;START ADDR
        PUSH    HL              ;SAVE AGAIN
        JR      JUST2           ;NEXT PASS
;
; FOUND MEMORY ERROR, PRINT POINTER AND
; BIT MAP: 0=GOOD, 1=BAD BIT
JERR:   PUSH    AF              ;SAVE COMPLEMENT
        CALL    CRHL            ;PRINT POINTER
        POP     AF
        XOR     (HL)            ;SET BAD BITS
        PUSH    HL              ;SAVE POINTER
        LD      L,A             ;BIT MAP TO L
        CALL    BITS            ;PRINT BINARY
        POP     HL
        JR      JUST3           ;CONTINUE
;
; REPLACE HEX BYTE WITH ANOTHER
; FORMAT IS: START, STOP, ORIG, NEW
REPL:   CALL    HLDEBC          ;RANGE, 1ST BYTE
        JP      C,ERROR         ;NO 2ND
        LD      B,C             ;1ST TO B
        PUSH    HL
        CALL    READHL          ;2ND BYTE
        LD      C,L             ;INTO C
        POP     HL
REPL2:  LD      A,(HL)          ;FETCH BYTE
        CP      B               ;A MATCH?
        JR      NZ,REPL3        ;NO
        LD      (HL),C          ;SUBSTITUTE
        LD      A,C
        CP      (HL)            ;SAME
        JP      NZ,ERRB         ;NO, BAD
REPL3:  CALL    TSTOP           ;DONE?
        JR      REPL2
;
; GIVE RANGE OF 1ST BLOCK AND START OF SECOND
;
VERM:   CALL    HLDEBC          ;3 ADDRESSES
VERM2:  LD      A,(BC)          ;FETCH BYTE
        CP      (HL)            ;SAME AS OTHER?
        JR      Z,VERM3         ;YES
        PUSH    HL              ;DIFFERENT
        PUSH    BC
        CALL    CRHL            ;PRINT 1ST POINTER
        LD      C,(HL)          ;FIRST BYTE
        CALL    OUTHEX          ;PRINT IT
        LD      A,':'
        CALL    OUTT
        POP     HL              ;B,C TO H,L
        CALL    OUTHL           ;SECOND POINTER
        LD      C,(HL)          ;2ND BYTE
        CALL    OUTHX           ;PRINT IT
        LD      C,L             ;RESTORE C
        LD      B,H             ;AND B
        POP     HL              ;AND H,L
VERM3:  CALL    TSTOP           ;DONE?
        INC     BC              ;2ND POINTER
        JR      VERM2
;
        END
