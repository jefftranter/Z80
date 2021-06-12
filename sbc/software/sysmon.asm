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
TOP:    EQU     24      ;MEMORY TOP, K BYTES
ORGIN:  EQU     (TOP-2)*1024   ;PROGRAM START
;ASEG                          ;ABSOLUTE CODE
;.Z80
 ORG     ORGIN
;
STACK:  EQU     ORGIN-60H
CSTAT:  EQU     10H     ;CONSOLE STATUS
CDATA:  EQU     CSTAT+1 ;CONSOLE DATA
INMSK:  EQU     1       ;INPUT MASK
OMSK:   EQU     2       ;OUTPUT MASK
LSTAT:  EQU     12H     ;LIST STATUS (18)
LDATA:  EQU     LSTAT+1 ;LIST DATA   (18)
LOMSK:  EQU     2       ;OUTPUT MAST (18)
NNULS:  EQU     4       ;LIST NULLS (18)
;
PORTN:  EQU     STACK   ;CONS=0,LIST=1
IBUFP:  EQU     STACK+3 ;BUFFER POINTER
IBUFC:  EQU     IBUFF+2 ;BUFFER COUNT
IBUFF:  EQU     IBUFF+3 ;INPUT BUFFER
;
CTRH:   EQU     8       ;^H BACKSPACE
TAB:    EQU     9       ;^I
CTRP:   EQU     16      ;^P (18)
CTRQ:   EQU     17      ;^Q
CTRS:   EQU     19      ;^S
CTRX:   EQU     24      ;^X, ABORT
BACKUP: EQU     CTRH    ;BACKUP CHAR
DEL:    EQU     127     ;RUBOUT
APOS:   EQU     (39-'Q') & 0FFH
CR:     EQU     13      ;CARRIAGE RET
LF:     EQU     10      ;LINE FEED
;
START:
        JP      COLD    ;COLD START
RESTRT: JP      WARM    ;WARM START
;
; VECTORS TO USEFUL ROUTINES
;
COUT:   JP      OUTT    ;OUTPUT CHAR
CIN:    JP      INPUTT  ;INPUT CHAR
INLN:   JP      INPLN   ;INPUT A LINE
GCHAR:  JP      GETCH   ;GET CHAR
OUTH:   JP      OUTH    ;BIN TO HEX
;
; CONSOLE INPUT ROUTINE
; CHECK FOR CONTROL-, LIST TOGGLE
;
INPUTT: CALL    INSTAT    ;CHECK STATUS
        JR      Z,INPUTT  ;NOT READY
INPUT2: IN      A,(CDATA) ;GET BYTE
        AND     DEL
        CP      CTRX    ;ABORT?
        JR      Z,START ;YES
        CP      CTRP    ;^P?
        JR      Z,SETLST ;LIST
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
SETLST: LD      A,(PORTN) ;CHECK FLAG
        CPL               ;INVERT
        LD      (PORTN),A ;SAVE
        JR      INPUTT    ;NEXT BYTE
;
; CONSOLE INPUT ROUTINE
;
OUTT:   PUSH    AF
        LD      A,(PORTN) ;WHERE?
        OR      A       ;ZERO?
        JR      NZ,LOUT ;LIST OUTPUT
OUT2:   CALL    INSTAT  ;INPUT?
        JR      Z,OUT4  ;NO
        CALL    INPUT2  ;GET INPUT
        CP      CTRS    ;FREEZE?
        JR      NZ,OUT2 ;NO
OUT3:   CALL    INPUTT  ;INPUT?
        CP      CTRQ    ;RESUME?
        JR      NZ,OUT3 ;NO
        JR      OUT2
;
OUT4:   IN      A,(CSTAT)  ;GET STATUS
        AND     OMSK
        JR      Z,OUT2     ;NOT READY
        POP     AF
        OUT     (CDATA),A  ;SEND DATA
;
; LIST OUTPUT ROUTINE
; SEND TO CONSOLE TOO
;
LOUT:   CALL    INSTAT  ;INPUT?
        CALL    NZ,INPUT2 ;YES, GET IT
;
        IN      A,(LSTAT) ;CHECK STATUS
        AND     OMSK
        JR      Z,LOUT    ;NOT READY
        POP     AF
        OUT     (LDATA),A ;SEND DATA
        OUT     (CDATA),A ;CONSOLE TOO
        AND     7FH       ;MASK PARITY
;
; ADD TIME DELAY AFTER CARRIAGE RETURN
;
        CP      CR      ;CARRIAGE RET?
        RET     NZ      ;NO
        PUSH    DE      ;USE D,E
        LD      D,30 * NNULS
OUTCR:  LD      E,250
OUTCR2: DEC     E
        JR      NZ,OUTCR2 ;INNER LOOP
        DEC     D
        JR      NZ,OUTCR ;OUTER LOOP
        POP     DE       ;RESTORE
        RET
;
; CONTINUATION OF COLD START
;
COLD:   LD      SP,STACK
;
; INITIALIZE I/O PORTS
;
        LD      A,3







        LD      E,(HL)  ;LOW BYTE
        INC     HL
        LD      D,(HL)  ;HIGH BYTE
        EX      DE,HL   ;INTO H,L
        JP      (HL)    ;GO THERE
;
; COMMAND TABLE
;
TABLE:  DW      ASCII   ;A, DUMP,LOAD
        DW      ERROR   ;B
        DW      CALLS   ;C, SUBROUTINE
        DW      DUMP    ;D, DUMP
        DW      ERROR   ;E
        DW      FILL    ;F, MEMORY
        DW      GO      ;G,GO
        DW      HMATH   ;H, HEX MATH
        DW      IPORT   ;I, PORT INPUT
        DW      JUST    ;J, MEMORY TEST
        DW      ERROR   ;K
        DW      LOAD    ;L, LOAD
        DW      MOVE    ;M, MEMORY
        DW      ERROR   ;N
        DW      OPORT   ;O, PORT OUTPUT
        DW      ERROR   ;P
        DW      ERROR   ;Q
        DW      REPL    ;R, REPLACE
        DW      SEARCH  ;S, MEMORY
        DW      ERROR   ;T
        DW      ERROR   ;U
        DW      VERM    ;V, VERIFY MEM
        DW      ERROR   ;W
        DW      REGS    ;X, STK PNTR
        DW      ERROR   ;Y
        DW      ZERO    ;Z, MEMORY
;
; INPUT A LINE FROM CONSOLE AND PUT IT
; INTO THE BUFFER. CARRIAGE RETURN ENDS
; THE LINE. RUBOUT OR ^H CORRECTS LAST
; ENTRY. CONTROL-X RESTARTS LINE.
; OTHER CONTROL CHARACTERS ARE IGNORED.
;
INPLN:  LD      A,'>'   ;PROMPT
        CALL    OUTT
INPL1:  LD      HL,IBUFF  ;BUFFER ADDR
        LD      (IBUFP),HL  ;SAVE POINTER
        LD      C,0     ;COUNT
INPLI:  CALL    INPUTT  ;CONSOLE CHAR
        CP      ' '     ;CONTROL?
        JR      C,INPLC ;YES
        CP      DEL     ;DELETE?
        JR      Z,INPLB ;YES
        CP      'Z'+1   ;UPPER CASE?
        JR      C,INPL3 ;YES
        AND     5FH     ;MAKE UPPER
INPL3:  LD      (HL),A  ;INTO BUFFER
        LD      A,32    ;BUFFER SIZE
        CP      C       ;FULL
        JR      Z,INPLI ;YES, LOOP
