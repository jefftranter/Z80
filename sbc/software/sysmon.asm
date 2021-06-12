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
