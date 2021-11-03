; Port of Palo Alto Tiny BASIC Version Three, by Li-Chen Wang.
; Source taken from PCC's Reference Book of Personal and Home Computing.
; Adapted to ASL assembler and ported to my Z80 SBC.

        CPU     8080

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                     T B I
;            TINY BASIC INTERPRETER
;                 VERSION 3.0
;               FOR 8080 SYSTEM
;                 LI-CHEN WANG
;                26 APRIL, 1977
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  *** MEMORY USAGE ***
;
;  0080-01FF  ARE FOR VARIABLES, INPUT LINE, AND STACK
;  2000-3FFF  ARE FOR TINY BASIC TExT & ARRAY
;  F000-F7FF  ARE FOR TBI CODE
;

BOTSCR  EQU     00080H
TOPSCR  EQU     00200H
BOTRAM  EQU     02999H
DFTLMT  EQU     04000H
BOTROM  EQU     0F000H

;
;  DEFINE VARIABLES, BUFFER, AND STACK IN RAM

        ORG     BOTSCR
KEYWRD  DS      1       ; WAS INIT DONE?
TXTLMT  DS      1       ; ->LIMIT OF TEXT AREA
VARBGN  DS      2*26    ; TB VARIABLES A-Z
CURRNT  DS      2       ; POINTS TO CURRENT LINE
STKGOS  DS      2       ; SAVES SP IN 'GOSUB'
VARNXT  DS      0       ; TEMP STORAGE
STKINP  DS      2       ; SAVES SP IN 'INPUT'
LOPVAR  DS      2       ; 'FOR' LOOP SAVE AREA
LOPINC  DS      2       ; INCREMENT
LOPLMT  DS      2       ; LIMIT
LOPLN   DS      2       ; LINE NUMBER
LOPPT   DS      2       ; TEXT POINTER
RANPNT  DS      2       ; RANDOM NUMBER POINTER
        DS      1       ; EXTRA BYTE FOR BUFFER
BUFFER  DS      132     ; INPUT BUFFER
BUFEND  DS      0       ; BUFFER ENDS
        DS      4       ; EXTRA BYTES FOR STACK
STKLMT  DS      0       ; SOFT LIMIT FOR STACK
        ORG     TOPSCR
STACK   DS      0       ; STACK STARTS HERE
        ORG     BOTRAM
TXTONF  DS      2
TEXT    DS      2

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** INITIALIZE
;

        ORG     BOTROM
INIT    LXI     SP,STACK
