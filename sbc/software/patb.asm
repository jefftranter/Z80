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
EOTROM  EQU     0F000H

;
;  DEFINE VARIABLES, BUFFER, AND STACK IN RAM

         ORG    BOTSCR
KEYWRD   DS     1       ; WAS INIT DONE?
TXTLMT   DS     1       ; ->LIMIT CF TEXT AREA
