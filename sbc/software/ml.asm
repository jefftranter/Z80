; Machine language example from NASCOM Basic.
;
; See NASCOM Basic manual Appendix D, Basic and Assembly Language.
; Note that on the Z80 SBC, USRLOC is not $1004,$1005 as on the NASCOM
; computer, it is instead at $8049,$804A.
;
; Program runs from address $F000. To reserve this, from Basic cold
; start respond to "Memory top?" with 61440.
;
; 1. Set the USR() function address to $F000 by POKEing the address
;    (low, high) to addresses $8049 and $804A.
; 2. POKE the program into memory starting at $F000.
; 3. Call USR() function.
;
; Example basic program:
;
; 10 FOR I = 0 TO 9
; 20 READ D
; 30 POKE &HF000+I,D
; 40 NEXT I
; 50 POKE &H8049,&H00
; 60 POKE &H804A,&HFF
; 70 FOR I = 0 TO 1000
; 80 PRINT "USR(";I;")=";USR(I)
; 90 NEXT I
; 100 DATA 205,7,10,19,122,67,205,125,17,201

; Source code
; Takes value passed to it, increments it, and returns value.

DEINT:  EQU     $0A07           ; Routine to get function argument and return in DE.
ABPASS: EQU     $117D           ; Routine to pass value in AB back to Basic

        ORG     $F000

        CALL    DEINT           ; Get function argument
        INC     DE              ; Increment value
        LD      A,D             ; Put D in A
        LD      B,E             ; Put E in B
        CALL    ABPASS          ; Pass back to Basic
        RET                     ; Return to Basic
