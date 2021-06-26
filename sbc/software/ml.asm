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
; 10 A = &HF000
; 20 READ D
; 30 IF D = -1 THEN 70
; 40 POKE A,D
; 50 A = A + 1
; 60 GOTO 20
; 70 POKE &H8049,&H00
; 80 POKE &H804A,&HF0
; 90 FOR I = 0 TO 100
; 100 PRINT "USR(";I;")=";USR(I)
; 110 NEXT I
; 120 DATA 205,7,10,19,122,67,195,125,17,-1

; Can generate decimal data above using Linux command like:
; hexdump -v -e '/1 "%u,\n"' ml.bin

; Source code
; Takes value passed to it, increments it, and returns value.

DEINT:  EQU     $0A07           ; Routine to get function argument and return in DE.
ABPASS: EQU     $117D           ; Routine to pass value in AB back to Basic

        ORG     $F000

        CALL    DEINT           ; Get function argument
        INC     DE              ; Increment value
        LD      A,D             ; Put D in A
        LD      B,E             ; Put E in B
        JP      ABPASS          ; Pass back to Basic
