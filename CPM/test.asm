; CP/M assembly language demo program. Displays the character "$" on
; the console. Intended to be assembled using the CP/M asm 8080
; assembler. Taken from the book "CP/M Assembly Language Programming"
; by Ken Barbier.

BDOS	EQU	5
WCONF	EQU	2
	ORG	100H
	MVI	C,WCONF
	MVI	E,'$'
	CALL	BDOS
	JMP	0
	END
