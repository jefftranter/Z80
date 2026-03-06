;	TITLE	'Display color bars'
;	LON	LIGC
;	XTEXT	HDOS
;
;	STL	'Definitions for TMS-9918 VDP'
;	EJECT
;
;	Definitions for TMS-9918 Video Display Processor
;
;
;	I/O address for VDP
;
VDPADR	EQU	270Q		; All references to VDP use this address
;
;	Registers
;
;
;	External Video Register
;
VPEVR	EQU	0		; External video register
;
VPNEV	EQU	00000000B	; No external video
VPEV	EQU	00000001B*256	; Enable external video
VPG2M	EQU	00000010B*256	; Graphics 2 mode (TMS-9918A only)
;
;	Option Control Register
;
VPOCR	EQU	1		; Option control register
;
VP4K	EQU	00000000B	; 4K RAMs
VP16K	EQU	10000000B	; 16K RAMs
VPDDP	EQU	00000000B	; Blank display
VPEDP	EQU	01000000B	; Enable display
VPDI	EQU	00000000B	; Disable interrupts
VPEI	EQU	00100000B	; Enable interrupts
VPPM	EQU	00000000B	; Pattern mode
VPG1M	EQU	VPPM		; Graphics 1 mode (TMS-9918A only)
VPMCM	EQU	00001000B	; Multicolor mode
VPTM	EQU	00010000B	; Text mode
VPS0	EQU	00000000B	; Size 0 (8X8 bit) sprites
VPS1	EQU	00000010B	; Size 1 (16X16 bit) sprites
VPM0	EQU	00000000B	; Magnification 0
VPM1	EQU	00000001B	; Magnification 1
;
;	Name Table Base Address Register
;
VPNTR	EQU	2		; Name table base address register
VPNTB	EQU	1024		; Name table base divisor
;
;	Color Table Base Address Register
;
VPCTR	EQU	3		; Color table base address register
VPCTB	EQU	64		; Color table base divisor
;
;	Pattern Generator Table Base Address Register
;
VPPGR	EQU	4		; Pattern generator table base address register
VPPGB	EQU	2048		; Pattern generator table base divisor
;
;	Sprite Name Table Base Address Register
;
VPSNR	EQU	5		; Sprite name table base address register
VPSNB	EQU	128		; Sprite name table base divisor
VPSOF	EQU	208		; Sprite terminator flag
;
;	Sprite Pattern Generator Base Address Register
;
VPSGR	EQU	6		; Sprite pattern generator base address register
VPSGB	EQU	2048		; Sprite pattern generator base divisor
;
;	Text/Backdrop Color Register
;
VPTBR	EQU	7		; Text/backdrop color register
;
;	Colors
;
VCCLR	EQU	0		; Transparent
VCBLK	EQU	1		; Black
VCMGR	EQU	2		; Medium green
VCLGR	EQU	3		; Light green
VCDBL	EQU	4		; Dark blue
VCLBL	EQU	5		; Light blue
VCDRD	EQU	6		; Dark red
VCCYN	EQU	7		; Cyan
VCMRD	EQU	8		; Medium red
VCLRD	EQU	9		; Light red
VCDYL	EQU	10		; Dark yellow
VCLYL	EQU	11		; Light yellow
VCDGR	EQU	12		; Dark green
VCMAG	EQU	13		; Magenta
VCGRY	EQU	14		; Gray
VCWHT	EQU	15		; White
VCLFT	EQU	16		; Multiplier for color in left nibble
;
;	Status Register
;
VPIF	EQU	10000000B	; Interrupt flag
VP5S	EQU	01000000B	; Fifth sprite flag
VPC	EQU	00100000B	; Coincidence flag
VPFSN	EQU	00011111B	; Fifth sprite number mask
;
;	Sprite flags
;
VPEC	EQU	10000000B	; Early clock flag
VPNEC	EQU	00000000B	; No early clock
;
;	STL	'Output color bars'
;	EJECT
;
;	Output color bars. This routine uses the multicolor mode of the VDP.
;
;	Memory mapping
;
;	0000 - 07FF	Multicolor color generator table
;	0800 - 0AFF	Multicolor name table
;	0B00 - 0BFF	unused
;	0C00 - 0C7F	Sprite attribute table
;	0C80 - 0FFF	unused
;	1000 - 17FF	Sprite pattern generator table
;	1800 - 3FFF	unused
;
;	VRAM memory equates
;
MCGTAD	EQU	0000H	;Multicolor color generator table base address
MNTAD	EQU	0800H	;Multicolor name table base address
SATAD	EQU	0C00H	;Sprite attribute table base address
SPGTAD	EQU	1000H	;Sprite pattern generator table base address
;
;	Initialize
;
        ifdef   CPM
USERFWA EQU	100H
        endif
        ifdef   HDOS
USERFWA	EQU	2280H
        endif
;
	ORG	USERFWA
BARS	EQU	$
;	LXI	SP,USERFWA	;Initialize stack pointer
;
;	Blank display
;
;	Register 1
;
	MVI	L,VPOCR		;Option control register
	LXI	H,VPNEV+VP16K+VPDDP+VPDI+VPMCM+VPS0+VPM0
	CALL	VPSOP
;
;	Set multicolor name table base register
;
	LXI	H,MNTAD		;Name table address
	CALL	VPSPN		;Set name table address register
;
;	Load multicolor name table
;
	CALL	VPSWA		;Set this as write address
	MVI	B,24		;Loop 6 times
R2LP1	EQU	$
	LXI	D,BARPAT	;Point to bar pattern
	MVI	C,16		;loop for 16 bars
R2LP2	EQU	$
	LDAX	D		;Pick up a color
	INX	D		;Point to next entry
	CALL	VPWRV
	CALL	VPWRV
	DCR	C		;Count one bar written
	JNZ	R2LP2		;Loop for 16 bars
;
	DCR	B		;Count 1 line of bars
	JNZ	R2LP1		;Loop for 6 lines
;
;	Register 3 (not used in multicolor mode)
;
;	Set pattern generator address register
;
	LXI	H,MCGTAD	;Pattern generator table address
	CALL	VPSPG		;Set pattern generator register
;
;	Load 16 color patterns
;
	CALL	VPSWA		;Set this as beginning write address
;
	MVI	B,16		;Loop for 16 patterns
	XRA	A		;First pattern
;
R4LP	EQU	$
	CALL	VPWRV
	CALL	VPWRV
	CALL	VPWRV
	CALL	VPWRV
	CALL	VPWRV
	CALL	VPWRV
	CALL	VPWRV
	CALL	VPWRV
	ADI	11H		;Next pattern
	DCR	B		;Count one pattern written
	JNZ	R4LP		;Loop for all 16 patterns
;
;	Write sprite name table address
;
	LXI	H,SATAD		;Sprite name table address
	CALL	VPSSN		;Set sprite name table address
;
;	Turn off sprite processing
;
	CALL	VPSWA		;Set this as beginning write address
;
	MVI	A,VPSOF		;Kill sprite processing
	CALL	VPWRV		;Write to VRAM
;
;	Register 6
;
;
;	Register 7
;
	MVI	A,VCBLK		;Backdrop is black
	CALL	VPSTB		;Set text/backdrop color
;
;	Enable display
;
	LXI	H,VPNEV+VP16K+VPEDP+VPMCM+VPS0+VPM0
	CALL	VPSOP		;Set new options
;
;	Exit
;
	RET
;
;	STL	'Bar pattern'
;	EJECT
BARPAT	EQU	$
;
;ON	EQU	0		;On value for IF
;OFF	EQU	1		;Off value for IF
;
;	There are two options for bar patterns:
;
;	1)	PHASE - This option presents the bars in ascending phase
;			order so as to minimumize phase change between
;			colors.
;
;	2)	NTSC  - This is the order which appears on NTSC standard
;			color bars.
;
;
;	Change the following value to OFF to get NTSC bars.
;
;PHASE	EQU	ON		;Present color bars in phase order
;PHASE	EQU	OFF		;Present color bars in NTSC order
;
;	IF	PHASE
	DB	VCBLK		;Black
	DB	VCWHT		;White
	DB	VCGRY		;Gray
	DB	VCDBL		;Dark blue
	DB	VCLBL		;Light blue
	DB	VCCYN		;Cyan
	DB	VCDGR		;Dark green
	DB	VCMGR		;Medium green
	DB	VCLGR		;Light green
	DB	VCDYL		;Dark yellow
	DB	VCLYL		;Light yellow
	DB	VCDRD		;Dark red
	DB	VCMRD		;Medium red
	DB	VCLRD		;Light red
	DB	VCMAG		;Magenta
	DB	VCBLK		;Black
;	ELSE
;	DB	VCBLK		;Black
;	DB	VCWHT		;White
;	DB	VCGRY		;Gray
;	DB	VCDYL		;Yellow
;	DB	VCLYL		;Light yellow
;	DB	VCCYN		;Cyan
;	DB	VCDGR		;Dark green
;	DB	VCMGR		;Medium green
;	DB	VCLGR		;Light green
;	DB	VCMAG		;Magenta
;	DB	VCDRD		;Dark red
;	DB	VCMRD		;Medium red
;	DB	VCLRD		;Light red
;	DB	VCDBL		;Dark blue
;	DB	VCLBL		;Light blue
;	DB	VCBLK		;Black
;	ENDIF
;
;	STL	'VDP I/O Routines'
;	EJECT
;
;	VDP I/O Routines
;
;
;	VP.WRR - Write to VDP Register
;
;		Registers:
;
;			L:	Register to be written
;			A:	Data byte to be written to register
;
;		All registers preserved.
;
VPWRR	EQU	$
	PUSH	PSW		; Save PSW
	OUT	VDPADR+1	; Write to control register
	MOV	A,L		; Pick up register number to be written
	ANI	00000111B	; Mask out everything except register address
	ORI	10000000B	; High order bit must be on
	OUT	VDPADR+1	; Write to control register
	POP	PSW		; Restore PSW
	RET			; Return to caller
;
;	VP.SWA - Set VRAM write address
;
;		Registers:
;
;			H:     VRAM address to be written
;
;		All registers preserved.
;
VPSWA	EQU	$
	PUSH	PSW		; Save PSW
	MOV	A,L		; Pick up low order byte of RAM address
	OUT	VDPADR+1	; Write to control port
	MOV	A,H		; Pick up high order byte of RAM address
	ANI	00111111B	; Mask out unused address bits
	ORI	01000000B	; Set VRAM address code
	OUT	VDPADR+1	; Write to control port
	POP	PSW		; Restore PSW
	RET			; Return to caller
;
;	VP.WVD - Write to VRAM direct (from H)
;
;		Registers:
;
;			H:     VRAM address to be written
;			A:	Byte to be written
;
;		All registers preserved.
;
VPWVD	EQU	$
	CALL	VPSWA		; Set write address
	CALL	VPWRV		; Write to VRAM
	RET			; Return to caller
;
;	VP.WRV - Write to VRAM
;
;		Registers:
;
;			A:	Data to be written
;
;		All registers preserved.
;
VPWRV	EQU	$
	OUT	VDPADR		; Write to data port
	RET			; Return to caller
;
;	VP.RDR - Read VDP register
;
;		Registers:
;
;			A:	Status register returned here
;
;		Register A is destroyed.
;
VPRDR	EQU	$
	IN	VDPADR+1	; Read status register
	RET			; Return to caller
;
;	VP.SRA - Set VRAM read address
;
;		Registers:
;
;			H:     VRAM address to be read
;
;		All registers preserved.
;
VPSRA	EQU	$
	PUSH	PSW		; Save PSW
	MOV	A,L		; Pick up low order byte of RAM address
	OUT	VDPADR+1	; Write to control port
	MOV	A,H		; Pick up high order byte of RAM address
	ANI	00111111B	; Set flag
	OUT	VDPADR+1	; Write to control port
	POP	PSW		; Restore PSW
	RET			; Return to caller
;
;	VP.RVD - Read from VRAM direct (from H)
;
;		Registers:
;
;			H:     VRAM address to be read
;			A:	Data from VRAM stored here
;
;		Register A destroyed.
;
VPRVD	EQU	$
	CALL	VPSRA		; Set read address
	CALL	VPRDV		; Read from VRAM
	RET			; Return to caller
;
;	VP.RDV - Read from VRAM
;
;		Registers:
;
;			A:	Data from VRAM stored here
;
;		Register A is destroyed.
;
VPRDV	EQU	$
	IN	VDPADR		; Read from data port
	RET			; Return to caller
;	STL	'VDP Register maintenance routines'
;	EJECT
;
;	Register maintenance routines
;
;
;	In all of the following routines, register H contains the address
;	of the appropriate table.
;
;	These routines preserve all registers
;
;
;	VP.SOP - Set options
;
VPSOP	EQU	$
	PUSH	PSW		; Save PSW
	SHLD	VPOPT		; Save VDP options
	MOV	A,L		; Move in options control register
	MVI	L,VPOCR		; Select option control register
	CALL	VPWRR		; Write option control register
	MOV	A,H		; Move in external video register
	MVI	L,VPEVR		; Select external video register
	CALL	VPWRR		; Write external video register
	POP	PSW
	RET
;
;	Options
;
VPOPT	DW	0		; VDP options stored here
;
;	VP.STB- Set text/backdrop
;
VPSTB	EQU	$
	PUSH	H		; Save H
	STA	VPTBC		; Save text/backdrop color
	MVI	L,VPTBR		; Select text/backdrop register
	CALL	VPWRR		; Write text/backdrop color
	POP	H		; Restore H
	RET
;
;	Text/backdrop
;
VPTBC	DB	0		; Text/backdrop color stored here
;
;	VP.SPN - Set pattern name table base address register
;
VPSPN	EQU	$
	PUSH	PSW		; Save PSW
	PUSH	H		; Save H
	SHLD	VPPNT		; Store address of pattern name table
;
;	Move high order 4 bits of address to register A
;
	MOV	A,H		; Pick up high order address
	RRC			; High order 5 bits
	RRC			; High order 4 bits
;
;	Write pattern name table register
;
	MVI	L,VPNTR		; Pattern name table register
	CALL	VPWRR		; Write it to VDP
;
;	Restore registers and return
;
	POP	H		; Restore H
	POP	PSW		; Restore PSW
	RET			; Return to caller
;
;	Address of pattern name table in VRAM
;
VPPNT	DW	0		; address of pattern name table in VRAM
;
;	VP.SPG - Set pattern generator table base address register
;
VPSPG	EQU	$
	PUSH	PSW		; Save PSW
	PUSH	H		; Save H
	SHLD	VPPGT		; Store address of pattern generator table
;
;	Move high order 3 bits to register A
;
	MOV	A,H		; Pick up high order address
	RRC			; High order 5 bits
	RRC			; High order 4 bits
	RRC			; High order 3 bits
;
;	Write pattern generator table register
;
	MVI	L,VPPGR		; Pattern generator table register
	CALL	VPWRR		; Write it to VDP
;
;	Restore registers and return
;
	POP	H		; Restore H
	POP	PSW		; Restore PSW
	RET			; Return to caller
;
;	Address of pattern generator table in VRAM
;
VPPGT	DW	0		; Address of pattern generator table in VRAM
;
;	VP.SSN - Set sprite name table base address register
;
VPSSN	EQU	$
	PUSH	PSW		; Save PSW
	PUSH	H		; Save H
	SHLD	VPSNT		; Store address of sprite name table
;
;	Move high order 7 bits of address to register A
;
	MOV	A,L		; Low order byte
	RLC			; Get seventh bit out of low order byte
	MOV	A,H		; High order byte
	RAL			; Put seventh bit in register A
;
;	Write sprite name table register
;
	MVI	L,VPSNR		; Sprite name table register
	CALL	VPWRR		; Write it to VDP
;
;	Restore registers and return
;
	POP	H		; Restore H
	POP	PSW		; Restore PSW
	RET			; Return to caller
;
;	Address of sprite name table in VRAM
;
VPSNT	DW	0		; Address of sprite name table in VRAM
;
;	VP.SSG - Set sprite generator table base address register
;
VPSSG	EQU	$
	PUSH	PSW		; Save PSW
	PUSH	H		; Save H
	SHLD	VPSGT		; Store address of sprite generator table
;
;	Move high order 3 bits of address to register A
;
	MOV	A,H		; Pick up high order address
	RRC			; High order 5 bits
	RRC			; High order 4 bits
	RRC			; High order 3 bits
;
;	Write sprite generator table register
;
	MVI	L,VPSGR		; Sprite generator table register
	CALL	VPWRR		; Write it
;
;	Restore registers and return
;
	POP	H		; Restore H
	POP	PSW		; Restore PSW
	RET			; Return to caller
;
;	Address of sprite generator table in VRAM
;
VPSGT	DW	0		; Address of sprite generator table in VRAM
;
;	VP.SCG - Set color generator table base address register
;
VPSCG	EQU	$
	PUSH	PSW		; Save PSW
	PUSH	H		; Save H
	SHLD	VPCGT		; Store address of color generator table
;
;	Move high order 8 bits to register A
;
	MOV	A,L		; Low order address
	RLC
	RLC
	MOV	L,A		; Register L now set up
	MOV	A,H		; High order address
	RLC
	RLC
	ADD	L		; High order 8 bits now in register A
;
;	Write color generator table register
;
	MVI	L,VPCTR		; Color generator table register
	CALL	VPWRR		; Write it
;
;	Restore registers and return
;
	POP	H		; Restore H
	POP	PSW		; Restore PSW
	RET			; Return to caller
;
;	Address of color generator table in VRAM
;
VPCGT	DW	0		; Address of color generator table in VRAM
;
	END	BARS
