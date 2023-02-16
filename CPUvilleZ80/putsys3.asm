;Putsys3 by Donn Stewart, June 2019
;Copies the memory image of CP/M loaded at E400h onto track 0 of the first CP/M disk
;Image size is 6400 bytes, so need to save 6400/128 = 50 sectors (51 for 16 disk version)
;For system with 64-sector tracks, image can all be on track 0
;Load and run from ROM monitor
;Uses calls to cbios, in memory at FA00h (varies depending on number of disk drives)
;This version for z80_cbios3, with improved disk read and write routines
;Writes track 0, sectors 1 to 50 or 51 (sector 0 has cpm loader)

		include	'defines.asm'

; Make sure these addresses match those in z80-cbios.asm
		if	DRIVES == 4
CCP:		equ	0E400H
seldsk:		equ	0fb5fh		;pass disk no. in c
setdma:		equ	0fb89h		;pass address in bc
settrk:		equ	0fb78h		;pass track in reg C
setsec:		equ	0fb7dh		;pass sector in reg c
write:		equ	0fbfdh		;write one CP/M sector to disk
		endif
		if	DRIVES == 8
CCP:		eq0u	E100H
seldsk:		equ	0f89fh		;pass disk no. in c
setdma:		equ	0f8c9h		;pass address in bc
settrk:		equ	0f8b8h		;pass track in reg C
setsec:		equ	0f8bdh		;pass sector in reg c
write:		equ	0f942h		;write one CP/M sector to disk
		endif
		if	DRIVES == 16
CCP:		equ	0DD00H
seldsk:		equ	0f51fh		;pass disk no. in c
setdma:		equ	0f549h		;pass address in bc
settrk:		equ	0f538h		;pass track in reg C
setsec:		equ	0f53dh		;pass sector in reg c
write:		equ	0f5c4h		;write one CP/M sector to disk
		endif

monitor_warm_start:	equ	046Fh	;Return to ROM monitor
		org	0800h
		ld	c,00h		;CP/M disk a
		call	seldsk
;Write track 0, sectors 1 to 50
		ld	a,1		;starting sector
		ld	(sector),a
		ld	hl,CCP		;memory address to start
		ld	(address),hl
		ld	c,0		;CP/M track
		call	settrk
wr_trk_0_loop:	ld	a,(sector)
		ld	c,a		;CP/M sector
		call	setsec
		ld	bc,(address)	;memory location
		call	setdma
		call	write
		ld	a,(sector)
		if DRIVES == 16
		cp	51		;Need 51 sectors
		else
		cp	50		;Need 50 sectors
		endif
		jp	z,done
		inc	a
		ld	(sector),a
		ld	hl,(address)
		ld	de,128
		add	hl,de
		ld	(address),hl
		jp	wr_trk_0_loop
done:		jp	monitor_warm_start
sector:		db	00h
address:	dw	0000h
		end
