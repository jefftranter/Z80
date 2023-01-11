;Putsys3 by Donn Stewart, June 2019
;Copies the memory image of CP/M loaded at E400h onto track 0 of the first CP/M disk
;Image size is 6400 bytes, so need to save 6400/128 = 50 sectors
;For system with 64-sector tracks, image can all be on track 0
;Load and run from ROM monitor
;Uses calls to cbios, in memory at FA00h
;This version for z80_cbios3, with improved disk read and write routines
;Writes track 0, sectors 1 to 50 (sector 0 has cpm loader)
seldsk:		equ	0fb5fh		;pass disk no. in c
setdma:		equ	0fb89h		;pass address in bc
settrk:		equ	0fb78h		;pass track in reg C
setsec:		equ	0fb7dh		;pass sector in reg c
write:		equ	0fbfdh		;write one CP/M sector to disk
monitor_warm_start:	equ	046Fh	;Return to ROM monitor
		org	0800h
		ld	c,00h		;CP/M disk a
		call	seldsk
;Write track 0, sectors 1 to 50
		ld	a,1		;starting sector
		ld	(sector),a
		ld	hl,0E400h	;memory address to start
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
		cp	50
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


