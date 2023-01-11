;Formats four CP/M disks
;Updated June 2019 to match improved z80_cbios3
;Writes E5h to 64 sectors on tracks 1 to 255 of each disk (track 0 for system).
;Uses calls to cbios, in memory at FA00h
seldsk:		equ	0fb5fh		;pass disk no. in c
setdma:		equ	0fb89h		;pass address in bc
settrk:		equ	0fb78h		;pass track in reg C
setsec:		equ	0fb7dh		;pass sector in reg c
write:		equ	0fbfdh		;write one CP/M sector to disk
prmsg:		equ	0fb8fh		;subroutine to write message
conout:		equ	0fb43h		;print a character
monitor_warm_start:	equ	046fh
		org	0800h
		ld	sp,format_stack
		ld	hl,format_string
		call	prmsg
		ld	a,00h		;starting disk
		ld	(disk),a
disk_loop:	ld	c,a		;CP/M disk a
		call	seldsk
		ld	a,1		;starting track (offset = 1)
		ld	(track),a
track_loop:	ld	a,0		;starting sector
		ld	(sector),a
		ld	hl,directory_sector	;address of data to write
		ld	(address),hl
		ld	a,(track)
		ld	c,a		;CP/M track
		call	settrk
sector_loop:	ld	a,(sector)
		ld	c,a		;CP/M sector
		call	setsec
		ld	bc,(address)	;memory location
		call	setdma
		call	write
		ld	a,(sector)
		cp	63
		jp	z,next_track
		inc	a
		ld	(sector),a
		jp	sector_loop
next_track:	ld	a,(track)
		cp	255
		jp	z,next_disk
		inc	a
		ld	(track),a
		ld	a,2eh		;period character
		ld	c,a
		call	conout
		ld	a,(char_count)
		inc	a
		cp	80
		jp	z,skip_1
		ld	(char_count),a
		jp	track_loop
skip_1:		ld	a,0
		ld	(char_count),a
		ld	a,0dh
		ld	c,a
		call	conout
		ld	a,0ah
		ld	c,a
		call	conout
		jp	track_loop
next_disk:	ld	a,(disk)
		inc	a
		cp	4
		jp	z,done
		ld	(disk),a
		jp	disk_loop
done:		jp	monitor_warm_start
disk:		db	00h
sector:		db	00h
track:		db	00h
address:	dw	0000h
format_string:	defm	0dh,0ah,'Formatting...',0dh,0ah,0
char_count:	db	00h
directory_sector:
		ds	128,0e5h	;byte for empty directory
		ds	32		;stack space
format_stack:
		end
	

