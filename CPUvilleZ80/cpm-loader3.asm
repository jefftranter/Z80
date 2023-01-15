;Retrieves CP/M image from disk and loads it in memory starting at E400h
;Uses calls to ROM routine for disk read.
;Reads track 0, sectors 1 to 50
;This program is loaded into LBA sector 0 of disk, read to loc. 0800h by ROM and executed.
hstbuf: 	equ	0900h		;will put 256-byte raw sector here
disk_read:	equ	0294h		;in 2K ROM
cpm:		equ	0FA00h		;CP/M cold start entry
		org	0800h
;Read track 0, sectors 1 to 50
		ld	a,1		;starting sector -- sector 0 has cpm_loader
		ld	(sector),a
		ld	hl,0E400h	;memory address to place image
		ld	(dmaad),hl
		ld	a,0		;CP/M track
		ld	(track),a
rd_trk_0_loop:	call	read
		ld	a,(sector)
		cp	50
		jp	z,done
		inc	a
		ld	(sector),a
		ld	hl,(dmaad)
		ld	de,128
		add	hl,de
		ld	(dmaad),hl
		jp	rd_trk_0_loop
done:		out	(1),a		;switch memory config to all-RAM
		jp	cpm

read:
;Read one CP/M sector from disk 0
;Track number in 'track'
;Sector number in 'sector'
;Dma address (location in memory to place the CP/M sector) in 'dmaad' (0-65535)
;
			ld	hl,hstbuf		;buffer to place raw disk sector (256 bytes)
			ld	a,(sector)
			sla	a			;shifts to match cbios CP/M disk read routine
			sla	a			;which combine sector and disk no. in one byte
			ld	c,a			;LBA bits 0 to 7
			ld	a,(track)
			ld	b,a			;LBA bits 8 to 15
			ld	e,00h			;LBA bits 16 to 23
			call	disk_read		;subroutine in ROM
;Transfer top 128-bytes out of buffer to memory
			ld	hl,(dmaad)		;memory location to place data read from disk
			ld	de,hstbuf		;host buffer
			ld	b,128			;size of CP/M sector
rd_sector_loop:		ld	a,(de)			;get byte from host buffer
			ld	(hl),a			;put in memory
			inc	hl
			inc	de
			djnz	rd_sector_loop		;put 128 bytes into memory
			in	a,(0fh)			;get status
			and	01h			;error bit
			ret
sector:		db	00h
track:		db	00h
dmaad:		dw	0000h
		end

	

