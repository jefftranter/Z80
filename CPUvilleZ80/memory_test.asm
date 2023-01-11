;Memory testing program for 64-K RAM expansion
;Program initially loaded at 0x0800
;Starts in configuration 0, tests RAM from 0x0B00 to 0xFFFF
;Program re-locates to 0x0B00, switches to configuration 1, tests 0x0000 to 0x0AFF
;If memory error, quits and displays memory contents and test byte
write_char:		equ	0x000c
write_string:		equ	0x0018
byte_to_hex_string:	equ	0x0088
write_newline:		equ	0x0289
monitor_warm_start:	equ	0x046f
			org	0x0800			;start of RAM, config. 0
			call	write_newline
			ld	a,0x00			;first test pattern
			ld	(test_byte_0),a	;store
			ld	hl,0x0B00		;start of test, config 0
loop_1:		ld	a,(test_byte_0)	;get test byte
			ld	(hl),a			;store test byte
			inc	hl			;inc counter
			ld	a,h			;need to test for zero this way
			or	l
			jp	nz,loop_1
			ld	hl,0x0B00		;start of test loop
loop_2:		ld	a,(test_byte_0)
			cp	(hl)
			jp	nz,done_error_0
			inc	hl
			ld	a,h
			or	l
			jp	nz,loop_2
			ld	a,0xff			;second test pattern
			ld	(test_byte_0),a	;store
			ld	hl,0x0B00		;start of test, config 0
loop_3:		ld	a,(test_byte_0)	;get test byte
			ld	(hl),a			;store test byte
			inc	hl			;inc counter
			ld	a,h			;need to test for zero this way
			or	l
			jp	nz,loop_3
			ld	hl,0x0B00		;start of test loop
loop_4:		ld	a,(test_byte_0)
			cp	(hl)
			jp	nz,done_error_0
			inc	hl
			ld	a,h
			or	l
			jp	nz,loop_4
			ld	hl,done_configuration_0
			call	write_string
			jp	configuration_1
done_error_0:		ld	(current_location_0),hl
			ld	hl,error_msg_0
			call	write_string
			ld	a,(current_location_0+1)	;msb of address
			ld	hl, location_string_0
			call 	byte_to_hex_string
			ld	a,(current_location_0)	;lsb of address
			ld	hl,location_string_0+2
			call	byte_to_hex_string
			ld	hl,(current_location_0)
			ld	a,(hl)
			ld	hl,contents_string_0
			call	byte_to_hex_string
			ld	a,(test_byte_0)
			ld	hl,test_byte_string_0
			call	byte_to_hex_string
			ld	hl,location_string_0
			call	write_string
			ld	a,0x20			;space char
			call	write_char
			ld	hl,contents_string_0
			call	write_string
			ld	a,0x20
			call	write_char
			ld	hl,test_byte_string_0
			call	write_string
			call	write_newline
			jp	monitor_warm_start
current_location_0:	dw	0x0000		;current memory location being tested
test_byte_0:		db	0x00
location_string_0:	db	0,0,0,0,0		;will be address of memory error
contents_string_0:	db	0,0,0
test_byte_string_0:	db	0,0,0
error_msg_0:		db	"Error: memory contents do not match test byte: ",0
done_configuration_0:	db	"Configuration 0 test OK.\r\n",0
;
;Switch to configuration 1, test memory from 0x0000 to 0x09FF
;
;Code to move to higher memory
;
configuration_1:	ld	hl,code_origin	;start of code to transfer
			ld	bc,code_end-code_start+1	;length of code to transfer
			ld	de,0x0B00		;target of transfer
			ldir				;Z80 transfer instruction
			jp	0x0B00
code_origin:						;address of first byte of code before transfer
;	
			org	0x0B00
code_start:					
			out	(1),a			;switch to configuration 1
			ld	a,0x00			;first test pattern
			ld	(test_byte_1),a	;store
			ld	hl,0x0000		;start of test, config 1
loop_5:		ld	a,(test_byte_1)	;get test byte
			ld	(hl),a			;store test byte
			inc	hl			;inc counter
			ld	a,h		
			cp	0x0B			;MSB of space to test
			jp	nz,loop_5
			ld	hl,0x0000		;start of test loop
loop_6:		ld	a,(test_byte_1)
			cp	(hl)
			jp	nz,done_error_1
			inc	hl
			ld	a,h
			cp	0x0B
			jp	nz,loop_6
			ld	a,0xff			;second test pattern
			ld	(test_byte_1),a	;store
			ld	hl,0x0000		;start of test, config 1
loop_7:		ld	a,(test_byte_1)	;get test byte
			ld	(hl),a			;store test byte
			inc	hl			;inc counter
			ld	a,h
			cp	0x0B
			jp	nz,loop_7
			ld	hl,0x0000		;start of test loop
loop_8:		ld	a,(test_byte_1)
			cp	(hl)
			jp	nz,done_error_1
			inc	hl
			ld	a,h
			cp	0x0B
			jp	nz,loop_8
			jp	done_success
done_error_1:		out	(0),a
			ld	(current_location_1),hl
			ld	hl,error_msg_1
			call	write_string
			ld	a,(current_location_1+1)	;msb of address
			ld	hl, location_string_1
			call 	byte_to_hex_string
			ld	a,(current_location_1)	;lsb of address
			ld	hl,location_string_1+2
			call	byte_to_hex_string
			ld	hl,(current_location_1)
			ld	a,(hl)
			ld	hl,contents_string_1
			call	byte_to_hex_string
			ld	a,(test_byte_1)
			ld	hl,test_byte_string_1
			call	byte_to_hex_string
			ld	hl,location_string_1
			call	write_string
			ld	a,0x20			;space char
			call	write_char
			ld	hl,contents_string_1
			call	write_string
			ld	a,0x20
			call	write_char
			ld	hl,test_byte_string_1
			call	write_string
			call	write_newline
			jp	monitor_warm_start
done_success:		out	(0),a
			ld	hl,done_configuration_1
			call	write_string
			ld	hl,success_msg
			call	write_string
			jp	monitor_warm_start
current_location_1:	dw	0x0000			;current memory location being tested
test_byte_1:		db	0x00
location_string_1:	db	0,0,0,0,0		;will be address of memory error
contents_string_1:	db	0,0,0
test_byte_string_1:	db	0,0,0
done_configuration_1:	db	"Configuration 1 test OK.\r\n",0
success_msg:		db	"Memory test complete, no errors.\r\n",0
error_msg_1:		db	"Error: memory contents do not match test byte: ",0
code_end:
			end



