;ROM monitor for a system with serial interface and IDE disk and memory expansion board.
;The disk extension board has 64K RAM -- computer board memory decoder disabled (J2 off).
;The disk extension board uses ports 2 and 3 for the serial interface, and 8 to 15 for the disk
;Therefore the computer board I/O decoder is also disabled (J1 off)
;Output to port 0 will cause memory configuration flip-flop to activate 2K ROM 0000-07FF with 62K RAM 0800-FFFF
;Output to port 1 will cause memory configuration flip-flop to activate all RAM 0000-FFFF
;
			org	00000h
			jp 	monitor_cold_start			
;
;The following code is for a system with a serial port.
;Assumes the UART data port address is 02h and control/status address is 03h
;
;The subroutines for the serial port use these variables in RAM:
current_location:	equ	0xdb00		;word variable in RAM
line_count:		equ	0xdb02		;byte variable in RAM
byte_count:		equ	0xdb03		;byte variable in RAM
value_pointer:		equ	0xdb04		;word variable in RAM
current_value:		equ	0xdb06		;word variable in RAM
buffer:			equ	0xdb08		;buffer in RAM -- up to stack area
;Need to have stack in upper RAM, but not in area of CP/M or RAM monitor.
ROM_monitor_stack:	equ	0xdbff		;upper TPA in RAM, below RAM monitor
;
;Subroutine to initialize serial port UART
;Needs to be called only once after computer comes out of reset.
;If called while port is active will cause port to fail.
;16x = 9600 baud
initialize_port:	ld 	a,04eh			;1 stop bit, no parity, 8-bit char, 16x baud
			out 	(3),a			;write to control port
			ld 	a,037h			;enable receive and transmit
			out 	(3),a			;write to control port
			ret
;
;Puts a single char (byte value) on serial output
;Call with char to send in A register. Uses B register
write_char:		ld	b,a			;store char
write_char_loop:	in	a,(3)			;check if OK to send
			and	001h			;check TxRDY bit
			jp 	z,write_char_loop	;loop if not set
			ld 	a,b			;get char back
			out 	(2),a			;send to output
			ret				;returns with char in a
;
;Subroutine to write a zero-terminated string to serial output
;Pass address of string in HL register
;No error checking
write_string:		in 	a,(3)			;read status
			and 	001h			;check TxRDY bit
			jp 	z,write_string		;loop if not set
			ld 	a,(hl)			;get char from string
			and 	a			;check if 0
			ret 	z			;yes, finished
			out 	(2),a			;no, write char to output
			inc 	hl			;next char in string
			jp 	write_string		;start over
;
;Binary loader. Receive a binary file, place in memory.
;Address of load passed in HL, length of load (= file length) in BC
bload:			in 	a,(3)			;get status
			and 	002h			;check RxRDY bit
			jp 	z,bload			;not ready, loop
			in	a,(2)
			ld	(hl),a
			inc	hl
			dec	bc			;byte counter
			ld	a,b			;need to test BC this way because
			or	c			;dec rp instruction does not change flags
			jp	nz,bload
			ret
;
;Binary dump to port. Send a stream of binary data from memory to serial output
;Address of dump passed in HL, length of dump in BC
bdump:			in 	a,(3)			;get status
			and 	001h			;check TxRDY bit
			jp 	z,bdump			;not ready, loop
			ld	a,(hl)
			out	(2),a
			inc	hl
			dec	bc
			ld	a,b			;need to test this way because
			or	c			;dec rp instruction does not change flags
			jp	nz,bdump
			ret
;
;Subroutine to get a string from serial input, place in buffer.
;Buffer address passed in HL reg.
;Uses A,BC,DE,HL registers (including calls to other subroutines).
;Line entry ends by hitting return key. Return char not included in string (replaced by zero).
;Backspace editing OK. No error checking.
;
get_line:		ld	c,000h			;line position
			ld	a,h			;put original buffer address in de
			ld	d,a			;after this don't need to preserve hl
			ld	a,l			;subroutines called don't use de
			ld	e,a
get_line_next_char:	in 	a,(3)			;get status
			and 	002h			;check RxRDY bit
			jp 	z,get_line_next_char	;not ready, loop
			in 	a,(2)			;get char
			cp	00dh			;check if return
			ret	z			;yes, normal exit
			cp	07fh			;check if backspace (VT102 keys)
			jp	z,get_line_backspace	;yes, jump to backspace routine
			cp	008h			;check if backspace (ANSI keys)
			jp	z,get_line_backspace	;yes, jump to backspace
			call	write_char		;put char on screen
			ld	(de),a			;store char in buffer
			inc	de			;point to next space in buffer
			inc	c			;inc counter
			ld	a,000h
			ld	(de),a			;leaves a zero-terminated string in buffer
			jp	get_line_next_char
get_line_backspace:	ld	a,c			;check current position in line
			cp	000h			;at beginning of line?
			jp	z,get_line_next_char	;yes, ignore backspace, get next char
			dec	de			;no, erase char from buffer
			dec	c			;back up one
			ld	a,000h			;put a zero in buffer where the last char was
			ld	(de),a
			ld	hl,erase_char_string	;ANSI sequence to delete one char from line
			call	write_string		;transmits sequence to backspace and erase char
			jp	get_line_next_char
;
;Creates a two-char hex string from the byte value passed in register A
;Location to place string passed in HL
;String is zero-terminated, stored in 3 locations starting at HL
;Also uses registers b,d, and e
byte_to_hex_string:	ld	b,a			;store original byte
			srl	a			;shift right 4 times, putting
			srl	a			;high nybble in low-nybble spot
			srl	a			;and zeros in high-nybble spot
			srl	a
			ld	d,000h			;prepare for 16-bit addition
			ld	e,a			;de contains offset
			push	hl			;temporarily store string target address
			ld	hl,hex_char_table	;use char table to get high-nybble character
			add	hl,de			;add offset to start of table
			ld	a,(hl)			;get char
			pop	hl			;get string target address
			ld	(hl),a			;store first char of string
			inc	hl			;point to next string target address
			ld	a,b			;get original byte back from reg b
			and	00fh			;mask off high-nybble
			ld	e,a			;d still has 000h, now de has offset
			push	hl			;temp store string target address
			ld	hl,hex_char_table	;start of table
			add	hl,de			;add offset
			ld	a,(hl)			;get char
			pop	hl			;get string target address
			ld	(hl),a			;store second char of string
			inc	hl			;point to third location
			ld	a,000h			;zero to terminate string
			ld	(hl),a			;store the zero
			ret				;done
;
;Converts a single ASCII hex char to a nybble value
;Pass char in reg A. Letter numerals must be upper case.
;Return nybble value in low-order reg A with zeros in high-order nybble if no error.
;Return 0ffh in reg A if error (char not a valid hex numeral).
;Also uses b, c, and hl registers.
hex_char_to_nybble:	ld	hl,hex_char_table
			ld	b,00fh			;no. of valid characters in table - 1.
			ld	c,000h			;will be nybble value
hex_to_nybble_loop:	cp	(hl)			;character match here?
			jp	z,hex_to_nybble_ok	;match found, exit
			dec	b			;no match, check if at end of table
			jp	m,hex_to_nybble_err	;table limit exceded, exit with error
			inc	c			;still inside table, continue search
			inc	hl
			jp	hex_to_nybble_loop
hex_to_nybble_ok:	ld	a,c			;put nybble value in a
			ret
hex_to_nybble_err:	ld	a,0ffh			;error value
			ret
;
;Converts a hex character pair to a byte value
;Called with location of high-order char in HL
;If no error carry flag clear, returns with byte value in register A, and
;HL pointing to next mem location after char pair.
;If error (non-hex char) carry flag set, HL pointing to invalid char
hex_to_byte:		ld	a,(hl)			;location of character pair
			push	hl			;store hl (hex_char_to_nybble uses it)
			call	hex_char_to_nybble
			pop	hl			;returns with nybble value in a reg, or 0ffh if error
			cp	0ffh			;non-hex character?
			jp	z,hex_to_byte_err	;yes, exit with error
			sla	a			;no, move low order nybble to high side
			sla	a
			sla	a
			sla	a
			ld	d,a			;store high-nybble
			inc	hl			;get next character of the pair
			ld	a,(hl)
			push	hl			;store hl
			call	hex_char_to_nybble
			pop	hl
			cp	0ffh			;non-hex character?
			jp	z,hex_to_byte_err	;yes, exit with error
			or	d			;no, combine with high-nybble
			inc	hl			;point to next memory location after char pair
			scf
			ccf				;no-error exit (carry = 0)
			ret
hex_to_byte_err:	scf				;error, carry flag set
			ret
hex_char_table:		defm	"0123456789ABCDEF"	;ASCII hex table
;
;Subroutine to get a two-byte address from serial input.
;Returns with address value in HL
;Uses locations in RAM for buffer and variables
address_entry:		ld	hl,buffer		;location for entered string
			call	get_line		;returns with address string in buffer
			ld	hl,buffer		;location of stored address entry string
			call	hex_to_byte		;will get high-order byte first
			jp	c, address_entry_error	;if error, jump
			ld	(current_location+1),a	;store high-order byte, little-endian
			ld	hl,buffer+2		;point to low-order hex char pair
			call	hex_to_byte		;get low-order byte
			jp	c, address_entry_error	;jump if error
			ld	(current_location),a	;store low-order byte in lower memory
			ld	hl,(current_location)	;put memory address in hl
			ret
address_entry_error:	ld	hl,address_error_msg
			call	write_string
			jp	address_entry
;
;Subroutine to get a decimal string, return a word value
;Calls decimal_string_to_word subroutine
decimal_entry:		ld	hl,buffer
			call	get_line		;returns with DE pointing to terminating zero
			ld	hl,buffer
			call	decimal_string_to_word
			ret	nc			;no error, return with word in hl
			ld	hl,decimal_error_msg	;error, try again
			call	write_string
			jp	decimal_entry
;
;Subroutine to convert a decimal string to a word value
;Call with address of string in HL, pointer to end of string in DE
;Carry flag set if error (non-decimal char)
;Carry flag clear, word value in HL if no error.
decimal_string_to_word:	ld	b,d
			ld	c,e			;use BC as string pointer
			ld	(current_location),hl	;store addr. of start of buffer in RAM word variable
			ld	hl,000h			;starting value zero
			ld	(current_value),hl
			ld	hl,decimal_place_value	;pointer to values
			ld	(value_pointer),hl
decimal_next_char:	dec	bc			;next char in string (moving right to left)
			ld	hl,(current_location)	;check if at end of decimal string
			scf				;get ready to subtract de from buffer addr.
			ccf				;set carry to zero (clear)
			sbc	hl,bc			;keep going if bc > or = hl (buffer address)
			jp	c,decimal_continue	;borrow means bc > hl
			jp	z,decimal_continue	;z means bc = hl
			ld	hl,(current_value)	;return if de < buffer address (no borrow)
			scf				;get value back from RAM variable
			ccf
			ret				;return with carry clear, value in hl
decimal_continue:	ld	a,(bc)			;next char in string (right to left)
			sub	030h			;ASCII value of zero char
			jp	m,decimal_error		;error if char value less than 030h
			cp	00ah			;error if byte value > or = 10 decimal
			jp	p,decimal_error		;a reg now has value of decimal numeral
			ld	hl,(value_pointer)	;get value to add an put in de
			ld	e,(hl)			;little-endian (low byte in low memory)
			inc	hl
			ld	d,(hl)
			inc	hl			;hl now points to next value
			ld	(value_pointer),hl
			ld	hl,(current_value)	;get back current value
decimal_add:		dec	a			;add loop to increase total value
			jp	m,decimal_add_done	;end of multiplication
			add	hl,de
			jp	decimal_add
decimal_add_done:	ld	(current_value),hl
			jp	decimal_next_char
decimal_error:		scf
			ret
			jp	decimal_add
decimal_place_value:	defw	1,10,100,1000,10000
;
;Memory dump
;Displays a 256-byte block of memory in 16-byte rows.
;Called with address of start of block in HL
memory_dump:		ld	(current_location),hl	;store address of block to be displayed
			ld	a,000h
			ld	(byte_count),a		;initialize byte count
			ld	(line_count),a		;initialize line count
			jp	dump_new_line
dump_next_byte:		ld	hl,(current_location)	;get byte address from storage,
			ld	a,(hl)			;get byte to be converted to string
			inc	hl			;increment address and
			ld	(current_location),hl	;store back
			ld	hl,buffer		;location to store string
			call	byte_to_hex_string	;convert
			ld	hl,buffer		;display string
			call	write_string
			ld	a,(byte_count)		;next byte
			inc	a
			jp	z,dump_done		;stop when 256 bytes displayed
			ld	(byte_count),a		;not finished yet, store
			ld	a,(line_count)		;end of line (16 characters)?
			cp	00fh			;yes, start new line
			jp	z,dump_new_line
			inc	a			;no, increment line count
			ld	(line_count),a
			ld	a,020h			;print space
			call	write_char
			jp	dump_next_byte		;continue
dump_new_line:		ld	a,000h			;reset line count to zero
			ld	(line_count),a			
			call	write_newline
			ld	hl,(current_location)	;location of start of line
			ld	a,h			;high byte of address
			ld	hl, buffer
			call	byte_to_hex_string	;convert
			ld	hl,buffer
			call	write_string		;write high byte
			ld	hl,(current_location)
			ld	a,l			;low byte of address
			ld	hl, buffer
			call	byte_to_hex_string	;convert
			ld	hl,buffer
			call	write_string		;write low byte
			ld	a,020h			;space
			call	write_char
			jp	dump_next_byte		;now write 16 bytes
dump_done:		ld	a,000h
			ld	hl,buffer
			ld	(hl),a			;clear buffer of last string
			call	write_newline
			ret
;
;Memory load
;Loads RAM memory with bytes entered as hex characters
;Called with address to start loading in HL
;Displays entered data in 16-byte rows.
memory_load:		ld	(current_location),hl
			ld	hl,data_entry_msg
			call	write_string
			jp	load_new_line
load_next_char:		call	get_char
			cp	00dh			;return?
			jp	z,load_done		;yes, quit
			ld	(buffer),a
			call	get_char
			cp	00dh			;return?
			jp	z,load_done		;yes, quit
			ld	(buffer+1),a
			ld	hl,buffer
			call	hex_to_byte
			jp	c,load_data_entry_error	;non-hex character
			ld	hl,(current_location)	;get byte address from storage,
			ld	(hl),a			;store byte
			inc	hl			;increment address and
			ld	(current_location),hl	;store back
			ld	a,(buffer)
			call	write_char
			ld	a,(buffer+1)
			call	write_char
			ld	a,(line_count)		;end of line (16 characters)?
			cp	00fh			;yes, start new line
			jp	z,load_new_line
			inc	a			;no, increment line count
			ld	(line_count),a
			ld	a,020h			;print space
			call	write_char
			jp	load_next_char		;continue
load_new_line:		ld	a,000h			;reset line count to zero
			ld	(line_count),a
			call	write_newline
			jp	load_next_char		;continue
load_data_entry_error:	call	write_newline
			ld	hl,data_error_msg
			call	write_string
			ret
load_done:		call	write_newline
			ret
;
;Get one ASCII character from the serial port.
;Returns with char in A reg. No error checking.
get_char:		in 	a,(3)			;get status
			and 	002h			;check RxRDY bit
			jp 	z,get_char		;not ready, loop
			in 	a,(2)			;get char
			ret
;
;Subroutine to start a new line
write_newline:		ld	a,00dh			;ASCII carriage return character
			call	write_char
			ld	a,00ah			;new line (line feed) character
			call	write_char
			ret
;
;Subroutine to read one disk sector (256 bytes)
;Address to place data passed in HL
;LBA bits 0 to 7 passed in C, bits 8 to 15 passed in B
;LBA bits 16 to 23 passed in E
disk_read:
rd_status_loop_1:	in	a,(0fh)		;check status
			and	80h		;check BSY bit
			jp	nz,rd_status_loop_1	;loop until not busy
rd_status_loop_2:	in	a,(0fh)		;check	status
			and	40h		;check DRDY bit
			jp	z,rd_status_loop_2	;loop until ready
			ld	a,01h		;number of sectors = 1
			out	(0ah),a		;sector count register
			ld	a,c
			out	(0bh),a		;lba bits 0 - 7
			ld	a,b
			out	(0ch),a		;lba bits 8 - 15
			ld	a,e
			out	(0dh),a		;lba bits 16 - 23
			ld	a,11100000b	;LBA mode, select drive 0
			out	(0eh),a		;drive/head register
			ld	a,20h		;Read sector command
			out	(0fh),a
rd_wait_for_DRQ_set:	in	a,(0fh)		;read status
			and	08h		;DRQ bit
			jp	z,rd_wait_for_DRQ_set	;loop until bit set
rd_wait_for_BSY_clear:	in	a,(0fh)
			and	80h
			jp	nz,rd_wait_for_BSY_clear
			in	a,(0fh)		;clear INTRQ
read_loop:		in	a,(08h)		;get data
			ld	(hl),a
			inc	hl
			in	a,(0fh)		;check status
			and	08h		;DRQ bit
			jp	nz,read_loop	;loop until cleared
			ret
;
;Subroutine to write one disk sector (256 bytes)
;Address of data to write to disk passed in HL
;LBA bits 0 to 7 passed in C, bits 8 to 15 passed in B
;LBA bits 16 to 23 passed in E
disk_write:
wr_status_loop_1:	in	a,(0fh)		;check status
			and	80h		;check BSY bit
			jp	nz,wr_status_loop_1	;loop until not busy
wr_status_loop_2:	in	a,(0fh)		;check	status
			and	40h		;check DRDY bit
			jp	z,wr_status_loop_2	;loop until ready
			ld	a,01h		;number of sectors = 1
			out	(0ah),a		;sector count register
			ld	a,c
			out	(0bh),a		;lba bits 0 - 7
			ld	a,b
			out	(0ch),a		;lba bits 8 - 15
			ld	a,e
			out	(0dh),a		;lba bits 16 - 23
			ld	a,11100000b	;LBA mode, select drive 0
			out	(0eh),a		;drive/head register
			ld	a,30h		;Write sector command
			out	(0fh),a
wr_wait_for_DRQ_set:	in	a,(0fh)		;read status
			and	08h		;DRQ bit
			jp	z,wr_wait_for_DRQ_set	;loop until bit set			
write_loop:		ld	a,(hl)
			out	(08h),a		;write data
			inc	hl
			in	a,(0fh)		;read status
			and	08h		;check DRQ bit
			jp	nz,write_loop	;write until bit cleared
wr_wait_for_BSY_clear:	in	a,(0fh)
			and	80h
			jp	nz,wr_wait_for_BSY_clear
			in	a,(0fh)		;clear INTRQ
			ret
;
;Strings used in subroutines
length_entry_string:	defm	"Enter length of file to load (decimal): ",0
dump_entry_string:	defm	"Enter no. of bytes to dump (decimal): ",0
LBA_entry_string:	defm	"Enter LBA (decimal, 0 to 65535): ",0
erase_char_string:	defm	008h,01bh,"[K",000h	;ANSI sequence for backspace, erase to end of line.
address_entry_msg:	defm	"Enter 4-digit hex address (use upper-case A through F): ",0
address_error_msg:	defm	"\r\nError: invalid hex character, try again: ",0
data_entry_msg:		defm	"Enter hex bytes, hit return when finished.\r\n",0
data_error_msg:		defm	"Error: invalid hex byte.\r\n",0
decimal_error_msg:	defm	"\r\nError: invalid decimal number, try again: ",0
;
;Simple monitor program for CPUville Z80 computer with serial interface.
monitor_cold_start:	ld	sp,ROM_monitor_stack
			call	initialize_port
			ld	hl,monitor_message
			call	write_string
monitor_warm_start:	call	write_newline		;routine program return here to avoid re-initialization of port
			ld	a,03eh			;cursor symbol
			call	write_char
			ld	hl,buffer
			call	get_line		;get monitor input string (command)
			call	write_newline
			call	parse			;interprets command, returns with address to jump to in HL
			jp	(hl)
;
;Parses an input line stored in buffer for available commands as described in parse table.
;Returns with address of jump to action for the command in HL
parse:			ld	bc,parse_table		;bc is pointer to parse_table
parse_start:		ld	a,(bc)			;get pointer to match string from parse table
			ld	e,a
			inc	bc
			ld	a,(bc)			
			ld	d,a			;de will is pointer to strings for matching
			ld	a,(de)			;get first char from match string
			or	000h			;zero?
			jp	z,parser_exit		;yes, exit no_match
			ld	hl,buffer		;no, parse input string 
match_loop:		cp	(hl)			;compare buffer char with match string char
			jp	nz,no_match		;no match, go to next match string
			or	000h			;end of strings (zero)?
			jp	z,parser_exit		;yes, matching string found
			inc	de			;match so far, point to next char in match string
			ld	a,(de)			;get next character from match string
			inc	hl			;and point to next char in input string
			jp	match_loop		;check for match
no_match:		inc	bc			;skip over jump target to
			inc	bc
			inc	bc			;get address of next matching string
			jp	parse_start
parser_exit:		inc	bc			;skip to address of jump for match
			ld	a,(bc)
			ld	l,a
			inc	bc
			ld	a,(bc)
			ld	h,a			;returns with jump address in hl
			ret
;
;Actions to be taken on match
;
;Memory dump program
;Input 4-digit hexadecimal address
;Calls memory_dump subroutine
dump_jump:		ld	hl,dump_message		;Display greeting
			call	write_string
			ld	hl,address_entry_msg	;get ready to get address
			call	write_string
			call	address_entry		;returns with address in HL
			call	write_newline
			call	memory_dump
			jp	monitor_warm_start
;
;Hex loader, displays formatted input
load_jump:		ld	hl,load_message		;Display greeting
			call	write_string		;get address to load
			ld	hl,address_entry_msg	;get ready to get address
			call	write_string
			call	address_entry
			call	write_newline
			call	memory_load
			jp	monitor_warm_start
;
;Jump and run do the same thing: get an address and jump to it.
run_jump:		ld	hl,run_message		;Display greeting
			call	write_string
			ld	hl,address_entry_msg	;get ready to get address
			call	write_string
			call	address_entry
			jp	(hl)
;
;Help and ? do the same thing, display the available commands
help_jump:		ld	hl,help_message
			call	write_string
			ld	bc,parse_table		;table with pointers to command strings
help_loop:		ld	a,(bc)			;displays the strings for matching commands,
			ld	l,a			;getting the string addresses from the
			inc	bc			;parse table
			ld	a,(bc)			;pass address of string to hl through a reg
			ld	h,a
			ld	a,(hl)			;hl now points to start of match string
			or	000h			;exit if no_match string
			jp	z,help_done
			push	bc			;write_char uses b register
			ld	a,020h			;space char
			call	write_char
			pop	bc
			call	write_string		;writes match string
			inc	bc			;pass over jump address in table
			inc	bc
			inc	bc
			jp	help_loop
help_done:		jp	monitor_warm_start
;
;Binary file load. Need both address to load and length of file
bload_jump:		ld	hl,bload_message
			call	write_string
			ld	hl,address_entry_msg
			call	write_string
			call	address_entry
			call	write_newline
			push	hl
			ld	hl,length_entry_string
			call	write_string
			call	decimal_entry
			ld	b,h
			ld	c,l
			ld	hl,bload_ready_message
			call	write_string
			pop	hl
			call	bload
			jp	monitor_warm_start
;
;Binary memory dump. Need address of start of dump and no. bytes
bdump_jump:		ld	hl,bdump_message
			call	write_string
			ld	hl,address_entry_msg
			call	write_string
			call	address_entry
			call	write_newline
			push	hl
			ld	hl,dump_entry_string
			call	write_string
			call	decimal_entry
			ld	b,h
			ld	c,l
			ld	hl,bdump_ready_message
			call	write_string
			call	get_char
			pop	hl
			call	bdump
			jp	monitor_warm_start
;Disk read. Need memory address to place data, LBA of sector to read
diskrd_jump:		ld	hl,diskrd_message
			call	write_string
			ld	hl,address_entry_msg
			call	write_string
			call	address_entry
			call	write_newline
			push	hl
			ld	hl,LBA_entry_string
			call	write_string
			call	decimal_entry
			ld	b,h
			ld	c,l
			ld	e,00h
			pop	hl
			call	disk_read
			jp	monitor_warm_start
diskwr_jump:		ld	hl,diskwr_message
			call	write_string
			ld	hl,address_entry_msg
			call	write_string
			call	address_entry
			call	write_newline
			push	hl
			ld	hl,LBA_entry_string
			call	write_string
			call	decimal_entry
			ld	b,h
			ld	c,l
			ld	e,00h
			pop	hl
			call	disk_write
			jp	monitor_warm_start
cpm_jump:		ld	hl,0800h
			ld	bc,0000h
			ld	e,00h
			call	disk_read
			jp	0800h
;Prints message for no match to entered command
no_match_jump:		ld	hl,no_match_message
			call	write_string
			ld	hl, buffer
			call	write_string
			jp	monitor_warm_start
;
;Monitor data structures:
;
monitor_message: 	defm	"\r\nROM ver. 8\r\n",0
no_match_message:	defm	"? ",0
help_message:		defm	"Commands implemented:\r\n",0
dump_message:		defm	"Displays a 256-byte block of memory.\r\n",0
load_message:		defm	"Enter hex bytes starting at memory location.\r\n",0
run_message:		defm	"Will jump to (execute) program at address entered.\r\n",0
bload_message:		defm	"Loads a binary file into memory.\r\n",0
bload_ready_message:	defm	"\n\rReady to receive, start transfer.",0
bdump_message:		defm	"Dumps binary data from memory to serial port.\r\n",0
bdump_ready_message:	defm	"\n\rReady to send, hit any key to start.",0
diskrd_message:		defm	"Reads one sector from disk to memory.\r\n",0
diskwr_message:		defm	"Writes one sector from memory to disk.\r\n",0
;Strings for matching:
dump_string:		defm	"dump",0
load_string:		defm	"load",0
jump_string:		defm	"jump",0
run_string:		defm	"run",0
question_string:	defm	"?",0
help_string:		defm	"help",0
bload_string:		defm	"bload",0
bdump_string:		defm	"bdump",0
diskrd_string:		defm	"diskrd",0
diskwr_string:		defm	"diskwr",0
cpm_string:		defm	"cpm",0
no_match_string:	defm	0,0
;Table for matching strings to jumps
parse_table:		defw	dump_string,dump_jump,load_string,load_jump
			defw	jump_string,run_jump,run_string,run_jump
			defw	question_string,help_jump,help_string,help_jump
			defw	bload_string,bload_jump,bdump_string,bdump_jump
			defw	diskrd_string,diskrd_jump,diskwr_string,diskwr_jump
			defw	cpm_string,cpm_jump
			defw	no_match_string,no_match_jump

