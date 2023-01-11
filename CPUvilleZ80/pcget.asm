;
;  PCGET - This CP/M program receives a file from a PC via a serial 
;  port and writes it to a file on the CP/M system. The file transfer uses
;  the XMODEM protocol. 
;
;  Note this program is gutted from the Ward Christenson Modem program.
;
;  Hacked together by Mike Douglas for the Altair 2SIO serial interface board.
;	Ver	Date	Desc
;	1.0    11/7/12	Initial version
;	1.1    2/20/14  Allow transfer to occur over 2SIO port B
;	1.2   12/21/14	Send NAK immediately after file open to speed
;			up the start-up of file transfer (four second
;			delay otherwise).
;	2.0	5/11/17	Update for CPUVille system
;  Serial Port Equates
	CPU	8080
SIOACR:	EQU	003H		;2SIO port A control register
SIOADR:	EQU	002H		;2SIO port A data register
SIOBCR:	EQU	012H		;2SIO port B control register
SIOBDR	EQU	013H		;2SIO port B data register

XMTMASK: EQU	1		;MASK TO ISOLATE XMIT READY BIT
XMTRDY:	 EQU	1		;VALUE WHEN READY
RCVMASK:	EQU	2		;MASK TO ISOLATE RECEIVE READY BIT
RCVRDY:	 EQU	2		;BIT ON WHEN READY

;  Transfer related equates

SOH:	EQU	1
EOT:	EQU	4
ACK:	EQU	6
NAK:	EQU	15H
CTRLC:	EQU	3		;Control-C
LF:	EQU	10
CR:	EQU	13

	ORG	100H

;  Verify a file name was specified

	lda	PARAM1		;A=1st character of parameter 1
	cpi	' '		;make sure something entered
	jnz	doXfer
	lxi	d,mHelp		;display usage message
	mvi	c,PRINT
	call	BDOS
	ret			;return to CPM

	
;  doXfer - Switch to local stack and do the transfer

doXfer:
	lxi	d,mSendA	;port a send message
	LXI	H,0		;HL=0
	DAD	SP		;HL=STACK FROM CP/M
	SHLD	STACK		;..SAVE IT
	LXI	SP,STACK	;SP=MY STACK
	xra	a
	sta	SECTNO		;init sector number to zero
	CALL	INIT_ACIA	;MASTER RESET THE ACIA
	MVI	C,PRINT		;print the send message
	CALL	BDOS		;PRINT ID MESSAGE

;  GOBBLE UP GARBAGE CHARS FROM THE LINE

purge:	MVI	B,1		;times out after 1 second if no data
	CALL	RECV
	jc	RECEIVE_FILE	;line is clear, go receive the file
	cpi	CTRLC		;exit if abort requested
	jz	abort
	jmp	purge
;
;**************RECEIVE FILE****************
;
RECEIVE_FILE:
	CALL	ERASE_OLD_FILE
	CALL	MAKE_NEW_FILE
	MVI	A,NAK
	CALL	SEND		;SEND NAK

RECV_LOOP:
RECV_HDR:
	MVI	B,3		;3 SEC TIMEOUT
	CALL	RECV
	JNC	RHNTO		;NO TIMEOUT

RECV_HDR_TIMEOUT:
RECV_SECT_ERR:			;PURGE THE LINE OF INPUT CHARS
	MVI	B,1		;1 SEC W/NO CHARS
	CALL	RECV
	JNC	RECV_SECT_ERR 	;LOOP UNTIL SENDER DONE
	MVI	A,NAK
	CALL	SEND		;SEND NAK
	JMP	RECV_HDR

;GOT CHAR - MUST BE SOH OR CTRL-C TO ABORT

RHNTO:	CPI	SOH
	JZ	GOT_SOH
	cpi	CTRLC		;control-c to abort?
	jz	abort
	CPI	EOT
	JZ	GOT_EOT
	JMP	RECV_SECT_ERR

GOT_SOH:
	MVI	B,1
	CALL	RECV
	JC	RECV_HDR_TIMEOUT
	MOV	D,A		;D=BLK #
	MVI	B,1
	CALL	RECV		;GET CMA'D SECT #
	JC	RECV_HDR_TIMEOUT
	CMA
	CMP	D		;GOOD SECTOR #?
	JZ	RECV_SECTOR
	JMP	RECV_SECT_ERR

;  Receive Sector

RECV_SECTOR:
	MOV	A,D		;GET SECTOR #
	STA	RSECTNO
	MVI	C,0		;INIT CKSUM
	LXI	H,80H		;POINT TO BUFFER
RECV_CHAR:
	MVI	B,1		;1 SEC TIMEOUT
	CALL	RECV		;GET CHAR
	JC	RECV_HDR_TIMEOUT
	MOV	M,A		;STORE CHAR
	INR	L		;DONE?
	JNZ	RECV_CHAR

;VERIFY CHECKSUM

	MOV	D,C		;SAVE CHECKSUM
	MVI	B,1		;TIMEOUT
	CALL	RECV		;GET CHECKSUM
	JC	RECV_HDR_TIMEOUT
	CMP	D		;CHECK
	JNZ	RECV_SECT_ERR
;
;GOT A SECTOR, WRITE IF = 1+PREV SECTOR
;
	LDA	RSECTNO
	MOV	B,A		;SAVE IT
	LDA	SECTNO		;GET PREV
	INR	A		;CALC NEXT SECTOR #
	CMP	B		;MATCH?
	JNZ	DO_ACK

;GOT NEW SECTOR - WRITE IT

	LXI	D,FCB
	MVI	C,WRITE
	CALL	BDOS
	ORA	A
	JNZ	WRITE_ERROR
	LDA	RSECTNO
	STA	SECTNO		;UPDATE SECTOR #
DO_ACK:	MVI	A,ACK
	CALL	SEND
	JMP	RECV_LOOP

WRITE_ERROR:
	CALL	ERXIT
	DB	13,10,10,"Error Writing File",13,10,'$'

GOT_EOT:
	MVI	A,ACK		;ACK THE EOT
	CALL	SEND
	LXI	D,FCB
	MVI	C,CLOSE
	CALL	BDOS
	INR	A
	JNZ	XFER_CPLT
	CALL	ERXIT
	DB	13,10,10,"Error Closing File",13,10,'$'
;
ERASE_OLD_FILE:
	LXI	D,FCB
	MVI	C,SRCHF		;SEE IF IT EXISTS
	CALL	BDOS
	INR	A		;FOUND?
	RZ			;NO, RETURN
ERAY:	LXI	D,FCB
	MVI	C,ERASE
	CALL	BDOS
	RET
;
MAKE_NEW_FILE:
	LXI	D,FCB
	MVI	C,MAKE
	CALL	BDOS
	INR	A		;FF=BAD
	RNZ			;OPEN OK

;DIRECTORY FULL - CAN'T MAKE FILE
	CALL	ERXIT
	DB	13,10,10,"Error - Cannot Make File",13,10
	DB	"(directory must be full)",13,10,'$'
;
; S U B R O U T I N E S
;
; - - - - - - - - - - - - - - -

;EXIT PRINTING MESSAGE FOLLOWING 'CALL ERXIT'

ERXIT:	POP	D		;GET MESSAGE
	MVI	C,PRINT
	CALL	BDOS		;PRINT MESSAGE
EXIT:	LHLD	STACK		;GET ORIGINAL STACK
	SPHL			;RESTORE IT
;	RET			;--EXIT-- TO CP/M
	jmp	0		; For interrupting system

; - - - - - - - - - - - - - - -
;MODEM RECV
;-------------------------------------
RECV:	PUSH	D		;SAVE
MSEC:	
;	lxi	d,(159 shl 8)	;49 cycle loop, 6.272ms/wrap * 159 = 1 second
	lxi	d,09F00H	;49 cycle loop, 6.272ms/wrap * 159 = 1 second

;  port A input

MWTI:	IN	SIOACR
	ANI	RCVMASK
	CPI	RCVRDY
	JZ	MCHAR		;GOT CHAR
	DCR	E		;COUNT DOWN
	JNZ	MWTI		;FOR TIMEOUT
	DCR	D
	JNZ	MWTI
	DCR	B		;DCR # OF SECONDS
	JNZ	MSEC

;MODEM TIMED OUT RECEIVING

	POP	D		;RESTORE D,E
	STC			;CARRY SHOWS TIMEOUT
	RET

;GOT MODEM CHAR

MCHAR:	IN	SIOADR
	POP	D		;RESTORE DE
	PUSH	PSW		;CALC CHECKSUM
	ADD	C
	MOV	C,A
	POP	PSW
	ORA	A		;TURN OFF CARRY TO SHOW NO TIMEOUT
	RET


;GOT MODEM CHAR

MCHARB:	IN	SIOBDR
	POP	D		;RESTORE DE
	PUSH	PSW		;CALC CHECKSUM
	ADD	C
	MOV	C,A
	POP	PSW
	ORA	A		;TURN OFF CARRY TO SHOW NO TIMEOUT
	RET

; - - - - - - - - - - - - - - -
;MODEM SEND CHAR ROUTINE
;----------------------------------
;
SEND:	PUSH	PSW		;CHECK IF MONITORING OUTPUT
	ADD	C		;CALC CKSUM
	MOV	C,A

SENDW:	IN	SIOACR
	ANI	XMTMASK
	CPI	XMTRDY
	JNZ	SENDW
	POP	PSW		;GET CHAR
	OUT	SIOADR
	RET


; INITITIALIZE THE SERIAL PORT

INIT_ACIA:
;	mvi	a,003h		;don't reset console port
;	out	SIOACR
	mvi	a,015h		;rts on, 8N1
	out	SIOACR
	ret


mSendA:	db	"Send the file now using "
	db	"XMODEM...",'$'

mHelp:	db	CR,LF,"PCGET 2.0 for CPUVille",CR,LF,LF
	db	"Usage: PCGET file.ext",CR,LF
	DB	'$'

;DONE - CLOSE UP SHOP

XFER_CPLT:
	CALL	ERXIT
	DB	13,10,10,"Transfer Complete",13,10,'$'

abort:	call	ERXIT
	db	13,10,10,"Transfer Aborted",13,10,'$'

	DS	40	;STACK AREA
STACK:	DS	2	;STACK POINTER
RSECTNO:	DS	1	;RECEIVED SECTOR NUMBER
SECTNO:	DS	1	;CURRENT SECTOR NUMBER 

;
; BDOS EQUATES (VERSION 2)
;
RDCON	EQU	1
WRCON	EQU	2
PRINT	EQU	9
CONST	EQU	11	;CONSOLE STAT
OPEN	EQU	15	;0FFH=NOT FOUND
CLOSE	EQU	16	;   "	"
SRCHF	EQU	17	;   "	"
SRCHN	EQU	18	;   "	"
ERASE	EQU	19	;NO RET CODE
READ	EQU	20	;0=OK, 1=EOF
WRITE	EQU	21	;0=OK, 1=ERR, 2=?, 0FFH=NO DIR SPC
MAKE	EQU	22	;0FFH=BAD
REN	EQU	23	;0FFH=BAD
STDMA	EQU	26
BDOS	EQU	5
REIPL	EQU	0
FCB	EQU	5CH	;DEFAULT FCB
PARAM1	EQU	FCB+1	;COMMAND LINE PARAMETER 1 IN FCB
PARAM2	EQU	PARAM1+16	;COMMAND LINE PARAMETER 2
	END
