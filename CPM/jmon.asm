;
; JMON - Jeff's Monitor Program
; ------------------------------
;
; A machine language monitor program for the CPUville Z80 Single Board Computer.
; Ported from the version for my Z80 SBC computer design.
;
; It should also run on any CP/M system when built using CP/M BDOS
; calls for input output (if the hardware has a Z80 processor).
;
; Copyright (C) 2014-2024 by Jeff Tranter <tranter@pobox.com>
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;   http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
;
; Commands:
;   COPY: C <START> <END> <DEST>
;   DUMP: D <START>
;   FILL: F <START> <END> <DATA>
;   GO: G <ADDRESS>
;   INFO: I
;   CHECKSUM: K <START> <END>
;   CLR SCREEN: L
;   SCOPE LOOP: P <A/I> <R/W> <Y/N> <addr> [<data>]
;   QUIT: Q
;   REGISTERS: R
;   SEARCH: S <START> <END> <DATA>
;   TEST: T <START> <END>
;   UNASSEMBLE: U <ADDRESS>
;   VERIFY: V <START> <END> <DEST>
;   WRITE: : <ADDRESS> <DATA>...
;   MATH: = <ADDRESS> +/- <ADDRESS>
;   HELP: ?
;
; Revision History
; Version Date         Comments
; 0.1     26-Jun-2021  First version started, based on 8080 version for
;                      Altair, converted to Z80 mnemonics and ported to
;                      Z80 SBC.
; 0.2     05-Jul-2021  Added support for Z80 registers.
;                      Implemented memory (":") command.
;                      Added support for setting register values.
;                      Added scope loop ("P") command.
;                      Implemented math ("=") command.
;                      Implemented search ("S") command.
;                      Implemented verify ("V") command.
; 0.3     10-Jul-2021  Implemented test ("T") command.
;                      Enhanced scope loop command.
;                      Add support for <ESC> to cancel in more commands.
;                      Add memory size in info command.
; 0.4     04-Aug-2021  Implemented unassemble ("U") command.
; 0.5     07-Jun-2024  Add support for using CP/M BDOS calls for input/output.
; 0.6     10-Jun-2024  Add Quit command and new option to scope loop.
;
; TODO:
; Intel or Motorola hex file loader?


; Set either CPUVILLE or CPM to 1 to control whether input/output
; uses the CPUVille board hardware or CP/M BDOS calls.

CPUVILLE: equ   0
CPM:    equ     1

; Defines

if      CPUVILLE
SREG:   equ     03h             ; 8251A UART status register
CREG:   equ     03h             ; 8251A UART control register
DREG:   equ     02h             ; 8251A UART data register
endif

if      CPM
BOOT:   equ     0000H           ; Return to CP/M
BDOS:   equ     0005H           ; CP/M BDOS call
DIO:    equ     06H             ; BDOS call for Direct Console i/o
endif

        org     0100H           ; Start at address 0100 if running from CP/M
;       org     1000H           ; Start at address 1000 if running from ROM monitor

; Constants

prompt: equ     '?'             ; Prompt character
CR:             equ '\r'        ; Carriage Return
NL:             equ '\n'        ; Newline
ESC:            equ $1B         ; Escape
stack:          equ $F000       ; Starting address for stack

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Main program entry point

Start:
;       DI                      ; Disable interrupts
;       IM        1             ; Use interrupt mode 1

; Save registers on entry (for later use by commands like REGISTERS and GO)
        ld      (save_a),a
        ld      a,b
        ld      (save_b),a
        ld      a,c
        ld      (save_c),a
        ld      a,d
        ld      (save_d),a
        ld      a,e
        ld      (save_e),a
        ld      a,h
        ld      (save_h),a
        ld      a,l
        ld      (save_l),a
        ld      a,i
        ld      (save_i),a
        ld      a,r
        ld      (save_r),a
        ld      (save_ix),ix
        ld      (save_iy),iy
        push    af              ; Push A and Flags
        pop     bc              ; Pull A and flags to B,C
        ld      a,c             ; Put flags in A
        ld      (save_f),a      ; Save flags
        ld      hl,stack        ; Set initial value of SP
        ld      a,h
        ld      (save_sp),a
        ld      a,l
        ld      (save_sp+1),a

;       ld      sp,stack        ; Set up stack pointer

        call    ClearScreen     ; Clear screen
        ld      hl,strStartup   ; Print startup message
        call    PrintString

mainloop:
        ld      a,prompt        ; Display command prompt
        call    PrintChar
        call    PrintSpace

        call    GetChar         ; Get a command (letter)
        call    ToUpper         ; Convert to upper case

        cp      'C'
        jr      nz,tryD
        call    CopyCommand
        jr      mainloop
tryD:
        cp      'D'
        jr      nz,tryF
        call    DumpCommand
        jr      mainloop
tryF:
        cp      'F'
        jr      nz,tryG
        call    FillCommand
        jr      mainloop
tryG:
        cp      'G'
        jr      nz,tryI
        call    GoCommand
        jr      mainloop
tryI:
        cp      'I'
        jr      nz,tryK
        call    InfoCommand
        jr      mainloop
tryK:
        cp      'K'
        jr      nz,tryL
        call    ChecksumCommand
        jr      mainloop
tryL:
        cp      'L'
        jr      nz,tryP
        call    ClearCommand
        jr      mainloop
tryP:
        cp      'P'
        jr      nz,tryQ
        call    LoopCommand
        jr      mainloop
tryQ:
        cp      'Q'
        jr      nz,tryR
        call    QuitCommand
        jr      mainloop
tryR:
        cp      'R'
        jr      nz,tryS
        call    RegistersCommand
        jr      mainloop
tryS:
        cp      'S'
        jr      nz,tryT
        call    SearchCommand
        jr      mainloop
tryT:
        cp      'T'
        jr      nz,tryU
        call    TestCommand
        jr      mainloop
tryU:
        cp      'U'
        jr      nz,tryV
        call    UnassembleCommand
        jp      mainloop
tryV:
        cp      'V'
        jr      nz,tryColon
        call    VerifyCommand
        jp      mainloop
tryColon:
        cp      ':'
        jr      nz,tryEquals
        call    MemoryCommand
        jp      mainloop
tryEquals:
        cp      '='
        jr      nz,tryHelp
        call    MathCommand
        jp      mainloop
tryHelp:
        cp      '?'
        jr      nz,invalid
        call    HelpCommand
        jp      mainloop

invalid:
        call    PrintCR
        ld      hl,strInvalid   ; print error message
        call    PrintString
        jp    mainloop


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Commands


; Dump. Dumps memory in hex and ASCII, as below:
;
; 0000: 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 ................
;
; Prompts to continue after a page of lines dumped.

BYTES:equ 16                    ; Number of bytes to dump per line
LINES:equ 24                    ; Number of lines to dump per page

DumpCommand:
        call    PrintChar       ; Echo command back
        call    PrintSpace
        call    GetAddress      ; Prompt for address
        jr      nc,startScreen  ; Carry set indicates <ESC> pressed
        call    PrintCR
        ret
startScreen:
        call    PrintCR
        ld      c,LINES         ; Counts number of lines to be displayed
startLine:
        push    hl              ; Save address in HL
        call    PrintAddress    ; Print address
        ld      a,':'           ; Print colon
        call    PrintChar
        ld      b,BYTES         ; Counts number of bytes to be displayed
doline:
        call    PrintSpace      ; Print space
        ld      a,(hl)          ; Get data at current address
        call    PrintByte       ; Print it
        inc     hl              ; Increment current address
        dec     b               ; Decrement byte count
        jr      nz,doline       ; Continue until full line displayed

; Now dump line of data in ASCII

        call    PrintSpace      ; Print space
        pop     hl              ; Get start address of line
        ld      b,BYTES         ; Counts number of bytes to be displayed
doAscii:
        ld      a,(hl)          ; Get data at current address
        call    PrintAscii      ; Print it
        inc     hl              ; Increment current address
        dec     b               ; Decrement byte count
        jr      nz,doAscii      ; Continue until full line displayed

        call    PrintCR
        dec     c               ; Decrement count of lines printed
        jr      nz,startLine    ; Do the next line
        push    hl              ; Save HL
        ld      hl,strContinue  ; Prompt whether to continue
        call    PrintString
        pop     hl              ; Restore HL
cont:   call    GetChar         ; Get key
        cp      ESC             ; Escape?
        jr      nz,trySpace
        call    PrintCR         ; If so, return
        ret
trySpace:
        cp      ' '             ; Space?
        jr      z,startScreen   ; If so, do next screen
        jr      cont            ; Invalid key, try again


; Go.
; Prompts user for address, restores saved registers, and transfers
; control to address.

GoCommand:
        call    PrintChar       ; Echo command back
        call    PrintSpace
        call    GetAddress      ; Prompt for address
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
contgo:
        ld      (save_pc),hl    ; Save it
        call    PrintCR
                                ; Restore saved registers
        ld      hl,(Start)      ; Push start address of JMON on stack so that if
        push    hl              ; called code returns, will go back to monitor.
        ld      a,(save_pc)
        ld      l,a
        ld      a,(save_pc+1)
        ld      h,a
        push    hl              ; push go address so we can use ret to go to it
                                ; TODO: Restore stack pointer?
        ld      a,(save_h)
        ld      h,a
        ld      a,(save_l)
        ld      l,a
        ld      a,(save_d)
        ld      d,a
        ld      a,(save_e)
        ld      e,a
        ld      a,(save_b)
        ld      b,a
        ld      a,(save_c)
        ld      c,a
        ld      a,(save_i)
        ld      i,a
        ld      a,(save_r)
        ld      r,a
        ld      ix,(save_ix)
        ld      iy,(save_iy)
                                ; TODO: Restore flags?
        ld      a,(save_a)
        ret                     ; This jumps to go address


; CLEAR command.
; Sends code to clear terminal screen.

ClearCommand:
        call    PrintChar       ; Echo command back
        call    ClearScreen     ; Clear screen
        ret


; INFO command.
InfoCommand:
        call    PrintChar       ; Echo command back
        call    PrintCR
        ld      hl,strStartup   ; Print startup message
        call    PrintString

; Detect CPU type. 8080 flags have XX0X0X1X. If different, then assume Z80.

        ld      hl,strCpuType
        call    PrintString

        push    af              ; Push A and flags
        pop     bc              ; Pop B and C, flags now in C
        ld      a,c             ; Ld e flags to A
        and     00101010b       ; Mask out bit we don't care about
        cp      00000010b       ; Should have this value for 8080
        jr      nz,z80          ; If not, assume Z80 CPU
        ld      hl,str8080      ; It is an 8080
prnt:   call    PrintString     ; Print CPU type
        call    PrintCR
        ret
z80:
        ld      hl,strZ80       ; It is a Z80
        jr      prnt

; REGISTERS command.
; Displays saved value of registers
; Prompts for new value for each register.
; <Esc> cancels at any time.
; Example output:
; A=01 BC=4E56 DE=0000 HL=021C F=10101011 SP=6FFE PC=00C3
; IX=1234 IY=2345 I=12 R=34

RegistersCommand:
        call    PrintChar       ; Echo command back
        call    PrintCR
        ld      a,'A'
        call    PrintChar
        call    PrintEquals
        ld      a,(save_a)
        call    PrintByte
        call    PrintSpace
        ld      a,'B'
        call    PrintChar
        ld      a,'C'
        call    PrintChar
        call    PrintEquals
        ld      hl,(save_b)
        call    PrintAddress
        call    PrintSpace
        ld      a,'D'
        call    PrintChar
        ld      a,'E'
        call    PrintChar
        call    PrintEquals
        ld      hl,(save_d)
        call    PrintAddress
        call    PrintSpace
        ld      a,'H'
        call    PrintChar
        ld      a,'L'
        call    PrintChar
        call    PrintEquals
        ld      hl,(save_h)
        call    PrintAddress
        call    PrintSpace

; print flags in binary
; TODO: Print flags symbolically

        ld      a,'F'
        call    PrintChar
        call    PrintEquals
        ld      a,(save_f)      ; Get flags
        ld      l,8             ; Want to test 8 bits
nextbit:
        rla                     ; Rotate into carry bit
        call    c,PrintOne      ; Print "1" if set
        call    nc,PrintZero    ; Print "0" if cleared
        dec     l               ; Decrement counter
        jr      nz,nextbit      ; Repeat until all bits done

        call    PrintSpace
        ld      a,'S'
        call    PrintChar
        ld      a,'P'
        call    PrintChar
        call    PrintEquals
        ld      hl,(save_sp)
        call    PrintAddress
        call    PrintSpace
        ld      a,'P'
        call    PrintChar
        ld      a,'C'
        call    PrintChar
        call    PrintEquals
        ld      hl,(save_pc)
        call    PrintAddress
        call    PrintCR
        ld      a,'I'
        call    PrintChar
        ld      a,'X'
        call    PrintChar
        call    PrintEquals
        ld      hl,(save_ix)
        call    PrintAddress
        call    PrintSpace
        ld      a,'I'
        call    PrintChar
        ld      a,'Y'
        call    PrintChar
        call    PrintEquals
        ld      hl,(save_iy)
        call    PrintAddress
        call    PrintSpace
        ld      a,'I'
        call    PrintChar
        call    PrintEquals
        ld      a,(save_i)
        call    PrintByte
        call    PrintSpace
        ld      a,'R'
        call    PrintChar
        call    PrintEquals
        ld      a,(save_r)
        call    PrintByte
        call    PrintCR

; Now print and prompt for new values

        ld      a,'A'           ; Prompt A=
        call    PrintChar
        call    PrintEquals
        call    GetByte         ; Get new value
        jr      c,EscPressed1   ; Skip if <ESC> pressed
        ld      (save_a),a      ; Otherwise save value
        jr      EnterBC
EscPressed1:
        ld      a,(save_a)
        call    PrintByte
EnterBC:
        call    PrintSpace
        ld      a,'B'
        call    PrintChar
        ld      a,'C'
        call    PrintChar
        call    PrintEquals
        call    GetAddress
        jr      c,EscPressed2   ; Skip if <ESC> pressed
        ld      (save_b),hl     ; Otherwise save value
        jr      EnterDE
EscPressed2:
        ld      hl,(save_b)
        call    PrintAddress
EnterDE:
        call    PrintSpace
        ld      a,'D'
        call    PrintChar
        ld      a,'E'
        call    PrintChar
        call    PrintEquals
        call    GetAddress
        jr      c,EscPressed3   ; Skip if <ESC> pressed
        ld      (save_d),hl     ; Otherwise save value
        jr      EnterHL
EscPressed3:
        ld      hl,(save_d)
        call    PrintAddress
EnterHL:
        call    PrintSpace
        ld      a,'H'
        call    PrintChar
        ld      a,'L'
        call    PrintChar
        call    PrintEquals
        call    GetAddress
        jr      c,EscPressed4   ; Skip if <ESC> pressed
        ld      (save_h),hl     ; Otherwise save value
        jr      EnterF
EscPressed4:
        ld      hl,(save_h)
        call    PrintAddress
EnterF:
        call    PrintSpace
        ld      a,'F'
        call    PrintChar
        call    PrintEquals
        call    GetByte
        jr      c,EscPressed5   ; Skip if <ESC> pressed
        ld      (save_f),a      ; Otherwise save value
        jr      EnterSP
EscPressed5:
        ld      a,(save_f)
        call    PrintByte
EnterSP:
        call    PrintSpace
        ld      a,'S'
        call    PrintChar
        ld      a,'P'
        call    PrintChar
        call    PrintEquals
        call    GetAddress
        jr      c,EscPressed6   ; Skip if <ESC> pressed
        ld      (save_sp),hl    ; Otherwise save value
        jr      EnterPC
EscPressed6:
        ld      hl,(save_sp)
        call    PrintAddress
EnterPC:
        call    PrintSpace
        ld      a,'P'
        call    PrintChar
        ld      a,'C'
        call    PrintChar
        call    PrintEquals
        call    GetAddress
        jr      c,EscPressed7   ; Skip if <ESC> pressed
        ld      (save_pc),hl    ; Otherwise save value
        jr      EnterIX
EscPressed7:
        ld      hl,(save_pc)
        call    PrintAddress
EnterIX:
        call    PrintCR
        ld      a,'I'
        call    PrintChar
        ld      a,'X'
        call    PrintChar
        call    PrintEquals
        call    GetAddress
        jr      c,EscPressed8   ; Skip if <ESC> pressed
        ld      (save_ix),hl    ; Otherwise save value
        jr      EnterIY
EscPressed8:
        ld      hl,(save_ix)
        call    PrintAddress
EnterIY:
        call    PrintSpace
        ld      a,'I'
        call    PrintChar
        ld      a,'Y'
        call    PrintChar
        call    PrintEquals
        call    GetAddress
        jr      c,EscPressed9   ; Skip if <ESC> pressed
        ld      (save_iy),hl    ; Otherwise save value
        jr      EnterI
EscPressed9:
        ld      hl,(save_iy)
        call    PrintAddress
EnterI:
        call    PrintSpace
        ld      a,'I'
        call    PrintChar
        call    PrintEquals
        call    GetByte
        jr      c,EscPressed10  ; Skip if <ESC> pressed
        ld      (save_i),a      ; Otherwise save value
        jr      EnterR
EscPressed10:
        ld      a,(save_i)
        call    PrintByte
EnterR:
        call    PrintSpace
        ld      a,'R'
        call    PrintChar
        call    PrintEquals
        call    GetByte
        jr      c,EscPressed11  ; Skip if <ESC> pressed
        ld      (save_r),a      ; Otherwise save value
        jr      Eol
EscPressed11:
        ld      a,(save_r)
        call    PrintByte
Eol:
        call    PrintCR
        ret

; HELP command.
; Displays list of valid commands.

HelpCommand:
        call    PrintChar       ; Echo command back
        ld      hl,strHelp
        call    PrintString
        ret


; Fill command.
; Fill memory with bytes over a range of addresses.
; TODO: Check that start < end.
FillCommand:
        call    PrintChar       ; Echo command back
        call    PrintSpace
        call    GetAddress      ; Prompt for start address
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ex      de,hl           ; Put HL (start address) in DE
        call    PrintSpace
        call    GetAddress      ; Prompt for end address
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        call    PrintSpace
        ex      de,hl           ; Put HL (end address) in DE, start address goes back in HL
        call    GetByte         ; Prompt for fill byte
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      b,a             ; Store fill byte in B
        call    PrintCR
fill:
        ld      (hl),b          ; Fill address with byte
        inc     hl              ; Increment current address in HL
        ld      a,h             ; Get H
        cp      d               ; Compare to D
        jr      nz,fill         ; If no match, continue filling
        ld      a,l             ; Get L
        cp      e               ; Compare to E
        jr      nz,fill         ; If no match, continue filling
        ld      (hl),b          ; We are at last address, write byte to it
        ret

; Copy Command
; Copy a block of memory from one location to another.
; TODO: Change command options to C <start> <end> <dest>
; TODO: Try to minimize copying between registers and memory
; TODO: Check that start < end and handle overlap.

CopyCommand:
        call    PrintChar       ; Echo command back
        call    PrintSpace
        call    GetAddress      ; Prompt for start address
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (src),hl        ; Save source address in src
        call    PrintSpace
        call    GetAddress      ; Prompt for end address
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (dst),hl        ; Save destination address in dst
        call    PrintSpace
        call    GetAddress      ; Prompt for number of bytes
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (size),hl       ; Save length in size
        ld      bc,(size)       ; Put size in BC
        ld      hl,(src)        ; Put source in HL
        ld      de,(dst)        ; Put dest in DE
        ldir                    ; Do the copy
        call    PrintCR
        ret

; Checksum Command
; Calculate 16-bit checksum of a block of memory.

ChecksumCommand:
        call    PrintChar       ; Echo command back
        call    PrintSpace
        call    GetAddress      ; Prompt for start address
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ex      de,hl           ; Swap HL and DE (put start in DE)
        call    PrintSpace
        call    GetAddress      ; Prompt for end address
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ex      de,hl           ; Swap HL and DE
                                ; HL holds start/current address
                                ; DE holds end address
                                ; BC will hold checksum
        ld      bc,0000h        ; Clear checksum total
checkloop:
        scf                     ; Clear carry
        ccf
        ld      a,c             ; Get LSB of checksum
        adc     a,(hl)          ; Add next date byte of memory
        ld      c,a             ; Store LSB of checksum
        ld      a,b             ; Get MSB of checksum
        adc     a,0             ; Add possible carry from LSB
        ld      b,a             ; Store MSB of checksum
        ld      a,h             ; See if MSB of pointer has reached end address yet
        cp      d               ; e.g. H = D
        jr      nz,inc
        ld      a,l             ; See if MSB of pointer has reached end address yet
        cp      e               ; e.g. L = E
        jr      nz,inc
        call    PrintSpace      ; Done, print checksum value
        ld      h,b             ; Put value in HL
        ld      l,c
        call    PrintAddress
        call    PrintCR
        ret
inc:    inc     hl              ; Increment address pointer
        jr      checkloop


MemoryCommand:
; Format:
; : <addr> <bb> <bb> ... <Esc>
; eg:
; : A000 12 34 56 78
Memory:
        call    PrintChar       ; Echo command
        call    PrintSpace
        call    GetAddress      ; Get start address
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
writeLoop:
        call    PrintSpace      ; Echo space
        call    GetByte         ; Get data byte (ESC will exit)
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (hl),a          ; Write data to address
        inc     hl              ; Increment address
        jr      writeLoop       ; Input more data

; Scope loop command. For hardware debugging, loops on reading or
; writing a memory or i/o address. When third parameter is "Y",
; continuously loops until reset. When parameter is "N" , does one
; read or write (data is displayed on a read) and returns.

; FORMAT:
; P A R <Y/N> <address>
; P A W <Y/N> <address> <data>
; P I R <Y/N> <io port>
; P I W <Y/N> <io port> <data>

LoopCommand:
        call    PrintChar       ; Echo command
        call    PrintSpace
AorI:
        call    GetChar
        cp      ESC             ; Is it <ESC>
        jp      z,CancelCmd     ; Cancel if <ESC> pressed
        call    ToUpper
        cp      'A'             ; Is it 'A'?
        jr      z,Okay1
        cp      'I'             ; Is it 'I'?
        jr      z,Okay1
        jr      AorI            ; If not, try again
Okay1:
        ld      (size),a        ; Save operation (A or I)
        call    PrintChar       ; Echo operation
        call    PrintSpace
EorW:   call    GetChar         ; Get R or W
        cp      ESC             ; Is it <ESC>
        jp      z,CancelCmd     ; Cancel if <ESC> pressed
        call    ToUpper
        cp      'R'             ; Is it 'R'?
        jr      z,Okay2
        cp      'W'             ; Is it 'W'?
        jr      z,Okay2
        jr      EorW            ; If not, try again
Okay2:
        ld      (size+1),a      ; Save operation (R or W)
        call    PrintChar       ; Echo operation
        call    PrintSpace

YorN:   call    GetChar         ; Get Y or N
        cp      ESC             ; Is it <ESC>
        jp      z,CancelCmd     ; Cancel if <ESC> pressed
        call    ToUpper
        cp      'Y'             ; Is it 'Y'?
        jr      z,Okay3
        cp      'N'             ; Is it 'N'?
        jr      z,Okay3
        jr      YorN            ; If not, try again
Okay3:
        ld      (address),a     ; Save loop (Y or N)
        call    PrintChar       ; Echo operation
        call    PrintSpace
        ld      a,(size)        ; Get operation (A or I)
        cp      'A'             ; Address?
        jr      z,loopA         ; if so, branch
        ld      a,(size+1)      ; Get operation (R or W)
        cp      'R'
        jr      z,ioRead        ; Operation is I/O read
        jr      ioWrite         ; Operation is I/O write
loopA:
        ld      a,(size+1)      ; Get operation (R or W)
        cp      'R'
        jr      z,addrRead      ; Operation is address read
        jr      addrWrite       ; Operation is address write

addrRead:
        call    GetAddress      ; Get 16-bit address
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (src),hl        ; Save it
        call    PrintCR
        ld      hl,(src)        ; Get address
        ld      a,(address)     ; Get loop option
        cp      'N'             ; Was it 'N'?
        jr      z,addrRead1     ; Branch if yes
ReadLoop:
        ld      a,(hl)          ; Read from address
        jr      ReadLoop        ; Repeat forever
addrRead1:
        ld      a,(hl)          ; Read from address
        call    PrintByte       ; Display value read
        call    PrintCR         ; Newline
        ret                     ; Return
addrWrite:
        call    GetAddress      ; Get 16-bit address
        jr      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (src),hl        ; Save it
        call    PrintSpace
        call    GetByte         ; Get 8-bit data
        jr      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (dst),a         ; Save it
        call    PrintCR
        ld      hl,(src)        ; Get address
        ld      a,(address)     ; Get loop option
        cp      'N'             ; Was it 'N'?
        jr      z,addrWrite1    ; Branch if yes
        ld      a,(dst)         ; Get data
WriteLoop2:
        ld      (hl),a          ; Write to address
        jr      WriteLoop2      ; Repeat forever
addrWrite1:
        ld      a,(dst)         ; Get data
        ld      (hl),a          ; Write to address
        ret                     ; Return
ioRead:
        call    GetByte         ; Get 8-bit I/O address
        jr      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (src),a         ; Save it
        call    PrintCR
        ld      a,(src)         ; Get  address
        ld      c,a
        ld      a,(address)     ; Get loop option
        cp      'N'             ; Was it 'N'?
        jr      z,ioRead1       ; Branch if yes
ReadLoop1:
        in      a,(c)           ; Read from I/O port
        jr      ReadLoop1       ; Repeat forever
ioRead1:
        in      a,(c)           ; Read from I/O port
        call    PrintByte       ; Display value read
        call    PrintCR         ; Newline
        ret                     ; Return
ioWrite:
        call    GetByte         ; Get 8-bit I/O address
        jr      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (src),a         ; Save it
        call    PrintSpace
        call    GetByte         ; Get 8-bit data
        jr      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (dst),a         ; Save it
        call    PrintCR
        ld      a,(src)         ; Get address
        ld      c,a
        ld      a,(address)     ; Get loop option
        cp      'N'             ; Was it 'N'?
        jr      z,ioWrite1      ; Branch if yes
        ld      a,(dst)         ; Get data
WriteLoop1:
        out     (c),a           ; Write to I/O port
        jr      WriteLoop1      ; Repeat forever
ioWrite1:
        ld      a,(dst)         ; Get data
        out     (c),a           ; Write to I/O port
        ret                     ; Return

CancelCmd:
        call    PrintCR
        ret

; Math command. Add or subtract two 16-bit hex numbers.
; Format: = <ADDRESS> +/- <ADDRESS>
; e.g.
; = 1234 + 0077 = 12AB
; = FF00 - 0002 = FEFE
MathCommand:
        call    PrintChar       ; Echo command
        call    PrintSpace
        call    GetAddress      ; Get first number
        jr      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (src),hl
        call    PrintSpace
PlusOrMinus:
        call    GetChar
        cp      ESC             ; Is it <ESC>
        jr      z,CancelCmd     ; Cancel if <ESC> pressed
        cp      '+'             ; Is it plus?
        jr      z,Okay
        cp      '-'             ; Is it minus?
        jr      z,Okay
        jr      PlusOrMinus     ; If not, try again
Okay:
        ld      (size),a        ; Save operator
        call    PrintChar       ; Echo operator
        call    PrintSpace
        call    GetAddress      ; Get second number
        jr      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (dst),hl

        call    PrintSpace
        ld      a,'='
        call    PrintChar
        call    PrintSpace
        ld      a,(size)         ; Get operator
        cp      '-'
        jr      z,Sub           ; Branch if operation is subtract

        ld      hl,(src)        ; Perform 16-bit add
        ld      de,(dst)
        add     hl,de
        jr      PrintResult

Sub:
        ld      hl,(src)        ; Perform 16-bit add
        ld      de,(dst)
        scf                     ; Clear carry
        ccf
        sbc     hl,de

PrintResult:
        call    PrintAddress
        jp      PrintCR


; Search Memory
; Format: S <START> <END> <DATA>
; TODO: Make sure start <= end
SearchCommand:
        call    PrintChar       ; Echo command
        call    PrintSpace
        call    GetAddress      ; Get start address in HL
        jr      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (src),hl        ; Save start address
        call    PrintSpace
        call    GetAddress      ; Get end address in HL
        jr      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (size),hl       ; Save end address
        call    PrintSpace
        call    GetByte         ; Get search pattern in A
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        call    PrintCR
        ld      hl,(size)       ; Get end address
        ld      de,(src)        ; Get start address
        scf                     ; Clear carry
        ccf
        sbc     hl,de           ; Calculate byte count (HL) = end (HL) - start (DE)
        inc     hl              ; Need to add one for actual byte count
        ld      b,h             ; Put byte count HL in BC
        ld      c,l
        ld      hl,(src)        ; Put start address in HL

; Byte to search for is in A
; Start address is in HL
; Byte count is in BC

        cpir                    ; Do search
        jr      z,match         ; Branch if match found
        ld      hl,strNotFound  ; Not found message
        call    PrintString
        call    PrintCR
        ret
match:
        ld      d,h             ; Save HL in DE
        ld      e,l
        ld      hl,strFound     ; Found at message
        call    PrintString
        ld      h,d             ; Restore HL from DE
        ld      l,e
        dec     hl              ; Points one past the match address, so subtract one
        call    PrintAddress
        call    PrintCR
        ret


; Verify range of memory matches another
; Format: S <START> <END> <DEST>
; TODO: Make sure start <= end
VerifyCommand:
        call    PrintChar       ; Echo command
        call    PrintSpace
        call    GetAddress      ; Get start address in HL
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (src),hl        ; Save start address
        call    PrintSpace
        call    GetAddress      ; Get end address in HL
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (size),hl       ; Save end address
        call    PrintSpace
        call    GetAddress      ; Get dest address in HL
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (dst),hl        ; Save dest address
        call    PrintCR
        ld      hl,(size)       ; Get end address
        ld      de,(src)        ; Get start address
        scf                     ; Clear carry
        ccf
        sbc     hl,de           ; Calculate byte count (HL) = end (HL) - start (DE)
        inc     hl              ; Need to add one for actual byte count
        ld      b,h             ; Put byte count HL in BC
        ld      c,l
        ld      hl,(src)        ; Put start address in HL
        ld      de,(dst)        ; Put destination address in DE

; Start address is in HL
; Destination address is in DE
; Byte count is in BC

verify:
        ld      a,(de)          ; Get byte from destination
        cpi                     ; Do compare, increment HL, decrement BC
        jr      nz,mismatch     ; Branch if mismatch
        jp      po,passed       ; Done and passed if end of range (BC=0)
        inc     de              ; Increment destination pointer
        jr      verify          ; Otherwise go back

mismatch:
        ld      d,h             ; Save HL in DE
        ld      e,l
        ld      hl,strMismatch  ; Mismatch message
        call    PrintString
        ld      h,d             ; Restore HL from DE
        ld      l,e
        dec     hl              ; Points to one past the mismatch address, so subtract one
        call    PrintAddress
        call    PrintCR
        ret

; Memory test command
; Format: T <START> <END>
; TODO: Make sure start <= end

TestCommand:
        call    PrintChar       ; Echo the command back
        call    PrintSpace
        call    GetAddress      ; Get start address in HL
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (src),hl        ; Save start address
        call    PrintSpace
        call    GetAddress      ; Get end address in HL
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (size),hl       ; Save end address

        call    PrintCR
        ld      hl,(size)       ; Get end address
        ld      de,(src)        ; Get start address
        scf                     ; Clear carry
        ccf
        sbc     hl,de           ; Calculate byte count (HL) = end (HL) - start (DE)
        inc     hl              ; Need to add one for actual byte count
        ld      (size),hl       ; Store size


        ld      hl,(src)        ; Parameters to RAM test
        ld      de,(size)
        call    RAMTST          ; Call RAM test
        jr      c,fail          ; Error occurred
passed: ld      hl,strPassed    ; Passed message
        call    PrintString
        call    PrintCR
        ret

; Test failed. Display error, e.g.
; Error at: 1234 Exp: 55 Read: AA
fail:
        dec     hl              ; Need to decrement HL to point to error address
        ld      (src),hl        ; Save error address
        ld      (size),a        ; Save expected data

        ld      hl,strError     ; Error message
        call    PrintString

        ld      hl,(src)        ; Display error address
        call    PrintAddress

        ld      hl,strExp       ; Expected message
        call    PrintString

        ld      a,(size)        ; Get expected data
        call    PrintByte       ; Print expected data

        ld      hl,strRead      ; Read message
        call    PrintString

        ld      hl,(src)
        ld      a,(hl)          ; Get actual data
        call    PrintByte       ; Print actual data
        call    PrintCR
        ret


; Unassemble command
; Format: U <ADDRESS>

UnassembleCommand:
        call    PrintChar       ; Echo the command back
        call    PrintSpace
        call    GetAddress      ; Get start address in HL
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (address),hl    ; Save address
startScreen1:
        call    PrintCR
        ld      b,LINES         ; Number of instructions to disassemble per page
dloop:  push    bc              ; Save BC
        call    disass          ; Disassemble one instruction
        pop     bc              ; Restore BC
        dec     b               ; Page done?
        jp      nz,dloop        ; Branch if not

        ld      hl,strContinue  ; Prompt whether to continue
        call    PrintString
cont1:  call    GetChar         ; Get key
        cp      ESC             ; Escape?
        jr      nz,trySpace1
        call    PrintCR         ; If so, return
        ret
trySpace1:
        cp      ' '             ; Space?
        jr      z,startScreen1  ; If so, do next screen
        jr      cont1           ; Invalid key, try again

        ret

; Quit command
; Format: Q

QuitCommand:
        call    PrintChar       ; Echo the command back
        call    PrintCR         ; Print newline
if      CPM                     ; If running under CP/M
        jp      BOOT            ;   jump to CP/M
endif
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Utility Routines

; PrintChar
; Output character in A register to console.
; Registers affected: none.

if      CPUVILLE
PrintChar:
        push    af              ; Save A register
loop1:  in      a,(SREG)        ; Read status register
        bit     0,A             ; Test TDRE bit
        jr      z,loop1         ; Repeat until TxRDY is set
        pop     af              ; Restore A
        out     (DREG),a        ; Output it to data register
        ret                     ; And return
endif

if      CPM
PrintChar:
        push    af              ; Save registers
        push    bc
        push    de
        push    hl
        ld      c,DIO           ; BDOS call
        ld      e,a             ; Char is passed in e
        call    BDOS            ; Call BDOS to output char
        pop     hl              ; Restore registers
        pop     de
        pop     bc
        pop     af
        ret                     ; And return
endif

; GetChar
; Read character from console and return in A register. The character
; is not echoed. Waits for character to be entered.
; Registers affected: A.

if      CPUVILLE
GetChar:
        in      a,(SREG)        ; Read status register
        bit     1,A             ; Test RxRDY bit
        jr      z,GetChar       ; Repeat until RDRF is set
        in      a,(DREG)        ; Read character from data register
        ret                     ; And return
endif

if      CPM
GetChar:
        push    bc              ; Save registers
        push    de
        push    hl
l1:     ld      c,DIO           ; BDOS call
        ld      e,0FFH          ; Indicates to read
        call    BDOS            ; Call BDOS to input char
        cp      0               ; Char returned?
        jr      z,l1            ; If not, try again
        pop     hl              ; Restore registers
        pop     de
        pop     bc
        ret                     ; And return
endif

; PrintCR
; Print carriage return/newline.
; Registers affected: none

PrintCR:
        push    af              ; Save A reg
        ld      a,CR            ; Carriage Return character
        call    PrintChar
        ld      a,NL            ; Newline character
        call    PrintChar
        pop     af              ; Restore A reg
        ret

; PrintEquals
; Print equals sign.
; Registers affected: none

PrintEquals:
        push    af              ; Save A reg
        ld      a,'='           ; Equals character
        call    PrintChar
        pop     af              ; Restore A reg
        ret

; Print "0"
; Registers affected: none
PrintZero:
        push    af
        ld      a,'0'
        call    PrintChar
        pop     af
        ret


; Print "1"
; Registers affected: none
PrintOne:
        push    af
        ld      a,'1'
        call    PrintChar
        pop     af
        ret


; PrintAscii
; If character in A is printable ASCII, print it, otherwise print "."
; Registers affected: none.

PrintAscii:
        push    af              ; Save A
        cp      ' '             ; Less than <Space> ?
        jr      c,notPrintable  ; If so, not printable
        cp      '~'+1           ; Greater than tilde?
        jr      nc,notPrintable ; If so, not printable
ppr:    call    PrintChar       ; Print character
        pop     af              ; Restore A
        ret
notPrintable:
        ld      a,'.'
        jr      ppr


; ClearScreen
; Clear screen. Assumes an VT100/ANSI terminal.
; Registers affected: HL, A.

ClearScreen:
        ld      hl,strClearScreen
        call    PrintString
        ret

; PrintString
; Print string pointed to by HL until null found.
; Registers affected: HL

PrintString:
        push    af              ; Save A register
nextch: ld      a,(hl)          ; Get a character
        cp      0               ; Is it a null?
        jr      z,eos           ; If so, exit
        call    PrintChar       ; Print the character
        inc     hl              ; Advance pointer to next character
        jr      nextch          ; And repeat
eos:    pop     af              ; Restore A register
        ret                     ; Return

; PrintSpaces
; Print number of space characters specified in A.
; Registers affected: none
PrintSpaces:
        push    af              ; Save A reg
        push    bc              ; Save BC reg
        ld      b,a             ; Put count in B
sp1:    call    PrintSpace      ; Print a space
        dec     b               ; Decrement count
        jr      nz,sp1
        pop     bc              ; Restore BC reg
        pop     af              ; Restore A reg
        ret                     ; Return

; PrintSpace
; Print space character.
; Registers affected: none
PrintSpace:
        push    af              ; Save A reg
        ld      a,' '           ; Space character
        call    PrintChar
        pop     af              ; Restore A reg
        ret

; ToUpper
; Convert character in A to uppercase if it is a letter.
; Registers affected: A
ToUpper:
        cp      'a'             ; Less than 'a' ?
        jr      c,notUpper      ; If so, branch
        cp      'z'+1           ; Greater than 'z'?
        jr      nc,notUpper
        and     11011111b       ; Convert to upper case
notUpper:
        ret

; PrintByte
; Print 8-bit value in A as two ASCII hex characters
; Registers affected: A
PrintByte:
        push    bc              ; Save BC reg
        call    bhconv          ; Convert to two hex digits
        ld      a,b             ; Get first digit
        call    PrintChar       ; Print it
        ld      a,c             ; Get second digit
        call    PrintChar       ; Print it
        pop     bc              ; Restore BC reg
        ret                     ; Return

; Convert byte in A to two hex ASCII digits and return in B,C.
bhconv:
        push    hl              ; Save HL
        ld      l,a             ; Save original byte
        rra                     ; Shift upper nybble into lower nybble
        rra
        rra
        rra
        call    bin1            ; Convert digit to ASCII
        ld      b,a             ; Put it in B
        ld      a,l             ; Get original byte
        call    bin1            ; Convert digit to ASCII
        ld      c,a             ; Put it in C
        pop     hl              ; Restore HL
        ret                     ; Return

; Convert bottom nybble of byte on A to ASCII
bin1:
        and     0FH             ; Mask out lower nybble
        add     a,'0'           ; Convert to ASCII digit, e.g. 0->'0'
        cp      '9'+1           ; Is it greater than '9'?
        ret     c               ; If not, we are done
        add     a,'A'-'9'-1     ; Add offset to convert to hex letter A-F
        ret                     ; Return


; PrintAddress
; Print a two byte address passed in H,L.
; Registers affected: A.
PrintAddress:
        ld      a,h             ; Get MSB
        call    PrintByte       ; Print it
        ld      a,l             ; Get LSB
        call    PrintByte       ; Print it
        ret                     ; Return


; GetHex
; Gets a single hex digit from the keyboard, 0-9 or A-F or a-f.
; Ignores invalid characters. <Esc> cancels and sets carry bit.
; Returns binary nybble in A.
; Registers affected: A

GetHex:
        call    GetChar         ; Get a character
        cp      ESC             ; Is it <Escape> ?
        jr      nz,next         ; Branch if not
        sub     a               ; Set A to zero
        scf                     ; Otherwise set carry and return.
        ret
next:   cp      '0'             ; Less than '0'?
        jr       c,GetHex       ; Yes, ignore and try again
        cp      '9'+1           ; Greater than 9?
        jr       c,validDigit   ; Branch if not (is 0-9)
        cp      'A'             ; Less than 'A'?
        jr       c,GetHex       ; Yes, ignore and try again
        cp      'F'+1           ; Greater than 'F'?
        jr       c,validULetter ; Branch if not (is A-F)
        cp      'a'             ; less that 'a'?
        jr       c,GetHex       ; Yes, ignore and try again
        cp      'f'+1           ; Greater than 'f'?
        jr       c,validLLetter ; Branch if not (is a-f)
        jr      GetHex          ; Invalid, try again
validDigit:
        call    PrintChar       ; Echo the character
        sub     '0'             ; Convert digit to binary
        jr      done
validLLetter:
        and     11011111b       ; Convert to lowercase letter to upper case
validULetter:
        call    PrintChar       ; Echo the character
        sub     'A'-10          ; Convert uppercase letter to binary
        jr      done
done:   scf                     ; Weird 8080 way to clear carry
        ccf                     ; Set it and then complement it
        ret


; GetByte
; Gets a two character hex number from the keyboard.
; Ignores invalid characters. <Esc> cancels and sets carry bit.
; Returns binary byte in A.
; Registers affected: A,B

GetByte:
        call    GetHex          ; Get most significant nybble
        ret     c               ; Exit if <ESC> pressed
        rlca                    ; Shift to upper nybble
        rlca
        rlca
        rlca
        ld      b,a             ; Save result in B register
        call    GetHex          ; Get least significant nybble
        ret     c               ; Exit if <ESC> pressed
        add     a,b             ; Add upper nybble to lower
        ret                     ; Return


; GetAddress
; Gets a four character hex number from the keyboard.
; Ignores invalid characters. <Esc> cancels and sets carry bit.
; Returns binary word in HL.
; Registers affected: A,B,H,L

GetAddress:
        call    GetByte         ; Get MSB
        ret     c               ; Exit if <ESC> pressed
        ld      h,a             ; Save MSB in H
        call    GetByte         ; Get LSB
        ret     c               ; Exit if <ESC> pressed
        ld      l,a             ; Save LSB in L
        ret                     ; Return


; The code below was taken from the book "Z80 Assembly Language
; Subroutines" by Lance Leventhal and Winthrop Saville.
;
; Title RAM        test
; Name:            RAMTST
;
; Purpose:         Test a RAM (read/write memory) area.
;                    1) Write all 0 and test
;                    2) Write all FF hex and test
;                    3) Write all AA hex and test
;                    4) Write all 55 hex and test
;                    5) Shift a single 1 through each bit,
;                       while clearing all other bits
;
;                    If the program finds an error, it exits
;                    immediately with the Carry flag set and
;                    indicates where the error occurred and
;                    what value it used in the test.
;
; Entry:           Register pair HL = Base address of test area
;                  Register pair DE = Size of area in bytes
;
; Exit:            If there are no errors then
;                    Carry flag = 0
;                    test area contains 0 in all bytes
;                  else
;                    Carry flag = 1
;                    Register pair HL = Address of error
;                    Register A = Expected value
;
; Registers used:  AF,BC,DE,HL
;
; Time:            Approximately 633 cycles per byte plus
;                  663 cycles overhead
;
; Size:            Program 82 bytes

RAMTST:

; Exit with no errors if area size is 0

        ld      a,d             ; Test area size
        or      e
        ret     z               ; Exit with no errors if size is zero
        ld      b,d             ; BC = area size
        ld      c,e

; Fill memory with a 0 and test

       sub      a
       call     FILCMP
       ret      c               ; Exit if error found

; Fill memory with FF hex (all 1's) and test

       ld       a,0FFH
       call     FILCMP
       ret      c               ; Exit if error found

; Fill memory with AA hex (alternating 1's and 0's) and test

       ld       a,0AAH
       call     FILCMP
       ret      c               ; Exit if error found

; Fill memory with 55 hex (alternating 0's and 1's) and test

       ld       a,55H
       call     FILCMP
       ret      c               ; Exit if error found

; Perform walking bit test. Place a 1 in bit 7 and
; see if it can be read back. Then move the 1 to
; bits 6, 5, 4, 3, 2, 1, and 0 and see if it can
; be read back.
WLKLP:
        ld      a,10000000B     ; Make bit 7 1, all other bits 0
WLKLP1:
        ld      (hl),a          ; Store test pattern in memory
        cp      (hl)            ; Try to read it back
        scf                     ; Set carry in case of error
        ret     nz              ; Return if error
        rrca                    ; Rotate pattern to move 1 right
        cp      10000000B
        jr      nz,WLKLP1       ; Continue until 1 is back in bit 7
        ld      (hl),0          ; Clear byte just checked
        inc     hl
        dec     bc              ; Decrement and test 16-bit counter
        ld      a,b
        or      c
        jr      nz,WLKLP        ; Continue until memory tested
        ret                     ; No errors (note OR C clears carry)

; ***********************************
; Routine: FILCMP
; Purpose: Fill memory with a value and test
;          that it can be read back
; Entry:   A = test value
;          HL = base address
;          BC = size of area in bytes
; Exit:    If no errors then
;            carry flag is 0
;          else
;            carry flag is 1
;          HL = address of error
;          DE = base address
;          BC = size if area in bytes
;          A = test value
; Registers used: AF,BC,DE,HL
; ***********************************
FILCMP:
        push    hl              ; Save base address
        push    bc              ; Save size of area
        ld      e,a             ; Save test value
        ld      (hl),a          ; Store test value in first byte
        dec     bc              ; Remaining area = size - 1
        ld      a,b             ; Check if anything in remaining area
        or      c
        ld      a,e             ; Restore test value
        jr      z,COMPARE       ; Branch if area has only 1 byte

; Fill rest of area using block move
; Each iteration moves test value to next higher address

        ld      d,h             ; Destination is always source + 1
        ld      e,l
        inc     de
        ldir                    ; Fill memory

; Now that memory has been filled, test to see it
; each byte can be read back correctly.

COMPARE:
        pop     bc              ; Restore size of area
        pop     hl              ; Restore base address
        push    hl              ; Save base address
        push    bc              ; Save size of value

; Compare memory and test value

CMPLP:
        cpi
        jr      nz,CMPER        ; Jump if not equal
        jp      pe,CMPLP        ; Continue through entire area
                                ; Note CPI clears P/V flag if it
                                ; decrements BC to 0

; No errors found, so clear carry

        pop     bc              ; BC = size of area
        pop     hl              ; HL = base address
        or      a               ; Clear carry, indicating no errors
        ret

; Error exit, set carry
; HL = address of error
; A = test value

CMPER:
        pop     bc              ; BC = size of area
        pop     de              ; DE = base address
        scf                     ; Set carry, indicating an error
        ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Strings

strStartup:
        db      "JMON Monitor 0.6 by Jeff Tranter\r\n",0

strInvalid:
        db      "Invalid command. Type ? for help.\r\n",0

strHelp:
        db      "\r\n"
        db      "Valid commands:\r\n"
        db      "C <src> <dest> <size>               Copy memory\r\n"
        db      "D <address>                         Dump memory\r\n"
        db      "F <start> <end> <data>              Fill memory\r\n"
        db      "G <address>                         Go\r\n"
        db      "I                                   Show info\r\n"
        db      "K <start> <end>                     Checksum\r\n"
        db      "L                                   Clear screen\r\n"
        db      "P <A/I> <R/W> <Y/N> <addr> [<data>] Scope loop\r\n"
        db      "Q                                   Quit\r\n"
        db      "R                                   Examine registers\r\n"
        db      "S <start> <end> <data>              Search memory\r\n"
        db      "T <start> <end>                     Test memory\r\n"
        db      "U <address>                         Unassemble memory\r\n"
        db      "V <start> <end> <dest>              Verify memory\r\n"
        db      ": <address> <data>...               Write to memory\r\n"
        db      "= <address> +/- <address>           Hex math calculation\r\n"
        db      "?                                   Help\r\n",0

strClearScreen:
        db      ESC,"[2J",ESC,"[H",0       ; VT100/ANSI clear screen, cursor home
;       db      ESC,"H",ESC,"J",0          ; DEC VT52 clear screen, cursor home
;       db      ESC,"E",0                  ; Heathkit H19/H88/H89 clear display

strCpuType:
        db      "CPU type: ",0
str8080:
        db      "8080",0
strZ80:
        db      "Z80",0
strContinue:
        db      "Press <Space> to continue, <ESC> to stop ",0
strNotFound:
        db      "Not found",0
strFound:
        db      "Found at: ",0
strMismatch:
        db      "Mismatch at: ",0
strError:
        db      "Error at: ",0
strExp:
        db      " Exp: ",0
strRead:
        db      " Read: ",0
strPassed:
        db      "Passed",0

        include "disasm.asm"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Variables (size in bytes)

vars:
save_a: equ     vars           ; Save A reg (1)
save_f: equ     vars+1         ; Saved flags (1)
save_b: equ     vars+2         ; Saved B reg (1)
save_c: equ     vars+3         ; Saved C reg (1)
save_d: equ     vars+4         ; Saved D reg(1)
save_e: equ     vars+5         ; Saved E reg (1)
save_h: equ     vars+6         ; Saved H reg (1)
save_l: equ     vars+7         ; Saved L ref (1)
save_i: equ     vars+8         ; Saved I reg (1)
save_r: equ     vars+9         ; Saved R reg (1)
save_sp: equ    vars+10        ; Saved SP (2)
save_pc: equ    vars+12        ; Saved PC (2)
save_ix: equ    vars+14        ; Saved IX reg (2)
save_iy: equ    vars+16        ; Saved IY (2)
src:    equ     vars+18        ; Source address, used for commands like Copy (2)
dst:    equ     vars+20        ; Destination address (2)
size:   equ     vars+22        ; Size (2)
address: equ    vars+24        ; Next address to disassemble (2)
opcode:  equ    vars+26        ; Opcode e.g. OP_ADD (1)
mnemonic: equ   vars+27        ; Pointer to mnemonic string, e.g. "ADD " (2)
len:    equ     vars+29        ; Length of instruction (1)

        end
