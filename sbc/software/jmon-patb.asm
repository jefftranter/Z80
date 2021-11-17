;
; JMON - Jeff's Monitor Program
; ------------------------------
; 
; A machine language monitor program for my Z80 Single Board Computer.
; Inspired by JMON for the Apple Replica 1 and 6502 processor.
; I wrote this mostly as an exercise to learn Z80 assembly language.
; 
; Copyright (C) 2014-2021 by Jeff Tranter <tranter@pobox.com>
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
;   BASIC: B
;   COPY: C <START> <END> <DEST>
;   DUMP: D <START>
;   FILL: F <START> <END> <DATA>
;   GO: G <ADDRESS>
;   INFO: I
;   CHECKSUM: K <START> <END>
;   CLR SCREEN: L
;   SCOPE LOOP: P <A/I> <R/W> <addr> [<data>]
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
; 0.5     16-Nov-2021  Implemented BASIC ("B") command.
;                      Integrated with Palo Alto Tiny Basic.
;
; TODO:
; Intel or Motorola hex file loader?

        CPU     Z80

        org     0000H           ; Start at address 0 if running from ROM

; Constants

prompt: equ     '?'             ; Prompt character
CR:             equ '\r'        ; Carriage Return
NL:             equ '\n'        ; Newline
ESC:            equ 01BH        ; Escape
stack:          equ 0F000H      ; Starting address for stack

; Reset/RST 00 vector: jump to start entry point
RESET:  JP      Start

; RST 08 vector
        DB      (0008H-$) DUP (0FFH)
        RET                     ; Simply return

; RST 10 vector
        DB      (0010H-$) DUP (0FFH)
        RET                     ; Simply return

; RST 18 vector
        DB      (0018H-$) DUP 0FFH
        RET                     ; Simply return

; RST 20 vector
        DB      (0020H-$) DUP 0FFH
        RET                     ; Simply return

; RST 28 vector
        DB      (0028H-$) DUP 0FFH
        RET                     ; Simply return

; RST 30 vector
        DB      (0030H-$) DUP 0FFH
        RET                     ; Simply return

; Mode 1 IRQ/RST 38 vector
        DB      (0038H-$) DUP 0FFH
IRQ:    RETI                    ; Return from IRQ

; NMI vector
;        DB      (0066H-$) DUP 0FFH
;NMI:    RETN                    ; Return from NMI

; Start actual code at $0070

;        DB      (0070H-$) DUP 0FFH

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Main program entry point

Start:
        DI                      ; Disable interrupts
        IM        1             ; Use interrupt mode 1

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

        ld      sp,stack        ; Set up stack pointer

        ld      a,016H          ; Initialize 6850 ACIA
        out     (CREG),a

        call    ClearScreen     ; Clear screen
        ld      hl,strStartup   ; Print startup message
        call    PrintString

mainloop:
        ld      a,prompt        ; Display command prompt
        call    PrintChar
        call    PrintSpace

        call    GetChar         ; Get a command (letter)
        call    ToUpper         ; Convert to upper case

        cp      'B'
        jr      nz,tryC
        call    BasicCommand
        jr      mainloop

tryC:   cp      'C'
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
        jr      nz,tryR
        call    LoopCommand
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

; BASIC. Jumps into Palo Alto Tiny Basic. Does not return.

BasicCommand:
        jp      BASIC

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
        jr      mem
z80:    
        ld      hl,strZ80       ; It is a Z80
        jr      prnt

; Detect memory size. Assume RAM at FFFF and work backward until we
; find non-writable memory. Make sure we restore any memory we change.

mem:    ld      hl,0FFFFH       ; Start at FFFF
tst:    ld      a,(hl)          ; Read memory location
        cpl                     ; Complement it
        ld      (hl),a          ; Write it back
        cp      (hl)            ; Did it change?
        jr      nz,startRam     ; No, found start of RAM
        cpl                     ; Get back original data
        ld      (hl),a          ; Put it back
        dec     hl              ; Point to next location to test
        jr      tst
startRam:
        inc     hl              ; Actual start is previous location tested
        push    hl              ; Save RAM start
        ld      hl,strRamFound1 ; String to print
        call    PrintString     ; Print it
        pop     hl              ; Restore RAM start
        call    PrintAddress    ; Display it
        ld      hl,strRamFound2 ; String to print
        call    PrintString     ; Print it
        call    PrintCR
        ret

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
        ld      a,l             ; Save source address in src (low,high)
        ld      (src),a
        ld      a,h
        ld      (src+1),a
        call    PrintSpace
        call    GetAddress      ; Prompt for end address
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      a,l             ; Save destination address in dst (low,high)
        ld      (dst),a
        ld      a,h
        ld      (dst+1),a
        call    PrintSpace
        call    GetAddress      ; Prompt for number of bytes
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      a,l             ; Save length in size (low,high)
        ld      (size),a
        ld      a,h
        ld      (size+1),a
        ld      a,(size)        ; Put size in BC
        ld      c,a
        ld      a,(size+1)
        ld      b,a
        ld      a,(dst)         ; Put destination in HL
        ld      l,a
        ld      a,(dst+1)
        ld      h,a
        ld      a,(src)         ; Put source in DE
        ld      e,a
        ld      a,(src+1)
        ld      d,a
copy:   ld      a,b             ; Get B (remaining bytes)
        or      c               ; Also get C
        jp      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      a,(de)          ; Get byte from source address (DE)
        ld      (hl),a          ; Store byte in destination address (HL)
        inc     de              ; Increment source address
        inc     hl              ; Increment destination address
        dec     bc              ; Decrement count of bytes
        jr      copy            ; Repeat

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
; writing a memory or i/o address. Continuously loops until reset.
; FORMAT:
; P A R <address>
; P A W <address> <data>
; P I R <ioaddress>
; P I W <ioaddress> <data>

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
        jr      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (src),hl        ; Save it
        call    PrintCR
        ld      hl,(src)        ; Get address
ReadLoop:
        ld      a,(hl)          ; Read from address
        jr      ReadLoop        ; Repeat forever

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
        ld      a,(dst)         ; Get data
WriteLoop2:
        ld      (hl),a          ; Write to address
        jr      WriteLoop2      ; Repeat forever

ioRead:
        call    GetByte         ; Get 8-bit I/O address
        jr      c,CancelCmd     ; Cancel if <ESC> pressed
        ld      (src),a         ; Save it
        call    PrintCR
        ld      a,(src)         ; Get  address
        ld      c,a
ReadLoop1:
        in      a,(c)           ; Read from I/O port
        jr      ReadLoop1       ; Repeat forever

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
        ld      a,(dst)         ; Get data
WriteLoop1:
        out     (c),a           ; Write to I/O port
        jr      WriteLoop1      ; Repeat forever

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Utility Routines

SREG:   equ     80h
CREG:   equ     80h
DREG:   equ     81h

; PrintChar
; Output character in A register to console.
; Registers affected: none.

PrintChar:
        push    af              ; Save A register
loop1:  in      a,(SREG)        ; Read status register
        bit     1,A             ; Test TDRE bit
        jr      z,loop1         ; Repeat until TDRE is set
        pop     af              ; Restore A
        out     (DREG),a        ; Output it to data register
        ret                     ; And return

; GetChar
; Read character from console and return in A register. The character
; is not echoed. Waits for character to be entered.
; Registers affected: A.

GetChar:
        in      a,(SREG)        ; Read status register
        bit     0,A             ; Test RDRF bit
        jr      z,GetChar       ; Repeat until RDRF is set
        in      a,(DREG)        ; Read character from data register
        ret                     ; And return

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
        jr      nz,next1        ; Branch if not
        sub     a               ; Set A to zero
        scf                     ; Otherwise set carry and return.
        ret
next1:  cp      '0'             ; Less than '0'?
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
        db      "JMON Monitor 0.5 by Jeff Tranter\r\n",0

strInvalid:
        db      "Invalid command. Type ? for help.\r\n",0

strHelp:
        db      "\r\n"
        db      "Valid commands:\r\n"
        db      "B                             Run BASIC\r\n"
        db      "C <src> <dest> <size>         Copy memory\r\n"
        db      "D <address>                   Dump memory\r\n"
        db      "F <start> <end> <data>        Fill memory\r\n"
        db      "G <address>                   Go\r\n"
        db      "I                             Show info\r\n"
        db      "K <start> <end>               Checksum\r\n"
        db      "L                             Clear screen\r\n"
        db      "P <A/I> <R/W> <addr> [<data>] Scope loop\r\n"
        db      "R                             Examine registers\r\n"
        db      "S <start> <end> <data>        Search memory\r\n"
        db      "T <start> <end>               Test memory\r\n"
        db      "U <address>                   Unassemble memory\r\n"
        db      "V <start> <end> <dest>        Verify memory\r\n"
        db      ": <address> <data>...         Write to memory\r\n"
        db      "= <address> +/- <address>     Hex math calculation\r\n"
        db      "?                             Help\r\n",0

strClearScreen:
        db      ESC,"[2J",01BH,"[H",0     ; VT100/ANSI clear screen, cursor home

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
strRamFound1:
        db      "RAM found from $",0
strRamFound2:
        db      " to $FFFF",0

; Partial Z80 disassembler

; Constants for opcodes. In same order as table that follows.

OP_ADC:  equ    000H
OP_ADD:  equ    001H
OP_AND:  equ    002H
OP_BIT:  equ    003H
OP_CALL: equ    004H
OP_CCF:  equ    005H
OP_CP:   equ    006H
OP_CPD:  equ    007H
OP_CPDR: equ    008H
OP_CPI:  equ    009H
OP_CPIR: equ    00AH
OP_CPL:  equ    00BH
OP_DAA:  equ    00CH
OP_DEC:  equ    00DH
OP_DI:   equ    00EH
OP_DJNZ: equ    00FH
OP_EI:   equ    010H
OP_EX:   equ    011H
OP_EXX:  equ    012H
OP_HALT: equ    013H
OP_IM:   equ    014H
OP_IN:   equ    015H
OP_INC:  equ    016H
OP_IND:  equ    017H
OP_INDR: equ    018H
OP_INI:  equ    019H
OP_INIR: equ    01AH
OP_JP:   equ    01BH
OP_JR:   equ    01CH
OP_LD:   equ    01DH
OP_LDD:  equ    01EH
OP_LDDR: equ    01FH
OP_LDI:  equ    020H
OP_LDIR: equ    021H
OP_NEG:  equ    022H
OP_NOP:  equ    023H
OP_OR:   equ    024H
OP_OTDR: equ    025H
OP_OTIR: equ    026H
OP_OUT:  equ    027H
OP_OUTD: equ    028H
OP_OUTI: equ    029H
OP_POP:  equ    02AH
OP_PUSH: equ    02BH
OP_RES:  equ    02CH
OP_RET:  equ    02DH
OP_RETI: equ    02EH
OP_RETN: equ    02FH
OP_RL:   equ    030H
OP_RLA:  equ    031H
OP_RLC:  equ    032H
OP_RLCA: equ    033H
OP_RLD:  equ    034H
OP_RR:   equ    035H
OP_RRA:  equ    036H
OP_RRC:  equ    037H
OP_RRCA: equ    038H
OP_RRD:  equ    039H
OP_RST:  equ    03AH
OP_SBC:  equ    03BH
OP_SCF:  equ    03CH
OP_SET:  equ    03DH
OP_SLA:  equ    03EH
OP_SRA:  equ    03FH
OP_SRL:  equ    040H
OP_SUB:  equ    041H
OP_XOR:  equ    042H
OP_INV:  equ    043H

; Lookup table of opcode strings, 4 bytes each.

MNEMONICS:
        db      "ADC "          ; $00
        db      "ADD "          ; $01
        db      "AND "          ; $02
        db      "BIT "          ; $03
        db      "CALL"          ; $04
        db      "CCF "          ; $05
        db      "CP  "          ; $06
        db      "CPD "          ; $07
        db      "CPDR"          ; $08
        db      "CPI "          ; $09
        db      "CPIR"          ; $0A
        db      "CPL "          ; $0B
        db      "DAA "          ; $0C
        db      "DEC "          ; $0D
        db      "DI  "          ; $0E
        db      "DJNZ"          ; $0F
        db      "EI  "          ; $10
        db      "EX  "          ; $11
        db      "EXX "          ; $12
        db      "HALT"          ; $13
        db      "IM  "          ; $14
        db      "IN  "          ; $15
        db      "INC "          ; $16
        db      "IND "          ; $17
        db      "INDR"          ; $18
        db      "INI "          ; $19
        db      "INIR"          ; $1A
        db      "JP  "          ; $1B
        db      "JR  "          ; $1C
        db      "LD  "          ; $1D
        db      "LDD "          ; $1E
        db      "LDDR"          ; $1F
        db      "LDI "          ; $20
        db      "LDIR"          ; $21
        db      "NEG "          ; $22
        db      "NOP "          ; $23
        db      "OR  "          ; $24
        db      "OTDR"          ; $25
        db      "OTIR"          ; $26
        db      "OUT "          ; $27
        db      "OUTD"          ; $28
        db      "OUTI"          ; $29
        db      "POP "          ; $2A
        db      "PUSH"          ; $2B
        db      "RES "          ; $2C
        db      "RET "          ; $2D
        db      "RETI"          ; $2E
        db      "RETN"          ; $2F
        db      "RL  "          ; $30
        db      "RLA "          ; $31
        db      "RLC "          ; $32
        db      "RLCA"          ; $33
        db      "RLD "          ; $34
        db      "RR  "          ; $35
        db      "RRA "          ; $36
        db      "RRC "          ; $37
        db      "RRCA"          ; $38
        db      "RRD "          ; $39
        db      "RST "          ; $3A
        db      "SBC "          ; $3B
        db      "SCF "          ; $3C
        db      "SET "          ; $3D
        db      "SLA "          ; $3E
        db      "SRA "          ; $3F
        db      "SRL "          ; $40
        db      "SUB "          ; $41
        db      "XOR "          ; $42
        db      "??? "          ; $43

; Lookup table of opcodes and instruction lengths.

OPCODES:
        db      OP_NOP,  1      ; $00
        db      OP_LD,   3      ; $01
        db      OP_LD,   1      ; $02
        db      OP_INC,  1      ; $03
        db      OP_INC,  1      ; $04
        db      OP_DEC,  1      ; $05
        db      OP_LD,   2      ; $06
        db      OP_RLCA, 1      ; $07
        db      OP_EX,   1      ; $08
        db      OP_ADD,  1      ; $09
        db      OP_LD,   1      ; $0A
        db      OP_DEC,  1      ; $0B
        db      OP_INC,  1      ; $0C
        db      OP_DEC,  1      ; $0D
        db      OP_LD,   2      ; $0E
        db      OP_RRCA, 1      ; $0F
        db      OP_DJNZ, 2      ; $10
        db      OP_LD,   3      ; $11
        db      OP_LD,   1      ; $12
        db      OP_INC,  1      ; $13
        db      OP_INC,  1      ; $14
        db      OP_DEC,  1      ; $15
        db      OP_LD,   2      ; $16
        db      OP_RLA,  1      ; $17
        db      OP_JR,   2      ; $18
        db      OP_ADD,  1      ; $19
        db      OP_LD,   1      ; $1A
        db      OP_DEC,  1      ; $1B
        db      OP_INC,  1      ; $1C
        db      OP_DEC,  1      ; $1D
        db      OP_LD,   2      ; $1E
        db      OP_RRA,  1      ; $1F
        db      OP_JR,   2      ; $20
        db      OP_LD,   3      ; $21
        db      OP_LD,   3      ; $22
        db      OP_INC,  1      ; $23
        db      OP_INC,  1      ; $24
        db      OP_DEC,  1      ; $25
        db      OP_LD,   2      ; $26
        db      OP_DAA,  1      ; $27
        db      OP_JR,   2      ; $28
        db      OP_ADD,  1      ; $29
        db      OP_LD,   3      ; $2A
        db      OP_DEC,  1      ; $2B
        db      OP_INC,  1      ; $2C
        db      OP_DEC,  1      ; $2D
        db      OP_LD,   2      ; $2E
        db      OP_CPL,  1      ; $2F
        db      OP_JR,   2      ; $30
        db      OP_LD,   3      ; $31
        db      OP_LD,   3      ; $32
        db      OP_INC,  1      ; $33
        db      OP_INC,  1      ; $34
        db      OP_DEC,  1      ; $35
        db      OP_LD,   2      ; $36
        db      OP_SCF,  1      ; $37
        db      OP_JR,   2      ; $38
        db      OP_ADD,  1      ; $39
        db      OP_LD,   3      ; $3A
        db      OP_DEC,  1      ; $3B
        db      OP_INC,  1      ; $3C
        db      OP_DEC,  1      ; $3D
        db      OP_LD,   2      ; $3E
        db      OP_CCF,  1      ; $3F
        db      OP_LD,   1      ; $40
        db      OP_LD,   1      ; $41
        db      OP_LD,   1      ; $42
        db      OP_LD,   1      ; $43
        db      OP_LD,   1      ; $44
        db      OP_LD,   1      ; $45
        db      OP_LD,   1      ; $46
        db      OP_LD,   1      ; $47
        db      OP_LD,   1      ; $48
        db      OP_LD,   1      ; $49
        db      OP_LD,   1      ; $4A
        db      OP_LD,   1      ; $4B
        db      OP_LD,   1      ; $4C
        db      OP_LD,   1      ; $4D
        db      OP_LD,   1      ; $4E
        db      OP_LD,   1      ; $4F
        db      OP_LD,   1      ; $50
        db      OP_LD,   1      ; $51
        db      OP_LD,   1      ; $52
        db      OP_LD,   1      ; $53
        db      OP_LD,   1      ; $54
        db      OP_LD,   1      ; $55
        db      OP_LD,   1      ; $56
        db      OP_LD,   1      ; $57
        db      OP_LD,   1      ; $58
        db      OP_LD,   1      ; $59
        db      OP_LD,   1      ; $5A
        db      OP_LD,   1      ; $5B
        db      OP_LD,   1      ; $5C
        db      OP_LD,   1      ; $5D
        db      OP_LD,   1      ; $5E
        db      OP_LD,   1      ; $5F
        db      OP_LD,   1      ; $60
        db      OP_LD,   1      ; $61
        db      OP_LD,   1      ; $62
        db      OP_LD,   1      ; $63
        db      OP_LD,   1      ; $64
        db      OP_LD,   1      ; $65
        db      OP_LD,   1      ; $66
        db      OP_LD,   1      ; $67
        db      OP_LD,   1      ; $68
        db      OP_LD,   1      ; $69
        db      OP_LD,   1      ; $6A
        db      OP_LD,   1      ; $6B
        db      OP_LD,   1      ; $6C
        db      OP_LD,   1      ; $6D
        db      OP_LD,   1      ; $6E
        db      OP_LD,   1      ; $6F
        db      OP_LD,   1      ; $70
        db      OP_LD,   1      ; $71
        db      OP_LD,   1      ; $72
        db      OP_LD,   1      ; $73
        db      OP_LD,   1      ; $74
        db      OP_LD,   1      ; $75
        db      OP_HALT, 1      ; $76
        db      OP_LD,   1      ; $77
        db      OP_LD,   1      ; $78
        db      OP_LD,   1      ; $79
        db      OP_LD,   1      ; $7A
        db      OP_LD,   1      ; $7B
        db      OP_LD,   1      ; $7C
        db      OP_LD,   1      ; $7D
        db      OP_LD,   1      ; $7E
        db      OP_LD,   1      ; $7F
        db      OP_ADD,  1      ; $80
        db      OP_ADD,  1      ; $81
        db      OP_ADD,  1      ; $82
        db      OP_ADD,  1      ; $83
        db      OP_ADD,  1      ; $84
        db      OP_ADD,  1      ; $85
        db      OP_ADD,  1      ; $86
        db      OP_ADD,  1      ; $87
        db      OP_ADC,  1      ; $88
        db      OP_ADC,  1      ; $89
        db      OP_ADC,  1      ; $8A
        db      OP_ADC,  1      ; $8B
        db      OP_ADC,  1      ; $8C
        db      OP_ADC,  1      ; $8D
        db      OP_ADC,  1      ; $8E
        db      OP_ADC,  1      ; $8F
        db      OP_SUB,  1      ; $90
        db      OP_SUB,  1      ; $91
        db      OP_SUB,  1      ; $92
        db      OP_SUB,  1      ; $93
        db      OP_SUB,  1      ; $94
        db      OP_SUB,  1      ; $95
        db      OP_SUB,  1      ; $96
        db      OP_SUB,  1      ; $97
        db      OP_SBC,  1      ; $98
        db      OP_SBC,  1      ; $99
        db      OP_SBC,  1      ; $9A
        db      OP_SBC,  1      ; $9B
        db      OP_SBC,  1      ; $9C
        db      OP_SBC,  1      ; $9D
        db      OP_SBC,  1      ; $9E
        db      OP_SBC,  1      ; $9F
        db      OP_AND,  1      ; $A0
        db      OP_AND,  1      ; $A1
        db      OP_AND,  1      ; $A2
        db      OP_AND,  1      ; $A3
        db      OP_AND,  1      ; $A4
        db      OP_AND,  1      ; $A5
        db      OP_AND,  1      ; $A6
        db      OP_AND,  1      ; $A7
        db      OP_XOR,  1      ; $A8
        db      OP_XOR,  1      ; $A9
        db      OP_XOR,  1      ; $AA
        db      OP_XOR,  1      ; $AB
        db      OP_XOR,  1      ; $AC
        db      OP_XOR,  1      ; $AD
        db      OP_XOR,  1      ; $AE
        db      OP_XOR,  1      ; $AF
        db      OP_OR,   1      ; $B0
        db      OP_OR,   1      ; $B1
        db      OP_OR,   1      ; $B2
        db      OP_OR,   1      ; $B3
        db      OP_OR,   1      ; $B4
        db      OP_OR,   1      ; $B5
        db      OP_OR,   1      ; $B6
        db      OP_OR,   1      ; $B7
        db      OP_CP,   1      ; $B8
        db      OP_CP,   1      ; $B9
        db      OP_CP,   1      ; $BA
        db      OP_CP,   1      ; $BB
        db      OP_CP,   1      ; $BC
        db      OP_CP,   1      ; $BD
        db      OP_CP,   1      ; $BE
        db      OP_CP,   1      ; $BF
        db      OP_RET,  1      ; $C0
        db      OP_POP,  1      ; $C1
        db      OP_JP,   3      ; $C2
        db      OP_JP,   3      ; $C3
        db      OP_CALL, 3      ; $C4
        db      OP_PUSH, 1      ; $C5
        db      OP_ADD,  2      ; $C6
        db      OP_RST,  1      ; $C7
        db      OP_RET,  1      ; $C8
        db      OP_RET,  1      ; $C9
        db      OP_JP,   3      ; $CA
        db      OP_INV,  1      ; $CB
        db      OP_CALL, 3      ; $CC
        db      OP_CALL, 3      ; $CD
        db      OP_ADC,  2      ; $CE
        db      OP_RST,  1      ; $CF
        db      OP_RET,  1      ; $D0
        db      OP_POP,  1      ; $D1
        db      OP_JP,   3      ; $D2
        db      OP_OUT,  2      ; $D3
        db      OP_CALL, 3      ; $D4
        db      OP_PUSH, 1      ; $D5
        db      OP_SUB,  2      ; $D6
        db      OP_RST,  1      ; $D7
        db      OP_RET,  1      ; $D8
        db      OP_EXX,  1      ; $D9
        db      OP_JP,   3      ; $DA
        db      OP_IN,   2      ; $DB
        db      OP_CALL, 3      ; $DC
        db      OP_INV,  1      ; $DD
        db      OP_SBC,  2      ; $DE
        db      OP_RST,  1      ; $DF
        db      OP_RET,  1      ; $E0
        db      OP_POP,  1      ; $E1
        db      OP_JP,   3      ; $E2
        db      OP_EX,   1      ; $E3
        db      OP_CALL, 3      ; $E4
        db      OP_PUSH, 1      ; $E5
        db      OP_AND,  2      ; $E6
        db      OP_RST,  1      ; $E7
        db      OP_RET,  1      ; $E8
        db      OP_JP,   1      ; $E9
        db      OP_JP,   3      ; $EA
        db      OP_EX,   1      ; $EB
        db      OP_CALL, 3      ; $EC
        db      OP_INV,  1      ; $ED
        db      OP_XOR,  2      ; $EE
        db      OP_RST,  1      ; $EF
        db      OP_RET,  1      ; $F0
        db      OP_POP,  1      ; $F1
        db      OP_JP,   3      ; $F2
        db      OP_DI,   1      ; $F3
        db      OP_CALL, 3      ; $F4
        db      OP_PUSH, 1      ; $F5
        db      OP_OR,   2      ; $F6
        db      OP_RST,  1      ; $F7
        db      OP_RET,  1      ; $F8
        db      OP_LD,   1      ; $F9
        db      OP_JP,   3      ; $FA
        db      OP_EI,   1      ; $FB
        db      OP_CALL, 3      ; $FC
        db      OP_INV,  1      ; $FD
        db      OP_CP,   2      ; $FE
        db      OP_RST,  1      ; $FF

; Lookup tables of opcodes and instruction lengths for multibyte
; instructions: 0xcb, 0xdd, 0xed, 0xfd. We can combine 0xdd and 0xfd
; since instructions are the same (other than addressing mode).

CBOPCODES:
        db      OP_RLC,  2      ; $CB00
        db      OP_RLC,  2      ; $CB01
        db      OP_RLC,  2      ; $CB02
        db      OP_RLC,  2      ; $CB03
        db      OP_RLC,  2      ; $CB04
        db      OP_RLC,  2      ; $CB05
        db      OP_RLC,  2      ; $CB06
        db      OP_RLC,  2      ; $CB07
        db      OP_RRC,  2      ; $CB08
        db      OP_RRC,  2      ; $CB09
        db      OP_RRC,  2      ; $CB0A
        db      OP_RRC,  2      ; $CB0B
        db      OP_RRC,  2      ; $CB0C
        db      OP_RRC,  2      ; $CB0D
        db      OP_RRC,  2      ; $CB0E
        db      OP_RRC,  2      ; $CB0F
        db      OP_RL,   2      ; $CB10
        db      OP_RL,   2      ; $CB11
        db      OP_RL,   2      ; $CB12
        db      OP_RL,   2      ; $CB13
        db      OP_RL,   2      ; $CB14
        db      OP_RL,   2      ; $CB15
        db      OP_RL,   2      ; $CB16
        db      OP_RL,   2      ; $CB17
        db      OP_RR,   2      ; $CB18
        db      OP_RR,   2      ; $CB19
        db      OP_RR,   2      ; $CB1A
        db      OP_RR,   2      ; $CB1B
        db      OP_RR,   2      ; $CB1C
        db      OP_RR,   2      ; $CB1D
        db      OP_RR,   2      ; $CB1E
        db      OP_RR,   2      ; $CB1F
        db      OP_SLA,  2      ; $CB20
        db      OP_SLA,  2      ; $CB21
        db      OP_SLA,  2      ; $CB22
        db      OP_SLA,  2      ; $CB23
        db      OP_SLA,  2      ; $CB24
        db      OP_SLA,  2      ; $CB25
        db      OP_SLA,  2      ; $CB26
        db      OP_SLA,  2      ; $CB27
        db      OP_SRA,  2      ; $CB28
        db      OP_SRA,  2      ; $CB29
        db      OP_SRA,  2      ; $CB2A
        db      OP_SRA,  2      ; $CB2B
        db      OP_SRA,  2      ; $CB2C
        db      OP_SRA,  2      ; $CB2D
        db      OP_SRA,  2      ; $CB2E
        db      OP_SRA,  2      ; $CB2F
        db      OP_INV,  1      ; $CD30
        db      OP_INV,  1      ; $CD31
        db      OP_INV,  1      ; $CD32
        db      OP_INV,  1      ; $CD33
        db      OP_INV,  1      ; $CD34
        db      OP_INV,  1      ; $CD35
        db      OP_INV,  1      ; $CD36
        db      OP_INV,  1      ; $CD37
        db      OP_SRL,  2      ; $CB38
        db      OP_SRL,  2      ; $CB39
        db      OP_SRL,  2      ; $CB3A
        db      OP_SRL,  2      ; $CB3B
        db      OP_SRL,  2      ; $CB3C
        db      OP_SRL,  2      ; $CB3D
        db      OP_SRL,  2      ; $CB3E
        db      OP_SRL,  2      ; $CB3F
        db      OP_BIT,  2      ; $CB40
        db      OP_BIT,  2      ; $CB41
        db      OP_BIT,  2      ; $CB42
        db      OP_BIT,  2      ; $CB43
        db      OP_BIT,  2      ; $CB44
        db      OP_BIT,  2      ; $CB45
        db      OP_BIT,  2      ; $CB46
        db      OP_BIT,  2      ; $CB47
        db      OP_BIT,  2      ; $CB48
        db      OP_BIT,  2      ; $CB49
        db      OP_BIT,  2      ; $CB4A
        db      OP_BIT,  2      ; $CB4B
        db      OP_BIT,  2      ; $CB4C
        db      OP_BIT,  2      ; $CB4D
        db      OP_BIT,  2      ; $CB4E
        db      OP_BIT,  2      ; $CB4F
        db      OP_BIT,  2      ; $CB50
        db      OP_BIT,  2      ; $CB51
        db      OP_BIT,  2      ; $CB52
        db      OP_BIT,  2      ; $CB53
        db      OP_BIT,  2      ; $CB54
        db      OP_BIT,  2      ; $CB55
        db      OP_BIT,  2      ; $CB56
        db      OP_BIT,  2      ; $CB57
        db      OP_BIT,  2      ; $CB58
        db      OP_BIT,  2      ; $CB59
        db      OP_BIT,  2      ; $CB5A
        db      OP_BIT,  2      ; $CB5B
        db      OP_BIT,  2      ; $CB5C
        db      OP_BIT,  2      ; $CB5D
        db      OP_BIT,  2      ; $CB5E
        db      OP_BIT,  2      ; $CB5F
        db      OP_BIT,  2      ; $CB60
        db      OP_BIT,  2      ; $CB61
        db      OP_BIT,  2      ; $CB62
        db      OP_BIT,  2      ; $CB63
        db      OP_BIT,  2      ; $CB64
        db      OP_BIT,  2      ; $CB65
        db      OP_BIT,  2      ; $CB66
        db      OP_BIT,  2      ; $CB67
        db      OP_BIT,  2      ; $CB68
        db      OP_BIT,  2      ; $CB69
        db      OP_BIT,  2      ; $CB6A
        db      OP_BIT,  2      ; $CB6B
        db      OP_BIT,  2      ; $CB6C
        db      OP_BIT,  2      ; $CB6D
        db      OP_BIT,  2      ; $CB6E
        db      OP_BIT,  2      ; $CB6F
        db      OP_BIT,  2      ; $CB70
        db      OP_BIT,  2      ; $CB71
        db      OP_BIT,  2      ; $CB72
        db      OP_BIT,  2      ; $CB73
        db      OP_BIT,  2      ; $CB74
        db      OP_BIT,  2      ; $CB75
        db      OP_BIT,  2      ; $CB76
        db      OP_BIT,  2      ; $CB77
        db      OP_BIT,  2      ; $CB78
        db      OP_BIT,  2      ; $CB79
        db      OP_BIT,  2      ; $CB7A
        db      OP_BIT,  2      ; $CB7B
        db      OP_BIT,  2      ; $CB7C
        db      OP_BIT,  2      ; $CB7D
        db      OP_BIT,  2      ; $CB7E
        db      OP_BIT,  2      ; $CB7F
        db      OP_RES,  2      ; $CB80
        db      OP_RES,  2      ; $CB81
        db      OP_RES,  2      ; $CB82
        db      OP_RES,  2      ; $CB83
        db      OP_RES,  2      ; $CB84
        db      OP_RES,  2      ; $CB85
        db      OP_RES,  2      ; $CB86
        db      OP_RES,  2      ; $CB87
        db      OP_RES,  2      ; $CB88
        db      OP_RES,  2      ; $CB89
        db      OP_RES,  2      ; $CB8A
        db      OP_RES,  2      ; $CB8B
        db      OP_RES,  2      ; $CB8C
        db      OP_RES,  2      ; $CB8D
        db      OP_RES,  2      ; $CB8E
        db      OP_RES,  2      ; $CB8F
        db      OP_RES,  2      ; $CB90
        db      OP_RES,  2      ; $CB91
        db      OP_RES,  2      ; $CB92
        db      OP_RES,  2      ; $CB93
        db      OP_RES,  2      ; $CB94
        db      OP_RES,  2      ; $CB95
        db      OP_RES,  2      ; $CB96
        db      OP_RES,  2      ; $CB97
        db      OP_RES,  2      ; $CB98
        db      OP_RES,  2      ; $CB99
        db      OP_RES,  2      ; $CB9A
        db      OP_RES,  2      ; $CB9B
        db      OP_RES,  2      ; $CB9C
        db      OP_RES,  2      ; $CB9D
        db      OP_RES,  2      ; $CB9E
        db      OP_RES,  2      ; $CB9F
        db      OP_RES,  2      ; $CBA0
        db      OP_RES,  2      ; $CBA1
        db      OP_RES,  2      ; $CBA2
        db      OP_RES,  2      ; $CBA3
        db      OP_RES,  2      ; $CBA4
        db      OP_RES,  2      ; $CBA5
        db      OP_RES,  2      ; $CBA6
        db      OP_RES,  2      ; $CBA7
        db      OP_RES,  2      ; $CBA8
        db      OP_RES,  2      ; $CBA9
        db      OP_RES,  2      ; $CBAA
        db      OP_RES,  2      ; $CBAB
        db      OP_RES,  2      ; $CBAC
        db      OP_RES,  2      ; $CBAD
        db      OP_RES,  2      ; $CBAE
        db      OP_RES,  2      ; $CBAF
        db      OP_RES,  2      ; $CBB0
        db      OP_RES,  2      ; $CBB1
        db      OP_RES,  2      ; $CBB2
        db      OP_RES,  2      ; $CBB3
        db      OP_RES,  2      ; $CBB4
        db      OP_RES,  2      ; $CBB5
        db      OP_RES,  2      ; $CBB6
        db      OP_RES,  2      ; $CBB7
        db      OP_RES,  2      ; $CBB8
        db      OP_RES,  2      ; $CBB9
        db      OP_RES,  2      ; $CBBA
        db      OP_RES,  2      ; $CBBB
        db      OP_RES,  2      ; $CBBC
        db      OP_RES,  2      ; $CBBD
        db      OP_RES,  2      ; $CBBE
        db      OP_RES,  2      ; $CBBF
        db      OP_SET,  2      ; $CBC0
        db      OP_SET,  2      ; $CBC1
        db      OP_SET,  2      ; $CBC2
        db      OP_SET,  2      ; $CBC3
        db      OP_SET,  2      ; $CBC4
        db      OP_SET,  2      ; $CBC5
        db      OP_SET,  2      ; $CBC6
        db      OP_SET,  2      ; $CBC7
        db      OP_SET,  2      ; $CBC8
        db      OP_SET,  2      ; $CBC9
        db      OP_SET,  2      ; $CBCA
        db      OP_SET,  2      ; $CBCB
        db      OP_SET,  2      ; $CBCC
        db      OP_SET,  2      ; $CBCD
        db      OP_SET,  2      ; $CBCE
        db      OP_SET,  2      ; $CBCF
        db      OP_SET,  2      ; $CBD0
        db      OP_SET,  2      ; $CBD1
        db      OP_SET,  2      ; $CBD2
        db      OP_SET,  2      ; $CBD3
        db      OP_SET,  2      ; $CBD4
        db      OP_SET,  2      ; $CBD5
        db      OP_SET,  2      ; $CBD6
        db      OP_SET,  2      ; $CBD7
        db      OP_SET,  2      ; $CBD8
        db      OP_SET,  2      ; $CBD9
        db      OP_SET,  2      ; $CBDA
        db      OP_SET,  2      ; $CBDB
        db      OP_SET,  2      ; $CBDC
        db      OP_SET,  2      ; $CBDD
        db      OP_SET,  2      ; $CBDE
        db      OP_SET,  2      ; $CBDF
        db      OP_SET,  2      ; $CBE0
        db      OP_SET,  2      ; $CBE1
        db      OP_SET,  2      ; $CBE2
        db      OP_SET,  2      ; $CBE3
        db      OP_SET,  2      ; $CBE4
        db      OP_SET,  2      ; $CBE5
        db      OP_SET,  2      ; $CBE6
        db      OP_SET,  2      ; $CBE7
        db      OP_SET,  2      ; $CBE8
        db      OP_SET,  2      ; $CBE9
        db      OP_SET,  2      ; $CBEA
        db      OP_SET,  2      ; $CBEB
        db      OP_SET,  2      ; $CBEC
        db      OP_SET,  2      ; $CBED
        db      OP_SET,  2      ; $CBEE
        db      OP_SET,  2      ; $CBEF
        db      OP_SET,  2      ; $CBF0
        db      OP_SET,  2      ; $CBF1
        db      OP_SET,  2      ; $CBF2
        db      OP_SET,  2      ; $CBF3
        db      OP_SET,  2      ; $CBF4
        db      OP_SET,  2      ; $CBF5
        db      OP_SET,  2      ; $CBF6
        db      OP_SET,  2      ; $CBF7
        db      OP_SET,  2      ; $CBF8
        db      OP_SET,  2      ; $CBF9
        db      OP_SET,  2      ; $CBFA
        db      OP_SET,  2      ; $CBFB
        db      OP_SET,  2      ; $CBFC
        db      OP_SET,  2      ; $CBFD
        db      OP_SET,  2      ; $CBFE
        db      OP_SET,  2      ; $CBFF

DDOPCODES:
        db      OP_INV,  1      ; $DD00
        db      OP_INV,  1      ; $DD01
        db      OP_INV,  1      ; $DD02
        db      OP_INV,  1      ; $DD03
        db      OP_INV,  1      ; $DD04
        db      OP_INV,  1      ; $DD05
        db      OP_INV,  1      ; $DD06
        db      OP_INV,  1      ; $DD07
        db      OP_INV,  1      ; $DD08
        db      OP_ADD,  2      ; $DD09
        db      OP_INV,  1      ; $DD0A
        db      OP_INV,  1      ; $DD0B
        db      OP_INV,  1      ; $DD0C
        db      OP_INV,  1      ; $DD0D
        db      OP_INV,  1      ; $DD0E
        db      OP_INV,  1      ; $DD0F
        db      OP_INV,  1      ; $DD10
        db      OP_INV,  1      ; $DD11
        db      OP_INV,  1      ; $DD12
        db      OP_INV,  1      ; $DD13
        db      OP_INV,  1      ; $DD14
        db      OP_INV,  1      ; $DD15
        db      OP_INV,  1      ; $DD16
        db      OP_INV,  1      ; $DD17
        db      OP_INV,  1      ; $DD18
        db      OP_ADD,  2      ; $DD19
        db      OP_INV,  1      ; $DD1A
        db      OP_INV,  1      ; $DD1B
        db      OP_INV,  1      ; $DD1C
        db      OP_INV,  1      ; $DD1D
        db      OP_INV,  1      ; $DD1E
        db      OP_INV,  1      ; $DD1F
        db      OP_INV,  1      ; $DD20
        db      OP_LD,   4      ; $DD21
        db      OP_LD,   4      ; $DD22
        db      OP_INC,  2      ; $DD23
        db      OP_INV,  1      ; $DD24
        db      OP_INV,  1      ; $DD25
        db      OP_INV,  1      ; $DD26
        db      OP_INV,  1      ; $DD27
        db      OP_INV,  1      ; $DD28
        db      OP_ADD,  2      ; $DD29
        db      OP_LD,   4      ; $DD2A
        db      OP_DEC,  2      ; $DD2B
        db      OP_INV,  1      ; $DD2C
        db      OP_INV,  1      ; $DD2D
        db      OP_INV,  1      ; $DD2E
        db      OP_INV,  1      ; $DD2F
        db      OP_INV,  1      ; $DD30
        db      OP_INV,  1      ; $DD31
        db      OP_INV,  1      ; $DD32
        db      OP_INV,  1      ; $DD33
        db      OP_INC,  3      ; $DD34
        db      OP_DEC,  3      ; $DD35
        db      OP_LD,   4      ; $DD36
        db      OP_INV,  1      ; $DD37
        db      OP_INV,  1      ; $DD38
        db      OP_ADD,  2      ; $DD39
        db      OP_INV,  1      ; $DD3A
        db      OP_INV,  1      ; $DD3B
        db      OP_INV,  1      ; $DD3C
        db      OP_INV,  1      ; $DD3D
        db      OP_INV,  1      ; $DD3E
        db      OP_INV,  1      ; $DD3F
        db      OP_INV,  1      ; $DD40
        db      OP_INV,  1      ; $DD41
        db      OP_INV,  1      ; $DD42
        db      OP_INV,  1      ; $DD43
        db      OP_INV,  1      ; $DD44
        db      OP_INV,  1      ; $DD45
        db      OP_LD,   3      ; $DD46
        db      OP_INV,  1      ; $DD47
        db      OP_INV,  1      ; $DD48
        db      OP_INV,  1      ; $DD49
        db      OP_INV,  1      ; $DD4A
        db      OP_INV,  1      ; $DD4B
        db      OP_INV,  1      ; $DD4C
        db      OP_INV,  1      ; $DD4D
        db      OP_LD,   3      ; $DD4E
        db      OP_INV,  1      ; $DD4F
        db      OP_INV,  1      ; $DD50
        db      OP_INV,  1      ; $DD51
        db      OP_INV,  1      ; $DD52
        db      OP_INV,  1      ; $DD53
        db      OP_INV,  1      ; $DD54
        db      OP_INV,  1      ; $DD55
        db      OP_LD,   3      ; $DD56
        db      OP_INV,  1      ; $DD57
        db      OP_INV,  1      ; $DD58
        db      OP_INV,  1      ; $DD59
        db      OP_INV,  1      ; $DD5A
        db      OP_INV,  1      ; $DD5B
        db      OP_INV,  1      ; $DD5C
        db      OP_INV,  1      ; $DD5D
        db      OP_LD,   3      ; $DD5E
        db      OP_INV,  1      ; $DD5F
        db      OP_INV,  1      ; $DD60
        db      OP_INV,  1      ; $DD61
        db      OP_INV,  1      ; $DD62
        db      OP_INV,  1      ; $DD63
        db      OP_INV,  1      ; $DD64
        db      OP_INV,  1      ; $DD65
        db      OP_LD,   3      ; $DD66
        db      OP_INV,  1      ; $DD67
        db      OP_INV,  1      ; $DD68
        db      OP_INV,  1      ; $DD69
        db      OP_INV,  1      ; $DD6A
        db      OP_INV,  1      ; $DD6B
        db      OP_INV,  1      ; $DD6C
        db      OP_INV,  1      ; $DD6D
        db      OP_LD,   3      ; $DD6E
        db      OP_INV,  1      ; $DD6F
        db      OP_LD,   3      ; $DD70
        db      OP_LD,   3      ; $DD71
        db      OP_LD,   3      ; $DD72
        db      OP_LD,   3      ; $DD73
        db      OP_LD,   3      ; $DD74
        db      OP_LD,   3      ; $DD75
        db      OP_INV,  1      ; $DD76
        db      OP_LD,   3      ; $DD77
        db      OP_INV,  1      ; $DD78
        db      OP_INV,  1      ; $DD79
        db      OP_INV,  1      ; $DD7A
        db      OP_INV,  1      ; $DD7B
        db      OP_INV,  1      ; $DD7C
        db      OP_INV,  1      ; $DD7D
        db      OP_LD,   3      ; $DD7E
        db      OP_INV,  1      ; $DD7F
        db      OP_INV,  1      ; $DD80
        db      OP_INV,  1      ; $DD81
        db      OP_INV,  1      ; $DD82
        db      OP_INV,  1      ; $DD83
        db      OP_INV,  1      ; $DD84
        db      OP_INV,  1      ; $DD85
        db      OP_ADD,  3      ; $DD86
        db      OP_INV,  1      ; $DD87
        db      OP_INV,  1      ; $DD88
        db      OP_INV,  1      ; $DD89
        db      OP_INV,  1      ; $DD8A
        db      OP_INV,  1      ; $DD8B
        db      OP_INV,  1      ; $DD8C
        db      OP_INV,  1      ; $DD8D
        db      OP_ADC,  3      ; $DD8E
        db      OP_INV,  1      ; $DD8F
        db      OP_INV,  1      ; $DD90
        db      OP_INV,  1      ; $DD91
        db      OP_INV,  1      ; $DD92
        db      OP_INV,  1      ; $DD93
        db      OP_INV,  1      ; $DD94
        db      OP_INV,  1      ; $DD95
        db      OP_SUB,  3      ; $DD96
        db      OP_INV,  1      ; $DD97
        db      OP_INV,  1      ; $DD98
        db      OP_INV,  1      ; $DD99
        db      OP_INV,  1      ; $DD9A
        db      OP_INV,  1      ; $DD9B
        db      OP_INV,  1      ; $DD9C
        db      OP_INV,  1      ; $DD9D
        db      OP_SBC,  3      ; $DD9E
        db      OP_INV,  1      ; $DD9F
        db      OP_INV,  1      ; $DDA0
        db      OP_INV,  1      ; $DDA1
        db      OP_INV,  1      ; $DDA2
        db      OP_INV,  1      ; $DDA3
        db      OP_INV,  1      ; $DDA4
        db      OP_INV,  1      ; $DDA5
        db      OP_AND,  3      ; $DDA6
        db      OP_INV,  1      ; $DDA7
        db      OP_INV,  1      ; $DDA8
        db      OP_INV,  1      ; $DDA9
        db      OP_INV,  1      ; $DDAA
        db      OP_INV,  1      ; $DDAB
        db      OP_INV,  1      ; $DDAC
        db      OP_INV,  1      ; $DDAD
        db      OP_XOR,  3      ; $DDAE
        db      OP_INV,  1      ; $DDAF
        db      OP_INV,  1      ; $DDB0
        db      OP_INV,  1      ; $DDB1
        db      OP_INV,  1      ; $DDB2
        db      OP_INV,  1      ; $DDB3
        db      OP_INV,  1      ; $DDB4
        db      OP_INV,  1      ; $DDB5
        db      OP_OR,   3      ; $DDB6
        db      OP_INV,  1      ; $DDB7
        db      OP_INV,  1      ; $DDB8
        db      OP_INV,  1      ; $DDB9
        db      OP_INV,  1      ; $DDBA
        db      OP_INV,  1      ; $DDBB
        db      OP_INV,  1      ; $DDBC
        db      OP_INV,  1      ; $DDBD
        db      OP_CP,   3      ; $DDBE
        db      OP_INV,  1      ; $DDBF
        db      OP_INV,  1      ; $DDC0
        db      OP_INV,  1      ; $DDC1
        db      OP_INV,  1      ; $DDC2
        db      OP_INV,  1      ; $DDC3
        db      OP_INV,  1      ; $DDC4
        db      OP_INV,  1      ; $DDC5
        db      OP_INV,  1      ; $DDC6
        db      OP_INV,  1      ; $DDC7
        db      OP_INV,  1      ; $DDC8
        db      OP_INV,  1      ; $DDC9
        db      OP_INV,  1      ; $DDCA
        db      OP_INV,  1      ; $DDCB
        db      OP_INV,  1      ; $DDCC
        db      OP_INV,  1      ; $DDCD
        db      OP_INV,  1      ; $DDCE
        db      OP_INV,  1      ; $DDCF
        db      OP_INV,  1      ; $DDD0
        db      OP_INV,  1      ; $DDD1
        db      OP_INV,  1      ; $DDD2
        db      OP_INV,  1      ; $DDD3
        db      OP_INV,  1      ; $DDD4
        db      OP_INV,  1      ; $DDD5
        db      OP_INV,  1      ; $DDD6
        db      OP_INV,  1      ; $DDD7
        db      OP_INV,  1      ; $DDD8
        db      OP_INV,  1      ; $DDD9
        db      OP_INV,  1      ; $DDDA
        db      OP_INV,  1      ; $DDDB
        db      OP_INV,  1      ; $DDDC
        db      OP_INV,  1      ; $DDDD
        db      OP_INV,  1      ; $DDDE
        db      OP_INV,  1      ; $DDDF
        db      OP_INV,  1      ; $DDE0
        db      OP_INV,  1      ; $DDE1
        db      OP_INV,  1      ; $DDE2
        db      OP_INV,  1      ; $DDE3
        db      OP_INV,  1      ; $DDE4
        db      OP_INV,  1      ; $DDE5
        db      OP_INV,  1      ; $DDE6
        db      OP_INV,  1      ; $DDE7
        db      OP_INV,  1      ; $DDE8
        db      OP_INV,  1      ; $DDE9
        db      OP_INV,  1      ; $DDEA
        db      OP_INV,  1      ; $DDEB
        db      OP_INV,  1      ; $DDEC
        db      OP_INV,  1      ; $DDED
        db      OP_INV,  1      ; $DDDE
        db      OP_INV,  1      ; $DDDF
        db      OP_INV,  1      ; $DDF0
        db      OP_INV,  1      ; $DDF1
        db      OP_INV,  1      ; $DDF2
        db      OP_INV,  1      ; $DDF3
        db      OP_INV,  1      ; $DDF4
        db      OP_INV,  1      ; $DDF5
        db      OP_INV,  1      ; $DDF6
        db      OP_INV,  1      ; $DDF7
        db      OP_INV,  1      ; $DDF8
        db      OP_INV,  1      ; $DDF9
        db      OP_INV,  1      ; $DDFA
        db      OP_INV,  1      ; $DDFB
        db      OP_INV,  1      ; $DDFC
        db      OP_INV,  1      ; $DDFD
        db      OP_INV,  1      ; $DDFE
        db      OP_INV,  1      ; $DDFF


EDOPCODES:
        db      OP_INV,  1      ; $ED00
        db      OP_INV,  1      ; $ED01
        db      OP_INV,  1      ; $ED02
        db      OP_INV,  1      ; $ED03
        db      OP_INV,  1      ; $ED04
        db      OP_INV,  1      ; $ED05
        db      OP_INV,  1      ; $ED06
        db      OP_INV,  1      ; $ED07
        db      OP_INV,  1      ; $ED08
        db      OP_INV,  1      ; $ED09
        db      OP_INV,  1      ; $ED0A
        db      OP_INV,  1      ; $ED0B
        db      OP_INV,  1      ; $ED0C
        db      OP_INV,  1      ; $ED0D
        db      OP_INV,  1      ; $ED0E
        db      OP_INV,  1      ; $ED0F
        db      OP_INV,  1      ; $ED10
        db      OP_INV,  1      ; $ED11
        db      OP_INV,  1      ; $ED12
        db      OP_INV,  1      ; $ED13
        db      OP_INV,  1      ; $ED14
        db      OP_INV,  1      ; $ED15
        db      OP_INV,  1      ; $ED16
        db      OP_INV,  1      ; $ED17
        db      OP_INV,  1      ; $ED18
        db      OP_INV,  1      ; $ED19
        db      OP_INV,  1      ; $ED1A
        db      OP_INV,  1      ; $ED1B
        db      OP_INV,  1      ; $ED1C
        db      OP_INV,  1      ; $ED1D
        db      OP_INV,  1      ; $ED1E
        db      OP_INV,  1      ; $ED1F
        db      OP_INV,  1      ; $ED20
        db      OP_INV,  1      ; $ED21
        db      OP_INV,  1      ; $ED22
        db      OP_INV,  1      ; $ED23
        db      OP_INV,  1      ; $ED24
        db      OP_INV,  1      ; $ED25
        db      OP_INV,  1      ; $ED26
        db      OP_INV,  1      ; $ED27
        db      OP_INV,  1      ; $ED28
        db      OP_INV,  1      ; $ED29
        db      OP_INV,  1      ; $ED2A
        db      OP_INV,  1      ; $ED2B
        db      OP_INV,  1      ; $ED2C
        db      OP_INV,  1      ; $ED2D
        db      OP_INV,  1      ; $ED2E
        db      OP_INV,  1      ; $ED2F
        db      OP_INV,  1      ; $ED30
        db      OP_INV,  1      ; $ED31
        db      OP_INV,  1      ; $ED32
        db      OP_INV,  1      ; $ED33
        db      OP_INV,  1      ; $ED34
        db      OP_INV,  1      ; $ED35
        db      OP_INV,  1      ; $ED36
        db      OP_INV,  1      ; $ED37
        db      OP_INV,  1      ; $ED38
        db      OP_INV,  1      ; $ED39
        db      OP_INV,  1      ; $ED3A
        db      OP_INV,  1      ; $ED3B
        db      OP_INV,  1      ; $ED3C
        db      OP_INV,  1      ; $ED3D
        db      OP_INV,  1      ; $ED3E
        db      OP_INV,  1      ; $ED3F
        db      OP_IN,   2      ; $ED40
        db      OP_OUT,  2      ; $ED41
        db      OP_SBC,  2      ; $ED42
        db      OP_LD,   4      ; $ED43
        db      OP_NEG,  2      ; $ED44
        db      OP_RETN, 2      ; $ED45
        db      OP_IM,   2      ; $ED46
        db      OP_LD,   2      ; $ED47
        db      OP_IN,   2      ; $ED48
        db      OP_OUT,  2      ; $ED49
        db      OP_ADC,  2      ; $ED4A
        db      OP_LD,   4      ; $ED4B
        db      OP_INV,  1      ; $ED4C
        db      OP_RETI, 2      ; $ED4D
        db      OP_INV,  1      ; $ED4E
        db      OP_LD,   2      ; $ED4F
        db      OP_IN,   2      ; $ED50
        db      OP_OUT,  2      ; $ED51
        db      OP_SBC,  2      ; $ED52
        db      OP_LD,   4      ; $ED53
        db      OP_INV,  1      ; $ED54
        db      OP_INV,  1      ; $ED55
        db      OP_IM,   2      ; $ED56
        db      OP_LD,   2      ; $ED57
        db      OP_IN,   2      ; $ED58
        db      OP_OUT,  2      ; $ED59
        db      OP_ADC,  2      ; $ED5A
        db      OP_LD,   4      ; $ED5B
        db      OP_INV,  1      ; $ED5C
        db      OP_INV,  1      ; $ED5D
        db      OP_IM,   2      ; $ED5E
        db      OP_LD,   2      ; $ED5F
        db      OP_IN,   2      ; $ED60
        db      OP_OUT,  2      ; $ED61
        db      OP_SBC,  2      ; $ED62
        db      OP_INV,  2      ; $ED63
        db      OP_INV,  2      ; $ED64
        db      OP_INV,  2      ; $ED65
        db      OP_INV,  2      ; $ED66
        db      OP_RRD,  2      ; $ED67
        db      OP_IN,   2      ; $ED68
        db      OP_OUT,  2      ; $ED69
        db      OP_ADC,  2      ; $ED6A
        db      OP_INV,  2      ; $ED6B
        db      OP_INV,  2      ; $ED6C
        db      OP_INV,  2      ; $ED6D
        db      OP_INV,  2      ; $ED6E
        db      OP_RLD,  2      ; $ED6F
        db      OP_INV,  2      ; $ED70
        db      OP_INV,  2      ; $ED71
        db      OP_SBC,  2      ; $ED72
        db      OP_LD,   4      ; $ED73
        db      OP_INV,  2      ; $ED74
        db      OP_INV,  2      ; $ED75
        db      OP_IN,   2      ; $ED76
        db      OP_INV,  2      ; $ED77
        db      OP_INV,  2      ; $ED78
        db      OP_OUT,  2      ; $ED79
        db      OP_ADC,  2      ; $ED7A
        db      OP_LD,   4      ; $ED7B
        db      OP_INV,  2      ; $ED7C
        db      OP_INV,  2      ; $ED7D
        db      OP_INV,  2      ; $ED7E
        db      OP_INV,  2      ; $ED7F
        db      OP_INV,  2      ; $ED80
        db      OP_INV,  2      ; $ED81
        db      OP_INV,  2      ; $ED82
        db      OP_INV,  2      ; $ED83
        db      OP_INV,  2      ; $ED84
        db      OP_INV,  2      ; $ED85
        db      OP_INV,  2      ; $ED86
        db      OP_INV,  2      ; $ED87
        db      OP_INV,  2      ; $ED88
        db      OP_INV,  2      ; $ED89
        db      OP_INV,  2      ; $ED8A
        db      OP_INV,  2      ; $ED8B
        db      OP_INV,  2      ; $ED8C
        db      OP_INV,  2      ; $ED8D
        db      OP_INV,  2      ; $ED8E
        db      OP_INV,  2      ; $ED8F
        db      OP_INV,  2      ; $ED90
        db      OP_INV,  2      ; $ED91
        db      OP_INV,  2      ; $ED92
        db      OP_INV,  2      ; $ED93
        db      OP_INV,  2      ; $ED94
        db      OP_INV,  2      ; $ED95
        db      OP_INV,  2      ; $ED96
        db      OP_INV,  2      ; $ED97
        db      OP_INV,  2      ; $ED98
        db      OP_INV,  2      ; $ED99
        db      OP_INV,  2      ; $ED9A
        db      OP_INV,  2      ; $ED9B
        db      OP_INV,  2      ; $ED9C
        db      OP_INV,  2      ; $ED9D
        db      OP_INV,  2      ; $ED9E
        db      OP_INV,  2      ; $ED9F
        db      OP_LDI,  2      ; $EDA0
        db      OP_CPI,  2      ; $EDA1
        db      OP_INI,  2      ; $EDA2
        db      OP_OUTI, 2      ; $EDA3
        db      OP_INV,  2      ; $EDA4
        db      OP_INV,  2      ; $EDA5
        db      OP_INV,  2      ; $EDA6
        db      OP_INV,  2      ; $EDA7
        db      OP_LDD,  2      ; $EDA8
        db      OP_CPD,  2      ; $EDA9
        db      OP_IND,  2      ; $EDAA
        db      OP_OUTD, 2      ; $EDAB
        db      OP_INV,  2      ; $EDAC
        db      OP_INV,  2      ; $EDAD
        db      OP_INV,  2      ; $EDA
        db      OP_INV,  2      ; $EDAF
        db      OP_LDIR, 2      ; $EDB0
        db      OP_CPIR, 2      ; $EDB1
        db      OP_INIR, 2      ; $EDB2
        db      OP_OTIR, 2      ; $EDB3
        db      OP_INV,  2      ; $EDB4
        db      OP_INV,  2      ; $EDB5
        db      OP_INV,  2      ; $EDB6
        db      OP_INV,  2      ; $EDB7
        db      OP_LDDR, 2      ; $EDB8
        db      OP_CPDR, 2      ; $EDB9
        db      OP_INDR, 2      ; $EDBA
        db      OP_OTDR, 2      ; $EDBB
        db      OP_INV,  2      ; $EDBD
        db      OP_INV,  2      ; $EDBD
        db      OP_INV,  2      ; $EDBE
        db      OP_INV,  2      ; $EDBF
        db      OP_INV,  2      ; $EDC0
        db      OP_INV,  2      ; $EDC1
        db      OP_INV,  2      ; $EDC2
        db      OP_INV,  2      ; $EDC3
        db      OP_INV,  2      ; $EDC4
        db      OP_INV,  2      ; $EDC5
        db      OP_INV,  2      ; $EDC6
        db      OP_INV,  2      ; $EDC7
        db      OP_INV,  2      ; $EDC8
        db      OP_INV,  2      ; $EDC9
        db      OP_INV,  2      ; $EDCA
        db      OP_INV,  2      ; $EDCB
        db      OP_INV,  2      ; $EDCC
        db      OP_INV,  2      ; $EDCD
        db      OP_INV,  2      ; $EDCE
        db      OP_INV,  2      ; $EDCF
        db      OP_INV,  2      ; $EDD0
        db      OP_INV,  2      ; $EDD1
        db      OP_INV,  2      ; $EDD2
        db      OP_INV,  2      ; $EDD3
        db      OP_INV,  2      ; $EDD4
        db      OP_INV,  2      ; $EDD5
        db      OP_INV,  2      ; $EDD6
        db      OP_INV,  2      ; $EDD7
        db      OP_INV,  2      ; $EDD8
        db      OP_INV,  2      ; $EDD9
        db      OP_INV,  2      ; $EDDA
        db      OP_INV,  2      ; $EDDB
        db      OP_INV,  2      ; $EDDC
        db      OP_INV,  2      ; $EDDD
        db      OP_INV,  2      ; $EDDE
        db      OP_INV,  2      ; $EDDF
        db      OP_INV,  2      ; $EDE0
        db      OP_INV,  2      ; $EDE1
        db      OP_INV,  2      ; $EDE2
        db      OP_INV,  2      ; $EDE3
        db      OP_INV,  2      ; $EDE4
        db      OP_INV,  2      ; $EDE5
        db      OP_INV,  2      ; $EDE6
        db      OP_INV,  2      ; $EDE7
        db      OP_INV,  2      ; $EDE8
        db      OP_INV,  2      ; $EDE9
        db      OP_INV,  2      ; $EDEA
        db      OP_INV,  2      ; $EDEB
        db      OP_INV,  2      ; $EDEC
        db      OP_INV,  2      ; $EDED
        db      OP_INV,  2      ; $EDEE
        db      OP_INV,  2      ; $EDEF
        db      OP_INV,  2      ; $EDF0
        db      OP_INV,  2      ; $EDF1
        db      OP_INV,  2      ; $EDF2
        db      OP_INV,  2      ; $EDF3
        db      OP_INV,  2      ; $EDF4
        db      OP_INV,  2      ; $EDF5
        db      OP_INV,  2      ; $EDF6
        db      OP_INV,  2      ; $EDF7
        db      OP_INV,  2      ; $EDF8
        db      OP_INV,  2      ; $EDF9
        db      OP_INV,  2      ; $EDFA
        db      OP_INV,  2      ; $EDFB
        db      OP_INV,  2      ; $EDFC
        db      OP_INV,  2      ; $EDFD
        db      OP_INV,  2      ; $EDFE
        db      OP_INV,  2      ; $EDFF


; Disassemble:
; Disassemble instruction at address
; Print address, length bytes, opcode string
; Increment address by length
; TODO: Add support for multibyte instructions
; e.g.
; 0122 01           NOP
; 0123 ED 5F        LD
; 0125 32 09 FF     LD
; 0128 DD 22 0E FF  LD
; 012C FD 22 10 FF  LD
; 0130 F5           PUSH
; 0131 C1           POP
; 0132 79           LD
; 0133 32 01 FF     LD
; 0136 21 00 F0     LD
; 0139 7C           LD
; 013A 32 0A FF     LD


disass:
        ld      hl,(address)    ; Get address of instruction
        ld      b,0             ; Clear upper byte of BC
        ld      c,(hl)          ; Get the opcode, e.g. $09 = ADD
        ld      a,c             ; Put opcode in A so we can compare
        cp      0cbH            ; Is it an extended CB opcode?
        jr      z,extcb
        cp      0ddH            ; Is it an extended DD opcode?
        jr      z,extdd
        cp      0fdH            ; Is it an extended FD opcode?
        jr      z,extfd         ; Same table as DD
        cp      0edH            ; Is it an extended ED opcode?
        jr      z,exted
        jr      notext          ; Not an extended opcode

; TODO: Refactor to reduce code duplicated in three places below.

extcb:  inc     hl              ; Advance address to second byte of extended opcode
        ld      c,(hl)          ; Get second byte
        ld      h,b             ; HL=BC
        ld      l,c
        add     hl,hl           ; Multiply by 2 because 2 bytes per table entry
        ld      b,h             ; BC=HL
        ld      c,l
        ld      hl,CBOPCODES    ; Get start address of opcode table
        add     hl,bc           ; Add opcode*2
        ld      a,(hl)          ; Get the opcode constant, e.g. OP_ADD = $01
        ld      (opcode),a      ; Save it
        inc     hl              ; Advance to instruction length entry in table
        ld      a,(hl)          ; Get length
        ld      (len),a         ; Save it
        jr      getmnem

extdd:
extfd:  inc     hl              ; Advance address to second byte of extended opcode
        ld      c,(hl)          ; Get second byte
        ld      h,b             ; HL=BC
        ld      l,c
        add     hl,hl           ; Multiply by 2 because 2 bytes per table entry
        ld      b,h             ; BC=HL
        ld      c,l
        ld      hl,DDOPCODES    ; Get start address of opcode table
        add     hl,bc           ; Add opcode*2
        ld      a,(hl)          ; Get the opcode constant, e.g. OP_ADD = $01
        ld      (opcode),a      ; Save it
        inc     hl              ; Advance to instruction length entry in table
        ld      a,(hl)          ; Get length
        ld      (len),a         ; Save it
        jr      getmnem

exted:  inc     hl              ; Advance address to second byte of extended opcode
        ld      c,(hl)          ; Get second byte
        ld      h,b             ; HL=BC
        ld      l,c
        add     hl,hl           ; Multiply by 2 because 2 bytes per table entry
        ld      b,h             ; BC=HL
        ld      c,l
        ld      hl,EDOPCODES    ; Get start address of opcode table
        add     hl,bc           ; Add opcode*2
        ld      a,(hl)          ; Get the opcode constant, e.g. OP_ADD = $01
        ld      (opcode),a      ; Save it
        inc     hl              ; Advance to instruction length entry in table
        ld      a,(hl)          ; Get length
        ld      (len),a         ; Save it
        jr      getmnem

notext: ld      h,b             ; HL=BC
        ld      l,c
        add     hl,hl           ; Multiply by 2 because 2 bytes per table entry
        ld      b,h             ; BC=HL
        ld      c,l
        ld      hl,OPCODES      ; Get start address of opcode table
        add     hl,bc           ; Add opcode*2
        ld      a,(hl)          ; Get the opcode constant, e.g. OP_ADD = $01
        ld      (opcode),a      ; Save it
        inc     hl              ; Advance to instruction length entry in table
        ld      a,(hl)          ; Get length
        ld      (len),a         ; Save it
getmnem:
        ld      a,(opcode)      ; Get the mnemonic, e.g. $01 = OP_ADD
        ld      c,a             ; Put in C
        ld      b,0             ; Clear upper byte of BC
        ld      h,b             ; HL=BC
        ld      l,c
        add     hl,hl           ; Multiply by 4 because 4 bytes per table entry
        add     hl,hl
        ld      b,h             ; BC=HL
        ld      c,l
        ld      hl,MNEMONICS    ; Get start address of mnemonics table
        add     hl,bc           ; Add index to address of table
        ld      (mnemonic),hl   ; Save address of mnemonic string

        ld      hl,(address)    ; Print address
        call    PrintAddress
        call    PrintSpace      ; Print space

        ld      a,(len)         ; Get instruction length
        cp      1               ; Is it 1?
        jr      z,len1
        cp      2               ; Is it 2?
        jr      z,len2
        cp      3               ; Is it 3?
        jr      z,len3
        jr      len4            ; Otherwise it must be 4

len1:                           ; If length is 1, print "DD           " (11 spaces)
        ld      ix,(address)
        ld      a,(ix+0)        ; Get byte at address
        call    PrintByte
        ld      a,11
        call    PrintSpaces
        jr      mnem

len2:                           ; If length is 2, print "DD DD        " (8 spaces)
        ld      ix,(address)
        ld      a,(ix+0)        ; Get byte at address
        call    PrintByte
        call    PrintSpace
        ld      a,(ix+1)        ; Get byte at address+1
        call    PrintByte
        ld      a,8
        call    PrintSpaces
        jr      mnem

len3:                           ; if length is 3, print "DD DD DD     " (5 spaces)
        ld      ix,(address)
        ld      a,(ix+0)        ; Get byte at address
        call    PrintByte
        call    PrintSpace
        ld      a,(ix+1)        ; Get byte at address+1
        call    PrintByte
        call    PrintSpace
        ld      a,(ix+2)        ; Get byte at address+2
        call    PrintByte
        ld      a,5
        call    PrintSpaces
        jr      mnem

len4:                           ; if length is 4, print "DD DD DD DD  " (2 spaces)
        ld      ix,(address)
        ld      a,(ix+0)        ; Get byte at address
        call    PrintByte
        call    PrintSpace
        ld      a,(ix+1)        ; Get byte at address+1
        call    PrintByte
        call    PrintSpace
        ld      a,(ix+2)        ; Get byte at address+2
        call    PrintByte
        call    PrintSpace
        ld      a,(ix+3)        ; Get byte at address+3
        call    PrintByte
        ld      a,2
        call    PrintSpaces
        jr      mnem

mnem:                           ; Print 4 byte mnemonic string
        ld      ix,(mnemonic)
        ld      a,(ix+0)
        call    PrintChar
        ld      a,(ix+1)
        call    PrintChar
        ld      a,(ix+2)
        call    PrintChar
        ld      a,(ix+3)
        call    PrintChar
        call    PrintCR         ; Print CR

; Increment address by instruction length

        ld      a,(len)         ; Get the instruction length
        ld      c,a             ; Put in C
        ld      b,0             ; Clear upper byte of BC

        ld      hl,(address)    ; Get address
        add     hl,bc           ; Add length to address
        ld      (address),hl    ; Save new address

        ret                     ; Return

JMONEND

; Port of Palo Alto Tiny BASIC Version Three, by Li-Chen Wang.
;
; Source taken from "PCC's Reference Book of Personal and Home
; Computing". See that document for a description of the language and
; commands.
;
; Jeff Tranter <tranter@pobox.com> made the following changes:
;
; 1. Minor changes to correct some spelling and grammatical errors in
;    comments.
; 2. Adapted to build with the ASL assembler.
; 3. Ported to my Z80 SBC.
; 4. Use more standard statement separator ":" rather than ";".
; 5. Use more standard not equals operator "<>" rather than "#".
; 6. Made error messages longer/more descriptive.
; 7. Added PEEK(), USR(), and INP() functions and POKE and OUT
;    commands.

; Define SBC below to get version for my Z80 Single Board Computer.
; Comment it out to get original code from published article.

SBC     EQU     1

        CPU     Z80

;CR      EQU     0DH             ; CARRIAGE RETURN
LF      EQU     0AH             ; LINE FEED
BS      EQU     08H             ; BACKSPACE

        IFDEF SBC
; 6850 UART I/O registers
;SREG    PORT    80H
;CREG    PORT    80H
;DREG    PORT    81H
        ENDIF

; Macro for character testing used by parser.
; See comments in routine TSTCH.
; Source was not shown in original Tiny Basic article.

TSTC    MACRO   P1,P2
        CALL    TSTCH
        DB      P1
        DB      P2-$-1
        ENDM

; Macro for keyword tables. Accepts a routine address. Outputs address
; as high byte with bit 7 set followed by low byte.

ITEM    MACRO   P1
        DB      (P1>>8)|080H, P1&00FFH
        ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                     T B I
;            TINY BASIC INTERPRETER
;                 VERSION 3.0
;               FOR 8080 SYSTEM
;                 LI-CHEN WANG
;                26 APRIL, 1977
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  *** MEMORY USAGE ***
;
; ORIGINAL:
;  0080-01FF  ARE FOR VARIABLES, INPUT LINE, AND STACK
;  2000-3FFF  ARE FOR TINY BASIC TEXT & ARRAY
;  F000-F7FF  ARE FOR TBI CODE
;
; FOR Z80 SBC (ROM from 0000-1FFF, RAM 8000-FFFF):
;  8080-81FF  ARE FOR VARIABLES, INPUT LINE, AND STACK
;  A000-FFFF  ARE FOR TINY BASIC TEXT & ARRAY
;  0000-1FFF  ARE FOR TBI CODE

        IFDEF   SBC
BOTSCR  EQU     08080H
TOPSCR  EQU     08200H
BOTRAM  EQU     0A000H
DFTLMT  EQU     0FFFFH
BOTROM  EQU     JMONEND
    ELSE
BOTSCR  EQU     00080H
TOPSCR  EQU     00200H
BOTRAM  EQU     02000H
DFTLMT  EQU     04000H
BOTROM  EQU     0F000H
        ENDIF
;
;  DEFINE VARIABLES, BUFFER, AND STACK IN RAM

        ORG     BOTSCR
KEYWRD  DS      1               ; WAS INIT DONE?
TXTLMT  DS      2               ; ->LIMIT OF TEXT AREA
VARBGN  DS      2*26            ; TB VARIABLES A-Z
CURRNT  DS      2               ; POINTS TO CURRENT LINE
STKGOS  DS      2               ; SAVES SP IN 'GOSUB'
VARNXT                          ; TEMP STORAGE
STKINP  DS      2               ; SAVES SP IN 'INPUT'
LOPVAR  DS      2               ; 'FOR' LOOP SAVE AREA
LOPINC  DS      2               ; INCREMENT
LOPLMT  DS      2               ; LIMIT
LOPLN   DS      2               ; LINE NUMBER
LOPPT   DS      2               ; TEXT POINTER
RANPNT  DS      2               ; RANDOM NUMBER POINTER
        IFDEF   SBC
USER    DS      4               ; CALL INSTRUCTION FOLLOWED BY ADDRESS OF USR() FUNCTION AND RET
        ENDIF
        DS      1               ; EXTRA BYTE FOR BUFFER
BUFFER  DS      132             ; INPUT BUFFER
BUFEND                          ; BUFFER ENDS
        DS      4               ; EXTRA BYTES FOR STACK
STKLMT                          ; SOFT LIMIT FOR STACK
        ORG     TOPSCR
STACKB                          ; STACK STARTS HERE
        ORG     BOTRAM
TXTUNF  DS      2
TEXT    DS      2

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
        ORG     BOTROM

        IFDEF   SBC

; Start actual code at $0100

;        DB      (0100H-$) DUP 0FFH
        ENDIF
BASIC
INIT
        IFDEF   SBC
        DI                      ; Disable interrupts
        LD      A,016H          ; Initialize 6850 ACIA
        OUT     (CREG),A
        ENDIF

        LD      SP,STACKB
        CALL    CRLF
        LD      HL,KEYWRD       ; AT POWER ON KEYWRD IS
        LD      A,0C3H          ; PROBABLY NOT C3
        CP      (HL)
        JR      Z,TELL          ; IT IS C3, CONTINUE
        LD      (HL),A          ; NO, SET IT TO C3
        LD      HL,DFTLMT       ; AND SET DEFAULT VALUE
        LD      (TXTLMT),HL     ; IN 'TXTLMT'
        LD      A,BOTROM>>8     ; INITIALIZE RANPNT
        LD      (RANPNT+1),A
        IFDEF   SBC
        LD      A,0CDH          ; STORE CALL INSTRUCTION AT USR() ADDRESS
        LD      (USER),A
        LD      HL,RET          ; INITIALIZE DEFAULT USR() ADDRESS
        LD      (USER+1),HL
        LD      A,0C9H          ; STORE RET INSTRUCTION AFTER USR() FUNCTION
        LD      (USER+3),A
        ENDIF
PURGE   LD      HL,TEXT+4        ; PURGE TEXT AREA
        LD      (TXTUNF),HL
        LD      H,0FFH
        LD      (TEXT),HL
TELL    LD      DE,MSG          ; TELL USER
        CALL    PRTSTG          ; ***********************
        JR      RSTART          ; ***** JMP USER-INIT ***
MSG     DB      "TINY "         ; ***********************
        DB      "BASIC"
        DB      " V3.0",CR
OK      DB      "OK",CR
        IFNDEF  SBC
WHAT    DB      "WHAT?",CR
HOW     DB      "HOW?",CR
SORRY   DB      "SORRY",CR
        ENDIF
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** DIRECT COMMAND / TEXT COLLECTOR
;
; TBI PRINTS OUT "OK(CR)", AND THEN IT PROMPTS ">" AND READS A LINE.
; IF THE LINE STARTS WITH A NON-ZERO NUMBER, THIS NUMBER IS THE LINE
; NUMBER. THE LINE NUMBER (IN 16 BIT BINARY) AND THE REST OF THE LINE
; (INCLUDING THE CR) IS STORED IN THE MEMORY. IF A LINE WITH THE SAME
; LINE NUMBER IS ALREADY THERE, IT IS REPLACED BY THE NEW ONE. IF THE
; REST OF THE LINE CONSISTS OF A CR ONLY, IT IS NOT STORED AND ANY
; EXISTING LINE WITH THE SAME LINE NUMBER IS DELETED.
;
; AFTER A LINE IS INSERTED, REPLACED, OR DELETED, THE PROGRAM LOOPS
; BACK AND ASK FOR ANOTHER LINE. THIS LOOP WILL BE TERMINATED WHEN IT
; READS A LINE WITH ZERO OR NO LINE NUMBER; AND CONTROL IS TRANSFERRED
; TO "DIRECT".
;
; TINY BASIC PROGRAM SAVE AREA STARTS AT THE MEMORY LOCATION LABELLED
; "TEXT". THE END OF TEST IS MARKED BY 2 BYTES XX FF. FOLLOWING
; THESE ARE 2 BYTES RESERVED FOR THE ARRAY ELEMENT @(0). THE CONTENT
; OF LOCATION LABELLED "TXTUNF" POINTS TO ONE AFTER @(0).
;
; THE MEMORY LOCATION "CURRNT" POINTS TO THE LINE NUMBER THAT IS
; CURRENTLY BEING EXECUTED. WHILE WE ARE IN THIS LOOP OR WHILE WE
; ARE INTERPRETING A DIRECT COMMENT (SEE NEXT SECTION), "CURRNT"
; SHOULD POINT TO A 0.
;
RSTART  LD      SP,STACKB       ; RE-INITIALIZE STACK
        LD      HL,ST1+1        ; LITERAL 0
        LD      (CURRNT),HL     ; CURRNT->LINE # = 0
ST1     LD      HL,0
        LD      (LOPVAR),HL
        LD      (STKGOS),HL
        LD      DE,OK           ; DE->STRING
        CALL    PRTSTG          ; PRINT STRING UNTIL CR
ST2     LD      A,'>'           ; PROMPT '>' AND
        CALL    GETLN           ; READ A LINE
        PUSH    DE              ; DE->END OF LINE
        LD      DE,BUFFER       ; DE->BEGINNING OF LINE
        CALL    TSTNUM          ; TEST IF IT IS A NUMBER
        CALL    IGNBLK
        LD      A,H             ; HL=VALUE OF THE # OR
        OR      L               ; 0 IF NO # WAS FOUND
        POP     BC              ; BC->END OF LINE
        JR      Z,DIRECT
        DEC     DE              ; BACKUP DE AND SAVE
        LD      A,H             ; VALUE OF LINE # THERE
        LD      (DE),A
        DEC     DE
        LD      A,L
        LD      (DE),A
        PUSH    BC              ; BC,DE->BEGIN, END
        PUSH    DE
        LD      A,C
        SUB     E
        PUSH    AF              ; A=# OF BYTES IN LINE
        CALL    FNDLN           ; FIND THIS LINE IN SAVE
        PUSH    DE              ; AREA, DE->SAVE AREA
        JR      NZ,ST3          ; NZ:NOT FOUND. INSERT
        PUSH    DE              ; Z:FOUND. DELETE IT
        CALL    FNDNXT          ; SET DE->NEXT LINE
        POP     BC              ; BC->LINE TO BE DELETED
        LD      HL,(TXTUNF)     ; HL->UNFILLED SAVE AREA
        CALL    MVUP            ; MOVE UP TO DELETE
        LD      H,B             ; TXTUNF->UNFILLED AREA
        LD      L,C
        LD      (TXTUNF),HL     ; UPDATE
ST3     POP     BC              ; GET READY TO INSERT
        LD      HL,(TXTUNF)     ; BUT FIRST CHECK IF
        POP     AF              ; THE LENGTH OF NEW LINE
        PUSH    HL              ; IS 3 (LINE # AND CR)
        CP      3               ; THEN DO NOT INSERT
        JR      Z,RSTART        ; MUST CLEAR THE STACK
        ADD     A,L             ; COMPUTE NEW TXTUNF
        LD      E,A
        LD      A,0
        ADC     A,H
        LD      D,A             ; DE->NEW UNFILLED AREA
        LD      HL,(TXTLMT)     ; CHECK TO SEE IF THERE
        EX      DE,HL
        CALL    COMP            ; IS ENOUGH SPACE
        IFDEF   SBC
        JP      NC,Q_OM         ; OUT OF MEMORY ERROR
        ELSE
        JR      NC,QSORRY       ; SORRY, NO ROOM FOR IT
        ENDIF
        LD      (TXTUNF),HL     ; OK, UPDATE TXTUNF
        POP     DE              ; DE->OLD UNFILLED AREA
        CALL    MVDOWN
        POP     DE              ; DE->BEGIN, HL->END
        POP     HL
        CALL    MVUP            ; MOVE NEW LINE TO SAVE
        JR      ST2             ; AREA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** DIRECT *** & EXEC
;
; THIS SECTION OF THE CODE TESTS A STRING AGAINST A TABLE. WHEN A
; MATCH IS FOUND, CONTROL IS TRANSFERRED TO THE SECTION OF CODE
; ACCORDING TO THE TABLE.
;
; AT 'EXEC', DE SHOULD POINT TO THE STRING AND HL SHOULD POINT TO THE
; TABLE-1. AT 'DIRECT', DE SHOULD POINT TO THE STRING, HL WILL BE SET
; UP TO POINT TO TAB1-1, WHICH IS THE TABLE OF ALL DIRECT AND
; STATEMENT COMMANDS.
;
; A '.' IN THE STRING WILL TERMINATE THE TEST AND THE PARTIAL MATCH
; WILL BE CONSIDERED AS A MATCH, E.G. 'P.', 'PR.', PRI.', 'PRIN.',
; OR 'PRINT' WILL ALL MATCH 'PRINT'.
;
; THE TABLE CONSISTS OF ANY NUMBER OF ITEMS. EACH ITEM IS A STRING OF
; CHARACTERS WITH BIT 7 SET TO 0 AND A JUMP ADDRESS STORED HI-LOW WITH
; BIT 7 OF THE HIGH BIT SET TO 1.
;
; END OF TABLE IS AN ITEM WITH A JUMP ADDRESS ONLY. IF THE STRING
; DOES NOT MATCH ANY OF THE OTHER ITEMS, IT WILL MATCH THIS NULL ITEM
; AS DEFAULT.
;
DIRECT  LD      HL,TAB1-1       ; *** DIRECT ***
;
EXEC    CALL    IGNBLK          ; *** EXEC ***
        PUSH    DE              ; SAVE POINTER
EX1     LD      A,(DE)          ; IF FOUND '.' IN STRING
        INC     DE              ; BEFORE ANY MISMATCH
        CP      '.'             ; WE DECLARE A MATCH
        JR      Z,EX3
        INC     HL              ; HL->TABLE
        CP      (HL)            ; IF MATCH, TEST NEXT
        JR      Z,EX1
        LD      A,07FH          ; ELSE, SEE IF BIT 7
        DEC     DE              ; OF TABLE IS SET, WHICH
        CP      (HL)            ; IS THE JUMP ADDR, (HI)
        JR      C,EX5           ; C:YES, MATCHED
EX2     INC     HL              ; NC:NO, FIND JUMP ADDR.
        CP      (HL)
        JR      NC,EX2
        INC     HL              ; BUMP TO NEXT TAB. ITEM
        POP     DE              ; RESTORE STRING POINTER
        JR      EXEC            ; TEST AGAINST NEXT ITEM
EX3     LD      A,7FH           ; PARTIAL MATCH, FIND
EX4     INC     HL              ; JUMP ADDR., WHICH IS
        CP      (HL)            ; FLAGGED BY BIT 7
        JR      NC,EX4
EX5     LD      A,(hl)          ; LOAD HL WITH THE JUMP
        INC     HL              ; ADDRESS FROM THE TABLE
        LD      L,(HL)          ; ****************
        IFDEF   SBC
        AND     07FH            ; CLEAR HIGH BIT TO GET REAL JUMP ADDRESS
        ELSE
        AND     0FFH
        ENDIF
        LD      H,A
        POP     AF              ; CLEAN UP THE GARBAGE
        JP      (HL)            ; AND WE GO DO IT
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; WHAT FOLLOWS IS THE CODE TO EXECUTE DIRECT AND STATEMENT COMMANDS.
; CONTROL IS TRANSFERRED TO THESE POINTS VIA THE COMMAND TABLE LOOKUP
; CODE OF 'DIRECT' AND 'EXEC' IN THE LAST SECTION. AFTER THE COMMAND IS
; EXECUTED, CONTROL IS TRANSFERRED TO OTHER SECTIONS AS FOLLOWS:
;
; FOR 'LIST', 'NEW', AND 'STOP': GO BACK TO 'RSTART'.
; FOR 'RUN': GO EXECUTE THE FIRST STORED LINE IF ANY; ELSE GO BACK TO
; 'RSTART'.
; FOR 'GOTO' AND 'GOSUB': GO EXECUTE THE TARGET LINE.
; FOR 'RETURN' AND 'NEXT': GOT BACK TO SAVED RETURN LINE.
; FOR ALL OTHERS: IF 'CURRNT'->0, GO TO 'RSTART', ELSE GO EXECUTE
; NEXT COMMAND. (THIS IS DONE IN 'FINISH'.)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** NEW *** STOP *** RUN (& FRIENDS) *** & GOTO
;
; 'NEW(CR)' RESETS 'TXTUNF'
;
; 'STOP(CR)' GOES BACK TO 'RSTART'
;
; 'RUN(CR)' FINDS THE FIRST STORED LINE, STORE ITS ADDRESS (IN
; 'CURRNT'), AND STARTS TO EXECUTE IT. NOTE THAT ONLY THOSE
; COMMANDS IN TAB2 ARE LEGAL FOR STORED PROGRAMS.
;
; THERE ARE 3 MORE ENTRIES IN 'RUN':
; 'RUNNXL' FINDS NEXT LINE, STORES ITS ADDR, AND EXECUTES IT.
; 'RUNTSL' STORES THE ADDRESS OF THIS LINE AND EXECUTES IT.
; 'RUNSML' CONTINUES EXECUTION ON THE SAME LINE.
;
; 'GOTO EXPR(CR)' EVALUATES THE EXPRESSION, FINDS THE TARGET
; LINE, AND JUMPS TO 'RUNTSL' TO DO IT.
;
NEW     CALL    ENDCHK          ; *** NEW(CR) ***
        JP      PURGE
;
STOP    CALL    ENDCHK          ; *** STOP(CR) ***
        JP      RSTART
;
RUN     CALL    ENDCHK          ; *** RUN (CR) ***
        LD      DE,TEXT         ; FIRST SAVED LINE
;
RUNNXL  LD      HL,0            ; *** RUNNXL ***
        CALL    FNDLP           ; FIND WHATEVER LINE #
        JP      C,RSTART        ; C:PASSED TXTUNF, QUIT
;
RUNTSL  EX      DE,HL           ; *** RUNTSL ***
        LD      (CURRNT),HL     ; SET 'CURRNT'->LINE #
        EX      DE,HL
        INC     DE               ; BUMP PASS LINE #
        INC     DE
;
RUNSML  CALL    CHKIO           ; *** RUNSML ***
        LD      HL,TAB2-1       ; FIND COMMAND IN TAB2
        JR      EXEC            ; AND EXECUTE IT
;
GOTO    CALL    EXPR            ; *** GOTO EXPR ***
        PUSH    DE              ; SAVE FOR ERROR ROUTINE
        CALL    ENDCHK          ; MUST FIND A CR
        CALL    FNDLN           ; FIND THE TARGET LINE #
        IFDEF   SBC
        JP      NZ,A_LF         ; LINE NOT FOUND ERROR
        ELSE
        JR      NZ,AHOW         ; NO SUCH LINE NUMBER
        ENDIF
        POP     AF              ; CLEAR THE "PUSH DE"
        JR      RUNTSL
        IFDEF   SBC
POKE                            ; *** POKE addr,data ***
        CALL    EXPR            ; GET FIRST PARAMETER (ADDRESS)
        PUSH    HL              ; SAVE IT
        TSTC    ',',BAD         ; SHOULD BE FOLLOWED BY A COMMA
        CALL    EXPR            ; GET SECOND PARAMETER (DATA)
        LD      A,H             ; GET MSB OF DATA
        OR      A               ; MUST BE ZERO
        JP      NZ,Q_IA         ; OTHERWISE INVALID ARGUMENT ERROR
        LD      A,L             ; GET LSB OF DATA TO POKE
        POP     HL              ; GET SAVED ADDRESS
        LD      (HL),A          ; WRITE BYTE TO ADDRESS
        JP      FINISH          ; DONE
BAD:    POP     HL              ; CLEAN UP STACK
        JP      Q_SN            ; SYNTAX ERROR
;
OUTP                            ; *** OUT port,data ***
        CALL    EXPR            ; GET FIRST PARAMETER (PORT)
        LD      A,H             ; GET MSB OF PORT
        OR      A               ; MUST BE ZERO
        JP      NZ,Q_IA         ; OTHERWISE INVALID ARGUMENT ERROR
        PUSH    HL              ; SAVE IT
        TSTC    ',',BAD         ; SHOULD BE FOLLOWED BY A COMMA
        CALL    EXPR            ; GET SECOND PARAMETER (DATA)
        LD      A,H             ; GET MSB OF DATA
        OR      A               ; MUST BE ZERO
        JP      NZ,Q_IA         ; OTHERWISE INVALID ARGUMENT ERROR
        LD      A,L             ; GET LSB OF DATA TO POKE
        POP     HL              ; GET SAVED PORT NUMBER
        LD      C,L             ; GET PORT NUMBER
; Use a Z80 instruction to write to the port. Otherwise, with 8080
; instructions we could only write to a dynamic port number by
; using self modifying code running out of RAM.
        OUT    (C),A
        JP      FINISH          ; DONE
        ENDIF
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** LIST ** & PRINT ***
;
; LIST HAS THREE FORMS:
; 'LIST(CR)' LISTS ALL SAVED LINES
; 'LIST N(CR)' STARTS LISTING AT LINE N
; 'LIST N1,N2(CR)' STARTS LISTING AT LINE N1 FOR N2 LINES. YOU CAN STOP
; THE LISTING BY CONTROL C KEY.
;
; PRINT COMMAND IS 'PRINT ....:' OR 'PRINT ....(CR)'
; WHERE '....' IS A LIST OF EXPRESSIONS, FORMATS, AND/OR STRINGS.
; THESE ITEMS ARE SEPARATED BY COMMAS.
;
; A FORMAT IS A POUND SIGN FOLLOWED BY A NUMBER. IT CONTROLS THE
; NUMBER OF SPACES THE VALUE OF AN EXPRESSION IS GOING TO BE PRINTED.
; IT STAYS EFFECTIVE FOR THE REST OF THE PRINT COMMAND UNLESS CHANGED
; BY ANOTHER FORMAT. IF NOT FORMAT IS SPECIFIED,  POSITIONS WILL BE
; USED.
;
; A STRING IS QUOTED IN A PAIR OF SINGLE QUOTES OR A PAIR OF DOUBLE
; QUOTES.
;
; CONTROL CHARACTERS AND LOWER CASE LETTERS CAN BE INCLUDED INSIDE THE
; QUOTES. ANOTHER (BETTER) WAY OF GENERATING CONTROL CHARACTERS IN
; THE OUTPUT IS TO USE THE UP-ARROW CHARACTER FOLLOWED BY A LETTER. ^L
; MEANS FF, ^I MEANS HT, ^G MEANS BELL ETC.
;
; A (CRLF) IS GENERATED AFTER THE ENTIRE LIST HAS BEEN PRINTED OR IF
; THE LIST IS A NULL LIST. HOWEVER, IF THE LIST ENDS WITH COMMA, NO
; (CRLF) IS GENERATED.
;
LIST    CALL    TSTNUM          ; TEST IF THERE IS A #
        PUSH    HL
        LD      HL,0FFFFH
        TSTC    ',',LS1
        CALL    TSTNUM
LS1     EX      (SP),HL
        CALL    ENDCHK          ; IF NO # WE GET A 0
        CALL    FNDLN           ; FINDS THIS OR NEXT LINE
LS2     JP      C,RSTART        ; C:PASSED TXTUNF
        EX      (SP),HL
        LD      A,H
        OR      L
        JP      Z,RSTART
        DEC     HL
        EX      (SP),HL
        CALL    PRTLN           ; PRINT THE LINE
        CALL    PRTSTG
        CALL    CHKIO
        CALL    FNDLP           ; FIND NEXT LINE
        JR      LS2             ; AND LOOP BACK
;
PRINT   LD      C,8             ; C= # OF SPACES
        TSTC    ';',PR1         ; IF NULL LIST & ";"
        CALL    CRLF            ; GIVE CR-LF AND
        JP      RUNSML          ; CONTINUE SAME LINE
PR1     TSTC    CR,PR6          ; IF NULL LIST (CR)
        CALL    CRLF            ; ALSO GIVE CR-LF AND
        JP      RUNNXL          ; GO TO NEXT LINE
PR2     TSTC    '#',PR4         ; ELSE IS IT FORMAT?
PR3     CALL    EXPR            ; YES, EVALUATE EXPR.
        LD      A,0C0H
        AND     L
        OR      H
        IFDEF   SBC
        JP      NZ,Q_SN         ; SYNTAX ERROR
        ELSE
        JR      NZ,QHOW
        ENDIF
        LD      C,L             ; AND SAVE IT IN C
        JR      PR5             ; LOOK FOR MORE TO PRINT
PR4     CALL    QTSTG           ; OR IS IT A STRING?
        JR      PR9             ; IF NOT, MUST BE EXPR.
PR5     TSTC    ',',PR8         ; IF ",", GO FIND NEXT
PR6     TSTC    '.',PR7
        LD      A,' '
        CALL    OUTCH
        JR      PR6
PR7     CALL    FIN             ; IN THE LIST
        JR      PR2             ; LIST CONTINUES
PR8     CALL    CRLF            ; LIST ENDS
        JP      FINISH
PR9     CALL    EXPR            ; EVALUATE THE EXPR
        PUSH    BC
        CALL    PRTNUM          ; PRINT THE VALUE
        POP     BC
        JR      PR5             ; MORE TO PRINT?
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** GOSUB *** & RETURN ***
;
; 'GOSUB EXPR:' OR 'GOSUB EXPR (CR)" IS LIKE THE 'GOTO' COMMAND,
; EXCEPT THAT THE CURRENT TEXT POINTER, STACK POINTER ETC, ARE SAVED SO
; THAT EXECUTION CAN BE CONTINUED AFTER THE SUBROUTINE 'RETURN'. IN
; ORDER THAT 'GOSUB' CAN BE NESTED (AND EVEN RECURSIVE), THE SAVE AREA
; 'STKGOS' IS SAVED ON THE STACK. IF WE ARE IN THE MAIN ROUTINE,
; 'STKGOS' IS ZERO (THIS WAS DONE BY THE "MAIN" SECTION OF THE CODE),
; BUT WE STILL SAVE IT AS A FLAG FOR NO FURTHER 'RETURN'S.
;
; 'RETURN(CR)' UNDOES EVERYTHING THAT 'GOSUB' DID, AND THUS RETURNS
; EXECUTION TO THE COMMAND AFTER THE MOST RECENT 'GOSUB'. IF 'STKGOS'
; IS ZERO, IT INDICATES THAT WE NEVER HAD A 'GOSUB' AND IS THUS AN
; ERROR.
;
GOSUB   CALL    PUSHA           ; SAVE THE CURRENT "FOR"
        CALL    EXPR            ; PARAMETERS
        PUSH    DE              ; AND TEXT POINTER
        CALL    FNDLN           ; FIND THE TARGET LINE
        IFDEF   SBC
        JP      NZ,A_LF         ; LINE NOT FOUND ERROR
        ELSE
        JR      NZ,AHOW         ; NOT THERE. SAY "HOW?"
        ENDIF
        LD      HL,(CURRNT)     ; SAVE OLD
        PUSH    HL              ; 'CURRNT' OLD 'STKGCS'
        LD      HL,(STKGOS)
        PUSH    HL
        LD      HL,0            ; AND LOAD NEW ONES
        LD      (LOPVAR),HL
        ADD     HL,SP
        LD      (STKGOS),HL
        JP      RUNTSL          ; THEN RUN THAT LINE
RETURN  CALL    ENDCHK          ; THERE MUST BE A CR
        LD      HL,(STKGOS)     ; OLD STACK POINTER
        LD      A,H             ; 0 MEANS NOT EXIST
        OR      L
        IFDEF   SBC
        JP      Z,Q_LF          ; LINE NOT FOUND ERROR
        ELSE
        JP      Z,QWHAT         ; SO WE SAY "WHAT?"
        ENDIF
        LD      SP,HL           ; ELSE, RESTORE IT
RESTOR  POP     HL
        LD      (STKGOS),HL     ; AND THE OLD 'STKGOS'
        POP     HL
        LD      (CURRNT),HL     ; AND THE OLD 'CURRNT'
        POP     DE              ; OLD TEXT POINTER
        CALL    POPA            ; OLD "FOR" PARAMETERS
        JP      FINISH
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** FOR *** & NEXT ***
;
; 'FOR' HAS TWO FORMS: 'FOR VAR=EXP1 TO EXP2 STEP EXP3' AND 'FOR
; VAR=EXP1 TO EXP2'. THE SECOND FORM MEANS THE SAME THING AS THE FIRST
; FORK WITH EXP3=1 (I.E., WITH A STEP OF +1). TBI WILL FIND THE
; VARIABLE VAR. AND SET ITS VALUE TO THE CURRENT VALUE OF EXP1. IT
; ALSO EVALUATES EXP2 AND EXP3 AND SAVES ALL THESE TOGETHER WITH THE
; TEXT POINTER ETC. IN THE 'FOR' SAVE AREA, WHICH CONSISTS OF
; 'LOPVAR', 'LOPINC', 'LOPLMT', 'LOPLN', AND 'LOPPT'. IF THERE IS
; ALREADY SOMETHING IN THE SAVE AREA (THIS IS INDICATED BY A
; NON-ZERO 'LOPVAR'), THEN THE OLD SAVE AREA IS SAVED ON THE STACK
; BEFORE THE NEW ONE OVERWRITES IT. TBI WILL THEN DIG IN THE STACK
; AND FIND OUT IF THIS SAME VARIABLE WAS USED IN ANOTHER CURRENTLY
; ACTIVE 'FOR' LOOP. IF THAT IS THE CASE, THEN THE OLD 'FOR' LOOP IS
; DEACTIVATED (PURGED FROM THE STACK).
;
; 'NEXT VAR' SERVES AS THE LOGICAL (NOT NECESSARILY PHYSICAL) END OF
; THE 'FOR' LOOP. THE CONTROL VARIABLE VAR. IS CHECKED WITH THE
; 'LOPVAR'. IF THEY ARE NOT THE SAME, TBI DIGS IN THE STACK TO FIND
; THE RIGHT ONE AND PURGES ALL THOSE THAT DID NOT MATCH. EITHER WAY,
; TBI THEN ADDS THE 'STEP' TO THAT VARIABLE AND CHECKS THE RESULT WITH
; THE LIMIT. IF IT IS WITHIN THE LIMIT, CONTROL LOOPS BACK TO THE
; COMMAND FOLLOWING THE 'FOR'. IF OUTSIDE THE LIMIT, THE SAVE AREA IS
; PURGED AND EXECUTION CONTINUES.
;
FOR     CALL    PUSHA           ; SAVE THE OLD SAVE AREA
        CALL    SETVAL          ; SET THE CONTROL VAR
        DEC     HL              ; HL IS ITS ADDRESS
        LD      (LOPVAR),HL     ; SAVE THAT
        LD      HL,TAB4-1       ; USE 'EXEC' TO LOOK
        JP      EXEC            ; FOR THE WORD 'TO'
FR1     CALL    EXPR            ; EVALUATE THE LIMIT
        LD      (LOPLMT),HL     ; SAVE THAT
        LD      HL,TAB5-1       ; USE 'EXEC' TO LOOK
        JP      EXEC            ; FOR THE WORD 'STEP'
FR2     CALL    EXPR            ; FOUND IT, GET STEP
        JR      FR4
FR3     LD      HL,1            ; NOT FOUND, SET TO 1
FR4     LD      (LOPINC),HL     ; SAVE THAT TOO
        LD      HL,(CURRNT)     ; SAVE CURRENT LINE #
        LD      (LOPLN),HL
        EX      DE,HL           ; AND TEXT POINTER
        LD      (LOPPT),HL
        LD      BC,10           ; DIG INTO STACK TO
        LD      HL,(LOPVAR)     ; FIND 'LOPVAR'
        EX      DE,HL
        LD      H,B
        LD      L,B             ; HL=0 NOW
        ADD     HL,SP           ; HERE IS THE STACK
        JR      FR6
FR5     ADD     HL,BC           ; EACH LEVEL IS 10 DEEP
FR6     LD      A,(HL)          ; GET THAT OLD 'LOPVAR'
        INC     HL
        OR      (HL)
        JR      Z,FR7           ; 0 SAYS NO MORE IN IT
        LD      A,(HL)
        DEC     HL
        CP      D               ; SAME AS THIS ONE?
        JR      NZ,FR5
        LD      A,(HL)          ; THE OTHER HALF?
        CP      E
        JR      NZ,FR5
        EX      DE,HL           ; YES, FOUND ONE
        LD      HL,0
        ADD     HL,SP           ; TRY TO MOVE SP
        LD      B,H
        LD      C,L
        LD      HL,10
        ADD     HL,DE
        CALL    MVDOWN          ; AND PURGE 10 WORDS
        LD      SP,HL           ; IN THE STACK
FR7     LD      HL,(LOPPT)      ; JOB DONE, RESTORE DE
        EX      DE,HL
        JP      FINISH          ; AND CONTINUE
;
NEXT    CALL    TSTV            ; GET ACCESS OF VAR.
        IFDEF   SBC
        JP      C,Q_NV          ; NO SUCH VARIABLE ERROR
        ELSE
        JP      C,QWHAT         ; NO VARIABLE, "WHAT?"
        ENDIF
        LD      (VARNXT),HL     ; YES, SAVE IT
NX1     PUSH    DE              ; SAVE TEXT POINTER
        EX      DE,HL
        LD      HL,(LOPVAR)     ; GET VAR. IN 'FOR'
        LD      A,H
        OR      L               ; 0 SAYS NEVER HAD ONE
        IFDEF   SBC
        JP      Z,A_NV          ; NO SUCH VARIABLE ERROR
        ELSE
        JP      Z,AWHAT         ; SO WE ASK: "WHAT?"
        ENDIF
        CALL    COMP            ; ELSE WE CHECK THEM
        JR      Z,NX2           ; OK, THEY AGREE
        POP     DE              ; NO, LET'S SEE
        CALL    POPA            ; PURGE CURRENT LOOP
        LD      HL,(VARNXT)     ; AND POP ONE LEVEL
        JR      NX1             ; GO CHECK AGAIN
NX2     LD      E,(HL)          ; COME HERE WHEN AGREED
        INC     HL
        LD      D,(HL)          ; DE=VALUE OF VAR.
        LD      HL,(LOPINC)
        PUSH    HL
        LD      A,H
        XOR     D               ; S=SIGN DIFFER
        LD      A,D             ; A=SIGN OF DE
        ADD     HL,DE           ; ADD ONE STEP
        JP      M,NX3           ; CANNOT OVERFLOW
        XOR     H               ; MAY OVERFLOW
        JP      M,NX5           ; AND IT DID
NX3     EX      DE,HL
        LD      HL,(LOPVAR)     ; PUT IT BACK
        LD      (HL),E
        INC     HL
        LD      (HL),D
        LD      HL,(LOPLMT)     ; HL=LIMIT
        POP     AF              ; OLD HL
        OR      A
        JP      P,NX4           ; STEP > 0
        EX      DE,HL           ; STEP < 0
NX4     CALL    CKHLDE          ; COMPARE WITH LIMIT
        POP     DE              ; RESTORE TEXT POINTER
        JR      C,NX6           ; OUTSIDE LIMIT
        LD      HL,(LOPLN)      ; WITHIN LIMIT, GO
        LD      (CURRNT),HL     ; BACK TO THE SAVED
        LD      HL,(LOPPT)      ; 'CURRNT' AND TEXT
        EX      DE,HL           ; POINTER
        JP      FINISH
NX5     POP     HL              ; OVERFLOW, PURGE
        POP     DE              ; GARBAGE IN STACK
NX6     CALL    POPA            ; PURGE THIS LOOP
        JP      FINISH
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** REM *** IF *** INPUT *** & LET (& DEFLT) ***
;
; 'REM' FOLLOWED BY ANYTHING IS IGNORED BY TBI. TBI TREATS
; IT LIKE AN 'IF' WITH A FALSE CONDITION.
;
; 'IF' FOLLOWED BY AN EXPR. AS A CONDITION AND ONE OR MORE COMMANDS
; (INCLUDING OTHER 'IF'S) SEPARATE BY SEMI-COLONS. NOTE THAT THE
; WORD 'THEN' IS NOT USED. TBI EVALUATES THE EXPR. IF IT IS NON-ZERO,
; EXECUTION CONTINUES. IF THE EXPR. IS ZERO, THE COMMANDS THAT
; FOLLOW ARE IGNORED AND EXECUTION CONTINUES AT THE NEXT LINE.
;
; 'INPUT' COMMAND IS LIKE THE 'PRINT' COMMAND, AND IS FOLLOWED BY A
; LIST OF ITEMS. IF THE ITEM IS A STRING IN SINGLE OR DOUBLE QUOTES,
; OR IS AN UP-ARROW, IT HAS THE SAME EFFECT AS IN 'PRINT'. IF AN ITEM
; IS A VARIABLE, THIS VARIABLE NAME IS PRINTED OUT FOLLOWED BY A
; COLON. THEN TBI WAITS FOR AN EXPR. TO BE TYPED IN. THE VARIABLE IS
; THEN SET TO THE VALUE OF THIS EXPR. IF THE VARIABLE IS PRECEDED BY
; A STRING (AGAIN IN SINGLE OR DOUBLE QUOTES), THE STRING WILL BE
; PRINTED FOLLOWED BY A COLON. TBI THEN WAITS FOR INPUT EXPR. AND
; SETS THE VARIABLE TO THE VALUE OF THE EXPR.
;
; IF THE INPUT EXPR. IS INVALID, TBI WILL PRINT "WHAT?", "HOW?" OR
; "SORRY" AND REPRINT THE PROMPT AND REDO THE INPUT. THE EXECUTION
; WILL NOT TERMINATE UNLESS YOU TYPE CONTROL-C. THIS IS HANDLED IN
;'INPERR'.
;
; 'LET' IS FOLLOWED BY A LIST OF ITEMS SEPARATED BY COMMAS. EACH ITEM
; CONSISTS OF A VARIABLE, AN EQUAL SIGN, AND AN EXPR. TBI EVALUATES
; THE EXPR. AND SETS THE VARIABLE TO THAT VALUE. TBI WILL ALSO HANDLE
; A 'LET' COMMAND WITHOUT THE WORD 'LET'. THIS IS DONE BY 'DEFLT'.
;
REM     LD      HL,0            ; *** REM ***
        JR      IF1             ; THIS IS LIKE 'IF 0'
;
IFF     CALL    EXPR            ; *** IF ***
IF1     LD      A,H             ; IS THE EXPR.=0?
        OR      L
        JP      NZ,RUNSML       ; NO, CONTINUE
        CALL    FNDSKP          ; YES, SKIP REST OF LINE
        JP      NC,RUNTSL       ; AND RUN THE NEXT LINE
        JP      RSTART          ; IF NC NEXT, RE-START
;
INPERR  LD      HL,(STKINP)     ; *** INPERR ***
        LD      SP,HL           ; RESTORE OLD SP
        POP     HL              ; AND OLD 'CURRNT'
        LD      (CURRNT),HL
        POP     DE              ; AND OLD TEXT POINTER
        POP     DE              ; REDO INPUT
;
INPUT
IP1     PUSH    DE              ; SAVE IN CASE OF ERROR
        CALL    QTSTG           ; IS NEXT ITEM A STRING?
        JR      IP8             ; NO
IP2     CALL    TSTV            ; YES. BUT FOLLOWED BY A
        JR      C,IP5           ; VARIABLE? NO.
IP3     CALL    IP12
        LD      DE,BUFFER       ; POINTS TO BUFFER
        CALL    EXPR            ; EVALUATE INPUT
        CALL    ENDCHK
        POP     DE              ; OK, GET OLD HL
        EX      DE,HL
        LD      (HL),E          ; SAVE VALUE IN VAR.
        INC     HL
        LD      (HL),D
IP4     POP     HL              ; GET OLD 'CURRNT'
        LD      (CURRNT),HL
        POP     DE              ; AND OLD TEST POINTER
IP5     POP     AF              ; PURGE JUNK IN STACK
IP6     TSTC    ',',IP7         ; IS NEXT CH. ","?
        JR      INPUT           ; YES, MORE ITEMS.
IP7     JP      FINISH
IP8     PUSH    DE               ; SAVE FOR 'PRTSTG'
        CALL    TSTV            ; MUST BE VARIABLE NOW
        JR      NC,IP11
        IFDEF   SBC
        JP      Q_SN            ; SYNTAX ERROR
        ELSE
        JR      QWHAT           ; "WHAT?" IT IS NOT?
        ENDIF
IP11    LD      B,E
        POP     DE
        CALL    PRTCHS          ; PRINT THOSE AS PROMPT
        JR      IP3             ; YES.INPUT VARIABLE
IP12    POP     BC              ; RETURN ADDRESS
        PUSH    DE              ; SAVE TEXT POINTER
        EX      DE,HL
        LD      HL,(CURRNT)     ; ALSO SAVE CURRNT
        PUSH    HL
        LD      HL,IP1          ; A NEGATIVE NUMBER
        LD      (CURRNT),HL     ; AS A FLAG
        LD      HL,0            ; SAVE SP TOO
        ADD     HL,SP
        LD      (STKINP),HL
        PUSH    DE              ; OLD HL
        LD      A,' '           ; PRINT A SPACE
        PUSH    BC
        JP      GETLN           ; AND GET A LINE
;
DEFLT   LD      A,(DE)          ; *** DEFLT ***
        CP      CR              ; EMPTY LINE IS OK
        JR      Z,LT4           ; ELSE IT IS 'LET'
;
LET                             ; *** LET ***
LT2     CALL    SETVAL
LT3     TSTC    ',',LT4         ; SET VALUE TO VAR.
        JR      LET             ; ITEM BY ITEM
LT4     JP      FINISH          ; UNTIL FINISH
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** EXPR ***
;
; 'EXPR' EVALUATES ARITHMETICAL OR LOGICAL EXPRESSIONS.
; <EXPR>::=<EXPR1>
;          <EXPR1><REL.OP.><EXPR1>
; WHERE >RE.OP.> IS ONE OF THE OPERATORS IN TAB6 AND THE RESULT OF
; THESE OPERATIONS IS 1 IF TRUE AND 0 IF FALSE.
; <EXPR1>::=(+ OR -)<EXPR2>(+ OR -<EXPR2>)(....)
; WHERE () ARE OPTIONAL AND (....) ARE OPTIONAL REPEATS.
; <EXPR2>::=<EXPR3>(< OR /><EXPR3>)(....)
; <EXPR3>::=<VARIABLE>
;           <FUNCTION>
;           (<EXPR>)
; <EXPR> IS RECURSIVE SO THAT VARIABLE "@" CAN HAVE AN <EXPR> AS
; <EXP3) CAN BE AN <EXPR> IN PARENTHESES.
;
EXPR    CALL    EXPR1           ; *** EXPR ***
        PUSH    HL              ; SAVE <EXPR1> VALUE
        LD      HL,TAB6-1       ; LOOK UP REL.OP.
        JP      EXEC            ; GO DO IT
XPR1    CALL    XPR8            ; REL.OP.">="
        RET     C               ; NO, RETURN HL=0
        LD      L,A             ; YES, RETURN HL=1
        RET
XPR2    CALL    XPR8            ; REL.OP."#" or "<>"
        RET     Z               ; FALSE, RETURN HL=0
        LD      L,A             ; TRUE, RETURN HL=1
        RET
XPR3    CALL    XPR8            ; REL.OP.">"
        RET     Z               ; FALSE
        RET     C               ; ALSO FALSE, HL=0
        LD      L,A             ; TRUE, HL=1
        RET
XPR4    CALL    XPR8            ; REL.OP."<="
        LD      L,A             ; SET HL=1
        RET     Z               ; REL. TRUE. RETURN
        RET     C
        LD      L,H             ; ELSE SET HL=0
        RET
XPR5    CALL    XPR8            ; RE.OP."="
        RET     NZ              ; FALSE, RETURN HL=0
        LD      L,A             ; ELSE SET HL=1
        RET
XPR6    CALL    XPR8            ; REL.OP."<"
        RET     NC              ; FALSE, RETURN HL=0
        LD      L,A             ; ELSE SET HL=1
        RET
XPR7    POP     HL              ; NOT REL.OP.
        RET                     ; RETURN HL=<EXPR1>
XPR8    LD      A,C             ; SUBROUTINE FOR ALL
        POP     HL              ; RE.OP.'S
        POP     BC
        PUSH    HL              ; REVERSE TOP OF STACK
        PUSH    BC
        LD      C,A
        CALL    EXPR1           ; GET 2ND <EXPR1>
        EX      DE,HL           ; VALUE IN DE NOW
        EX      (SP),HL         ; 1ST <EXPR1> IN HL
        CALL    CKHLDE          ; COMPARE 1ST WITH 2ND
        POP     DE              ; RESTORE TEXT POINTER
        LD      HL,0            ; SET HL=0, A=1
        LD      A,1
        RET
;
EXPR1   TSTC    '-',XP11        ; NEGATIVE SIGN?
        LD      HL,0            ; YES, FAKE '0-'
        JR      XP16            ; TREAT LIKE SUBTRACT
XP11    TSTC    '+',XP12        ; POSITIVE SIGN? IGNORE
XP12    CALL    EXPR2           ; 1ST <EXPR2>
XP13    TSTC    '+',XP15        ; ADD?
        PUSH    HL              ; YES, SAVE VALUE
        CALL    EXPR2           ; GET 2ND <EXPR2>
XP14    EX      DE,HL           ; 2ND IN DE
        EX      (SP),HL         ; 1ST IN HL
        LD      A,H             ; COMPARE SIGN
        XOR     D
        LD      A,D
        ADD     HL,DE
        POP     DE              ; RESTORE TEXT POINTER
        JP      M,XP13          ; 1ST 2ND SIGN DIFFER
        XOR     H               ; 1ST 2ND SIGN EQUAL
        JP      P,XP13          ; SO IS THE RESULT
        IFDEF   SBC
        JP      Q_OV            ; OVERFLOW ERROR
        ELSE
        JP      QHOW            ; ELSE WE HAVE OVERFLOW
        ENDIF
XP15    TSTC    '-',XPR9        ; SUBTRACT?
XP16    PUSH    HL              ; YES, SAVE 1ST <EXPR2>
        CALL    EXPR2           ; GET 2ND <EXPR2>
        CALL    CHGSGN          ; NEGATE
        JR      XP14            ; AND ADD THEM
;
EXPR2   CALL    EXPR3           ; GET 1ST <EXPR3>
XP21    TSTC    '*',XP24        ; MULTIPLY?
        PUSH    HL              ; YES, SAVE 1ST
        CALL    EXPR3           ; AND GET 2ND <EXPR3>
        LD      B,0             ; CLEAR B FOR SIGN
        CALL    CHKSGN          ; CHECK SIGN
        EX      (SP),HL         ; 1ST IN HL
        CALL    CHKSGN          ; CHECK SIGN OF 1ST
        EX      DE,HL
        EX      (SP),HL
        LD      A,H             ; IS HL > 255 ?
        OR      A
        JR      Z,XP22          ; NO
        LD      A,D             ; YES, HOW ABOUT DE
        OR      D
        EX      DE,HL           ; PUT SMALLER IN HL
        IFDEF   SBC
        JP      NZ,A_OV         ; OVERFLOW ERROR
        ELSE
        JP      NZ,AHOW         ; ALSO >, WILL OVERFLOW
        ENDIF
XP22    LD      A,L             ; THIS IS DUMB
        LD      HL,0            ; CLEAR RESULT
        OR      A               ; ADD AND COUNT
        JR      Z,XP25
XP23    ADD     HL,DE
        IFDEF   SBC
        JP      C,Q_OV          ; OVERFLOW ERROR
        ELSE
        JP      C,AHOW          ; OVERFLOW
        ENDIF
        DEC     A
        JR      NZ,XP23
        JR      XP25            ; FINISHED
XP24    TSTC    '/',XPR9        ; DIVIDE?
        PUSH    HL              ; YES, SAVE 1ST <EXPR3>
        CALL    EXPR3           ; AND GET 2ND ONE
        LD      B,0             ; CLEAR B FOR SIGN
        CALL    CHKSGN          ; CHECK SIGN OF 2ND
        EX      (SP),HL         ; GET 1ST IN HL
        CALL    CHKSGN          ; CHECK SIGN OF 1ST
        EX      DE,HL
        EX      (SP),HL
        EX      DE,HL
        LD      A,D             ; DIVIDE BY 0?
        OR      E
        IFDEF   SBC
        JP      Z,A_DZ          ; DIVIDE BY ZERO ERROR
        ELSE
        JP      Z,AHOW          ; SAY "HOW?"
        ENDIF
        PUSH    BC              ; ELSE SAVE SIGN
        CALL    DIVIDE          ; USE SUBROUTINE
        LD      H,B             ; RESULT IN HL NOW
        LD      L,C
        POP     BC              ; GET SIGN BACK
XP25    POP     DE              ; AND TEXT POINTER
        LD      A,H             ; HL MUST BE +
        OR      A
        IFDEF   SBC
        JP      M,Q_OV          ; OVERFLOW ERROR
        ELSE
        JP      M,QHOW          ; ELSE IT IS OVERFLOW
        ENDIF
        LD      A,B
        OR      A
        CALL    M,CHGSGN        ; CHANGE SIGN IF NEEDED
        JR      XP21            ; LOOK FOR MORE TERMS
;
EXPR3   LD      HL,TAB3-1       ; FIND FUNCTION IN TAB3
        JP      EXEC            ; AND GO DO IT
NOTF    CALL    TSTV            ; NO, NOT A FUNCTION
        JR      C,XP32          ; NOR A VARIABLE
        LD      A,(HL)          ; VARIABLE
        INC     HL
        LD      H,(HL)          ; VALUE IN HL
        LD      L,A
        RET
XP32    CALL    TSTNUM          ; OR IS IT A NUMBER
        LD      A,B             ; # OF DIGIT
        OR      A
        RET     NZ              ; OK
PARN    TSTC    '(',XPR0        ; NO DIGIT, MUST BE
PARNP   CALL    EXPR            ; "(EXPR)"
        TSTC    ')',XPR0
XPR9    RET
XPR0
        IFDEF   SBC
        JP      Q_SN            ; SYNTAX ERROR
        ELSE
        JP      QWHAT           ; ELSE SAY: "WHAT?"
        ENDIF
RND     CALL    PARN            ; *** RND(EXPR) ***
        LD      A,H             ; EXPR MUST BE +
        OR      A
        IFDEF   SBC
        JP      M,Q_IA          ; INVALID ARGUMENT ERROR
        ELSE
        JP      M,QHOW
        ENDIF
        OR      L               ; AND NON-ZERO
        IFDEF   SBC
        JP      Z,Q_IA          ; INVALID ARGUMENT ERROR
        ELSE
        JP      Z,QHOW
        ENDIF
        PUSH    DE              ; SAVE BOTH
        PUSH    HL
        LD      HL,(RANPNT)     ; GET MEMORY AS RANDOM
        LD      DE,RANEND
        CALL    COMP
        JR      C,RA1           ; WRAP AROUND IF LAST
        LD      HL,BOTROM
RA1     LD      E,(HL)
        INC     HL
        LD      D,(HL)
        LD      (RANPNT),HL
        POP     HL
        EX      DE,HL
        PUSH    BC
        CALL    DIVIDE          ; RND(N)=MOD(M,N)+1
        POP     BC
        POP     DE
        INC     HL
        RET
;
ABS     CALL    PARN            ; *** ABS(EXPR) ***
        DEC     DE
        CALL    CHKSGN          ; CHECK SIGN
        INC     DE
        RET
SIZEB   LD      HL,(TXTUNF)     ; *** SIZE ***
        PUSH    DE              ; GET THE NUMBER OF FREE
        EX      DE,HL           ; BYTES BETWEEN 'TXTUNF'
        LD      HL,(TXTLMT)     ; AND 'TXTLMT'
        CALL    SUBDE
        POP     DE
        RET
        IFDEF   SBC
;
; The PEEK() function reads a memory address and returns the contents.
;
PEEK    CALL    PARN            ; *** PEEK(EXPR) ***
        LD      A,(HL)          ; READ CONTENTS OF ADDRESS IN HL INTO A
        LD      H,0             ; SET MSB OF RESULT TO 0
        LD      L,A             ; PUT LSB OF RESULT IN L
        RET                     ; RETURN WITH RESULT IN HL
;
; The USR() function calls the machine language routine whose address
; is contained at locations USER+1 and USER+2 (low, high). By default,
; on cold start of Tiny Basic, this points to the RET instruction
; below. You need to write to the addresses to set up the address of
; your own routine.
;
; The integer argument is passed in register HL. On return, the value
; in HL is used as the function return value to Tiny Basic. Any other
; registers must be preserved by your routine.
USR                             ; *** USR(EXPR) ***
        CALL    PARN            ; EVALUATE ARGUMENT, PUT IN HL
        CALL    USER            ; CALL USER DEFINED ROUTINE
RET     RET                     ; RETURN. ALSO USED AS DEFAULT USR() ROUTINE ADDRESS
;
; The INP() function reads an I/O port and returns the result. The
; port must be an 8-bit number.
INP     CALL    PARN            ; *** INP(EXPR) ***
        LD      A,H             ; GET MSB OF PORT ARGUMENT
        OR      A               ; MUST BE ZERO
        JP      NZ,Q_IA         ; OTHERWISE INVALID ARGUMENT ERROR
        LD      C,L             ; GET PORT ARGUMENT
; Use a Z80 instruction to read the port. Otherwise, with 8080
; instructions we could only read from a dynamic port number by using
; self modifying code running out of RAM.
        IN      L,(C)           ; READ PORT
        RET                     ; RETURN WITH RESULT IN HL
        ENDIF
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** DIVIDE *** SUBDE *** CHKSGN *** CHGSGN *** & CKHLDE ***
;
; 'DIVIDE' DIVIDES HL BY DE, RESULT IN BC, REMAINDER IN HL.
;
; 'SUBDE' SUBTRACTS DE FROM HL
;
; 'CHKSGN' CHECKS SIGN OF HL. IF +, NO CHANGE. IF -, CHANGE SIGN AND
; FLIP SIGN OF B.
;
; 'CHGSGN' CHANGES SIGN OF HL AND B UNCONDITIONALLY.
;
; 'CKHLDE' CHECKS SIGN OF HL AND DE. IF DIFFERENT, HL AND DE ARE
; INTERCHANGED. IF SAME SIGN, NOT INTERCHANGED. IN EITHER CASE,
; HL AND DE ARE THEN COMPARED TO SET THE FLAGS.
;
DIVIDE  PUSH    HL              ; *** DIVIDE ***
        LD      L,H             ; DIVIDE H BY DE
        LD      H,0
        CALL    DV1
        LD      B,C             ; SAVE RESULT IN B
        LD      A,L             ; (REMAINDER+L)/DE
        POP     HL
        LD      H,A
DV1     LD      C,-1            ; RESULT IN C
DV2     INC     C               ; DUMB ROUTINE
        CALL    SUBDE           ; DIVIDE BY SUBTRACT
        JR      NC,DV2          ; AND COUNT
        ADD     HL,DE
        RET
SUBDE   LD      A,L             ; *** SUBDE ***
        SUB     E               ; SUBTRACT DE FROM
        LD      L,A             ; HL
        LD      A,H
        SBC     A,D
        LD      H,A
        RET
;
CHKSGN  LD      A,H             ; *** CHKSGN **
        OR      A               ; CHECK SGN OF HL
        RET     P               ; IF -, CHANGE SIGN
;
CHGSGN  LD      A,H             ; *** CHGSGN ***
        OR      L
        RET     Z
        LD      A,H
        PUSH    AF 
        CPL                     ; CHANGE SIGN OF HL
        LD      H,A
        LD      A,L
        CPL
        LD      L,A
        INC     HL
        POP     AF 
        XOR     H
        IFDEF   SBC
        JP      P,Q_SN          ; SYNTAX ERROR
        ELSE
        JP      P, QHOW
        ENDIF
        LD      A,B             ; ALSO FLIP B
        XOR     080H
        LD      B,A
        RET
;
CKHLDE  LD      A,H
        XOR     D               ; SAME SIGN?
        JP      P,CK1           ; YES, COMPARE
        EX      DE,HL           ; NO, XCH AND COM
CK1     CALL    COMP
        RET
;
COMP    LD      A,H             ; *** COMP ***
        CP      D               ; COMPARE HL WITH DE
        RET     NZ              ; RETURN CORRECT C AND
        LD      A,L             ; Z FLAGS
        CP      E               ; BUT OLD A IS LOST
        RET
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** SETVAL *** FIN *** ENDCHK *** & ERROR (& FRIENDS) **
;
; "SETVAL" EXPECTS A VARIABLE, FOLLOWED BY AN EQUAL SIGN AND THEN AN
; EXR. IT EVALUATES THE EXR. AND SETS THE VARIABLE TO THAT VALUE
;
; "FIN" CHECKS THE END OF A COMMAND. IF IT ENDED WITH ":", EXECUTION
; CONTINUES. IF IT ENDED WITH A CR, IT FINDS THE NEXT LINE AND
; CONTINUES FROM THERE.
;
; "ENDCHK" CHECKS IF A COMMAND ENDS WITH A CR. THIS IS REQUIRED IN
; CERTAIN COMMANDS (GOTO, RETURN, STOP, ETC.).
;
; "ERROR" PRINTS THE STRING POINTED TO BY DE (AND ENDS WITH CR). IT THEN
; PRINTS THE LINE POINTED BY 'CURRNT' WITH A "?" INSERTED AT WHERE THE
; OLD TEXT POINTER (SHOULD BE ON TOP OF THE STACK) POINTS TO.
; EXECUTION OF TB IS STOPPED AND TBI IS RESTARTED. HOWEVER, F
; 'CURRENT'-> ZERO (INDICATING A DIRECT COMMAND), THE DIRECT COMMAND
; IS NOT PRINTED. AND IF 'CURRNT' -> NEGATIVE # (INDICATING 'INPUT'
; COMMAND, THE INPUT LINE IS NOT PRINTED AND EXECUTION IS NOT
; TERMINATED BUT CONTINUED AT 'INPERR'.
;
; RELATED TO 'ERROR' ARE THE FOLLOWING: 'QWHAT' SAVES TEXT POINTER ON
; THE STACK AND GETS THE MESSAGE "WHAT?". 'AWHAT' JUST GETS THE MESSAGE "WHAT?" AND
; JUMPS TO 'ERROR'. 'QSORRY' AND 'ASORRY" DO THE SAME KIND OF THING.
; 'QHOW' AND 'AHOW' IN THE ZERO PAGE SECTION ALSO DO THIS.
;
SETVAL  CALL    TSTV            ; *** SETVAL ***
        IFDEF   SBC
        JR      C,Q_SN          ; SYNTAX ERROR
        ELSE
        JR      C,QWHAT         ; "WHAT?" NO VARIABLE
        ENDIF
        PUSH    HL              ; SAVE ADDRESS OF VAR.
        TSTC    '=',SV1         ; PASS "=" SIGN
        CALL    EXPR            ; EVALUATE EXPR.
        LD      B,H             ; VALUE IN BC NOW
        LD      C,L
        POP     HL              ; GET ADDRESS
        LD      (HL),C          ; SAVE VALUE
        INC     HL
        LD      (HL),B
        RET
;
FINISH  CALL    FIN             ; CHECK END OF COMMAND
SV1
        IFDEF   SBC
        JR      Q_SN            ; SYNTAX ERROR
        ELSE
        JR      QWHAT           ; PRINT "WHAT?" IF WRONG
        ENDIF
FIN     TSTC    ':',FI1         ; *** FIN *** Original Tiny Basic used ";"
        POP     AF              ; ":", PURGE RET ADDR.
        JP      RUNSML          ; CONTINUE SAME LINE
FI1     TSTC    CR,FI2          ; NOT ":", IT IS CR?
        POP     AF              ; YES, PURGE RET ADDR.
        JP      RUNNXL          ; RUN NEXT LINE
FI2     RET                     ; ELSE RETURN TO CALLER
IGNBLK  LD      A,(DE)          ; ** IGNBLK ***
        CP      ' '             ; IGNORE BLANKS
        RET     NZ              ; IN TEXT (WHERE DE->)
        INC     DE              ; AND RETURN THE FIRST
        JR      IGNBLK          ; NON-BLANK CHAR. IN A
;
ENDCHK  CALL    IGNBLK          ; *** ENDCHK ***
        CP      CR              ; END WITH CR?
        RET     Z               ; OK, ELSE SAY: "WHAT?"
;
        IFDEF   SBC
        JR      Q_SN            ; SYNTAX ERROR
        ELSE
QWHAT   PUSH    DE              ; *** QWHAT ***
AWHAT   LD      DE,WHAT         ; **  AWHAT ***
        ENDIF
; Optional detailed error messages. More explanatory than Tiny Basic's
; defaults of just "SORRY", "HOW?", and "WHAT?"
; TODO: Refactor common code to make it shorter and less repetitive.

Q_DZ    PUSH    DE
A_DZ    LD      DE,S_DZ
        JR      ERROR

Q_IA    PUSH    DE
A_IA    LD      DE,S_IA
        JR      ERROR

Q_IS    PUSH    DE
A_IS    LD      DE,S_IS
        JR      ERROR

Q_LF    PUSH    DE
A_LF    LD      DE,S_LF
        JR      ERROR

Q_NV    PUSH    DE
A_NV    LD      DE,S_NV
        JR      ERROR

Q_OM    PUSH    DE
A_OM    LD      DE,S_OM
        JR      ERROR

Q_OV    PUSH    DE
A_OV    LD      DE,S_OV
        JR      ERROR

Q_SN    PUSH    DE
A_SN    LD      DE,S_SN
        JR      ERROR

ERROR   CALL    CRLF
        CALL    PRTSTG          ; PRINT ERROR MESSAGE
        LD      HL,(CURRNT)     ; GET CURRENT LINE #
        PUSH    HL
        LD      A,(HL)          ; CHECK THE VALUE
        INC     HL
        OR      (HL)
        POP     DE
        JP      Z,TELL          ; IF ZERO, JUST RESTART
        LD      A,(HL)          ; IF NEGATIVE
        OR      A
        JP      M,INPERR        ; REDO INPUT
        CALL    PRTLN           ; ELSE PRINT THE LINE
        POP     BC
        LD      B,C
        CALL    PRTCHS
        LD      A,'?'           ; PRINT A "?"
        CALL    OUTCH
        CALL    PRTSTG          ; LINE
        JP      TELL            ; THEN RESTART
        IFNDEF  SBC
QSORRY  PUSH    DE              ; *** GSORRY ***
ASORRY  LD      DE,SORRY        ; *** ASORRY **
        JR      ERROR
        ENDIF
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** FINDLN (& FRIENDS) ***
;
; 'FNDLN' FINDS A LINE WITH A GIVEN LINE # (IN HL) IN THE TEXT SAVE
; AREA. DE IS USED AS THE TEXT POINTER. IF THE LINE IS FOUND, DE
; WILL POINT TO THE BEGINNING OF THAT LINE (I.E. THE LOW BYTE OF THE
; LINE #), AND FLAGS ARE NC AND Z. IF THAT LINE IS NOT THERE AND A LINE
; WITH A HIGHER LINE # IS FOUND, DE POINTS TO THERE AND FLAGS ARE NC &
; NZ. IF WE REACHED THE END OF TEXT SAVE AREA AND CANNOT FIND THE
; LINE, FLAGS ARE C AND NZ. 'FINDLN' WILL INITIALIZE DE TO THE BEGINNING
; OF THE TEXT SAVE AREA TO START THE SEARCH. SOME OTHER ENTRIES OF
; THIS ROUTINE WILL NOT INITIALIZE DE AND DO THE SEARCH. 'FINDLP'
; WILL START WITH DE AND SEARCH FOR THE LINE #. 'FNDNXT' WILL BUMP DE
; BY 2, FIND A CR AND THEN START SEARCH. 'FNDSKP' USES DE TO FIND A
; CR, AND THEN START SEARCH.
;
FNDLN   LD      A,H             ; *** FINDLN ***
        OR      A               ; CHECK SIGN OF HL
        IFDEF   SBC
        JP      M,Q_LF          ; LINE NOT FOUND ERROR
        ELSE
        JP      M,QHOW          ; IT CANNOT BE -
        ENDIF
        LD      DE,TEXT         ; INIT. TEXT POINTER
;
FNDLP   INC     DE              ; IS IT EOR MARK?
        LD      A,(DE)
        DEC     DE
        ADD     A,A
        RET     C               ; C,NZ PASSED END
        LD      A,(DE)          ; WE DID NOT, GET BYTE 1
        SUB     L               ; IS THIS THE LINE?
        LD      B,A             ; COMPARE LOW ORDER
        INC     DE
        LD      A,(DE)          ; GET BYTE 2
        SBC     A,H             ; COMPARE HIGH ORDER
        JR      C,FL1           ; NO, NOT THERE YET
        DEC     DE              ; ELSE WE EITHER FOUND
        OR      B               ; IT, OR IT IS NOT THERE
        RET                     ; NC,Z:FOUND; NC,NZ:NO
;
FNDNXT  INC     DE              ; FIND NEXT LINE
FL1     INC     DE              ; JUST PASSED BYTE 1 & 2
;
FNDSKP  LD      A,(DE)          ; *** FNDSKP ***
        CP      CR              ; TRY TO FIND CR
        JR      NZ,FL1          ; KEEP LOOKING
        INC     DE              ; FOUND CR, SKIP OVER
        JR      FNDLP           ; CHECK IF END OF TEXT
;
TSTV    CALL    IGNBLK          ; *** TSTV ***
        SUB     '@'             ; TEST VARIABLES
        RET     C               ; C:NOT A VARIABLE
        JR      NZ,TV1          ; NOT "@" ARRAY
        INC     DE              ; IT IS THE "@" ARRAY
        CALL    PARN            ; @ SHOULD BE FOLLOWED
        ADD     HL,HL           ; BY (EXPR) AS ITS INDEX
        IFDEF   SBC
        JP      C,S_IS          ; INVALID ARRAY SUBSCRIPT ERROR
        ELSE
        JP      C,QHOW          ; IS INDEX TOO BIG?
        ENDIF
TSTB    PUSH    DE              ; WILL IT FIT?
        EX      DE,HL
        CALL    SIZEB           ; FIND SIZE OF FREE
        CALL    COMP            ; AND CHECK THAT
        IFDEF   SBC
        JR      C,A_OM          ; OUT OF MEMORY ERROR
        ELSE
        JR      C,ASORRY        ; IF NOT, SAY "SORRY"
        ENDIF
        CALL    LOCR            ; IT FITS, GET ADDRESS
        ADD     HL,DE           ; OF A(EXPR) AND PUT IT
        POP     DE              ; IN HL
        RET                     ; C FLAG IS CLEARED
TV1     CP      27              ; NOT @, IS IT A TO Z?
        CCF                     ; IF NOT RETURN C FLAG
        RET     C
        INC     DE              ; IF A THROUGH Z
        LD      HL,VARBGN-2
        RLCA                    ; HL->VARIABLE
        ADD     A,L             ; RETURN
        LD      L,A             ; WITH C FLAG CLEARED
        LD      A,0
        ADC     A,H
        LD      H,A
        RET
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** TSTCH *** TSTNUM ***
;
; TSTCH IS USED TO TEST THE NEXT NON-BLANK CHARACTER IN THE TEXT
; (POINTED TO BY DE) AGAINST THE CHARACTER THAT FOLLOWS THE CALL. IF
; THEY DO NOT MATCH, N BYTES OF CODE WILL BE SKIPPED OVER, WHERE N IS
; BETWEEN 0 AND 255 AND IT IS STORED IN THE SECOND BYTE FOLLOWING THE
; CALL.
;
; TSTNUM IS USED TO CHECK WHETHER THE TEXT (POINTED TO BY DE) IS A
; NUMBER. IF A NUMBER IS FOUND, B WILL BE NON-ZERO AND HL WILL
; CONTAIN THE VALUE (IN BINARY) OF THE NUMBER, ELSE B AND HL ARE 0.
;
TSTCH   EX      (SP),HL         ; *** TSTCH ***
        CALL    IGNBLK          ; IGNORE LEADING BLANKS
        CP      (HL)            ; AND TEST THE CHARACTER
        INC     HL              ; COMPARE THE BYTE THAT
        JR      Z,TC1           ; FOLLOWS THE CALL INST.
        PUSH    BC              ; WITH THE TEXT (DE->)
        LD      C,(HL)          ; IF NOT =, ADD THE 2ND
        LD      B,0             ; BYTE THAT FOLLOWS THE
        ADD     HL,BC           ; CALL TO THE OLD PC
        POP     BC              ; I.E. DO A RELATIVE
        DEC     DE              ; JUMP IF NOT =
TC1     INC     DE              ; IF =, SKIP THOSE BYTES
        INC     HL              ; AND CONTINUE
        EX      (SP),HL
        RET
;
TSTNUM  LD      HL,0            ; *** TSTNUM ***
        LD      B,H             ; TEST IF THE TEXT IS
        CALL    IGNBLK          ; A NUMBER
TN1     CP      '0'             ; IF NOT, RETURN 0 IN
        RET     C               ; B AND HL
        CP      03AH            ; IF NUMBERS, CONVERT
        RET     NC              ; TO BINARY IN HL AND
        LD      A,0F0H          ; SET B TO # OF DIGITS
        AND     H               ; IF H>255. THERE IS NO
        IFDEF   SBC
        JP      NZ,Q_SN         ; SYNTAX ERROR
        ELSE
        JR      NZ,QHOW         ; ROOM FOR NEXT DIGIT
        ENDIF
        INC     B               ; B COUNTS # OF DIGITS
        PUSH    BC
        LD      B,H             ; HL=10*HL+(NEW DIGIT)
        LD      C,L
        ADD     HL,HL           ; WHERE 10* IS DONE BY
        ADD     HL,HL           ; SHIFT AND ADD
        ADD     HL,BC
        ADD     HL,HL
        LD      A,(DE)          ; AND (DIGIT) IS FROM
        INC     DE              ; STRIPPING THE ASCII
        AND     00FH            ; CODE
        ADD     A,L
        LD      L,A
        LD      A,0
        ADC     A,H
        LD      H,A
        POP     BC
        LD      A,(DE)          ; DO THIS DIGIT AFTER
        JP      P,TN1           ; DIGIT. S SAYS OVERFLOW
        IFNDEF  SBC
QHOW    PUSH    DE              ; *** ERROR: "HOW?" ***
AHOW    LD      DE,HOW
        JR  ERROR
        ENDIF
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** MVUP *** MVDOWN *** POPA ** & PUSH A ***
;
; 'MVUP' MOVES A BLOCK UP FROM WHERE DE-> TO WHERE DC-> UNTIL DE = HL
;
; 'MVDOWN' MOVES A BLOCK DOWN FROM WHERE DE-> TO WHERE HL-> UNTIL DE =
; BC
;
; 'POPA' RESTORES THE 'FOR' LOOP VARIABLE SAVE AREA FROM THE STACK
;
; 'PUSHA' STACKS THE 'FOR' LOOP VARIABLE SAVE AREA INTO THE STACK
;
MVUP    CALL    COMP            ; *** MVUP ***
        RET     Z               ; DE = HL, RETURN
        LD      A,(DE)          ; GET ONE BYE
        LD      (BC),A          ; MOVE IT
        INC     DE              ; INCREASE BOTH POINTERS
        INC     BC
        JR      MVUP            ; UNTIL DONE
;
MVDOWN  LD      A,B             ; *** MVDOWN **
        SUB     D               ; TEST IF DE = BC
        JR      NZ,MD1          ; NO, GO MOVE
        LD      A,C             ; MAYBE, OTHER BYTE?
        SUB     E
        RET     Z               ; YES, RETURN
MD1     DEC     DE              ; ELSE MOVE A BYTE
        DEC     HL              ; BUT FIRST DECREASE
        LD      A,(DE)          ; BOTH POINTERS AND
        LD      (HL),A          ; THEN DO IT
        JR      MVDOWN          ; LOOP BACK
;
POPA    POP     BC              ; BC = RETURN ADDR.
        POP     HL              ; RESTORE LOPVAR, BUT
        LD      (LOPVAR),HL     ; =0 MEANS NO MORE
        LD      A,H
        OR      L
        JR      Z,PP1           ; YEP, GO RETURN
        POP     HL              ; NOPE, RESTORE OTHERS
        LD      (LOPINC),HL
        POP     HL
        LD      (LOPLMT),HL
        POP     HL
        LD      (LOPLN),HL
        POP     HL
        LD      (LOPPT),HL
PP1     PUSH    BC              ; BC = RETURN ADDR.
        RET
;
PUSHA   LD      HL,STKLMT       ; *** PUSHA ***
        CALL    CHGSGN
        POP     BC              ; BC=RETURN ADDRESS
        ADD     HL,SP           ; IS STACK NEAR THE TOP?
        IFDEF   SBC
        JP      NC,Q_OM         ; OUT OF MEMORY ERROR
        ELSE
        JR      NC,QSORRY       ; YES, SORRY FOR THAT.
        ENDIF
        LD      HL,(LOPVAR)     ; ELSE SAVE LOOP VAR.S
        LD      A,H             ; BUT IF LOPVAR IS 0
        OR      L               ; THAT WILL BE ALL
        JR      Z,PU1
        LD      HL,(LOPPT)      ; ELSE MORE TO SAVE
        PUSH    HL
        LD      HL,(LOPLN)
        PUSH    HL
        LD      HL,(LOPLMT)
        PUSH    HL
        LD      HL,(LOPINC)
        PUSH    HL
        LD      HL,(LOPVAR)
PU1     PUSH    HL
        PUSH    BC              ; BC = RETURN ADDR.
        RET
LOCR    LD      HL,(TXTUNF)
        DEC     HL
        DEC     HL
        RET
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** PRTSTG *** QTSTG *** PRTNUM *** & PRTLN ***
;
; 'PRTSTG' PRINTS A STRING POINTED TO BY DE. IT STOPS PRINTING AND
; RETURNS TO THE CALLER WHEN EITHER A CR IS PRINTED OR WHEN THE NEXT BYTE
; IS ZERO. REG. A AND B ARE CHANGED. REG. DE POINTS TO WHAT FOLLOWS
; THE CR OR TO THE ZERO.
;
; 'QTSTG' LOOKS FOR UP-ARROW, SINGLE QUOTE, OR DOUBLE QUOTE. IF NOE
; OF THESE, RETURNS TO CALLER. IF UP-ARROW, OUTPUT A CONTROL
; CHARACTER. IF SINGLE OR DOUBLE QUOTE, PRINT THE STRING IN THE QUOTE
; AND DEMAND A MATCHING UNQUOTE. AFTER THE PRINTING THE NEXT 3 BYTES
; OF THE CALLER IS SKIPPED OVER USUALLY A JUMP INSTRUCTION).
;
; 'PRTNUM' PRINTS THE NUMBER IN HL. LEADING BLANKS ARE ADDED IF
; NEEDED TO PAD THE NUMBER OF SPACES TO THE NUMBER IN C. HOWEVER, IF
; THE NUMBER OF DIGITS IS LARGER THAN THE # IN C, ALL DIGITS ARE
; PRINTED ANYWAY. NEGATIVE SIGN IS ALSO PRINTED AND COUNTED IN.
; POSITIVE SIGN IS NOT.
;
; 'PRTLN' FINDS A SAVED LINE. PRINTS THE LINE # AND A SPACE.
;
PRTSTG  SUB     A               ; *** PRTSTG ***
PS1     LD      B,A
PS2     LD      A,(DE)          ; GET A CHARACTER
        INC     DE              ; BUMP POINTER
        CP      B               ; SAME AS OLD A?
        RET     Z               ; YES, RETURN
        CALL    OUTCH           ; ELSE PRINT IT
        CP      CR              ; WAS IT A CR?
        JR      NZ,PS2          ; NO, NEXT
        RET                     ; YES, RETURN
;
QTSTG   TSTC    '"',QT3         ; *** QTSTG ***
        LD      A,'"'           ; IT IS A "
QT1     CALL    PS1             ; PRINT UNTIL ANOTHER
QT2     CP      CR              ; WAS LAST ONE A CR?
        POP     HL              ; RETURN ADDRESS
        JP      Z,RUNNXL        ; WAS CR, RUN NEXT LINE
        INC     HL              ; SKIP 3 BYTES ON RETURN
        INC     HL
        INC     HL
        JP      (HL)            ; RETURN
QT3     TSTC    027H,QT4        ; IS IT A '?
        LD      A,027H          ; YES, DO SAME
        JR      QT1             ; AS IN "
QT4     TSTC    '^',QT5         ; IS IT AN UP ARROW?
        LD      A,(DE)          ; YES, CONVERT CHARACTER
        XOR     040H            ; TO CONTROL-CH.
        CALL    OUTCH
        LD      A,(DE)          ; JUST IN CASE IT IS A CR
        INC     DE
        JR      QT2
QT5     RET                     ; NONE OF ABOVE
PRTCHS  LD      A,E
        CP      B
        RET     Z
        LD      A,(DE)
        CALL    OUTCH
        INC     DE
        JR      PRTCHS
;
PRTNUM                          ; *** PRTNUM ***
PN3     LD      B,0             ; B=SIGN
        CALL    CHKSGN          ; CHECK SIGN
        JP      P,PN4           ; NO SIGN
        LD      B,'-'           ; B=SIGN
        DEC     C               ; '-' TAKES SPACE
PN4     PUSH    DE
        LD      DE,10           ; DECIMAL
        PUSH    DE              ; SAVE AS A FLAG
        DEC     C               ; C=SPACES
        PUSH    BC              ; SAVE SIGN & SPACE
PN5     CALL    DIVIDE          ; DIVIDE HL BY 10
        LD      A,B             ; RESULT 0?
        OR      C
        JR      Z,PN6           ; YES, WE GOT ALL
        EX      (SP),HL         ; NO, SAVE REMAINDER
        DEC     L               ; AND COUNT SPACE
        PUSH    HL              ; HL IS OLD BC
        LD      H,B             ; MOVE RESULT TO BC
        LD      L,C
        JR      PN5             ; AND DIVIDE BY 10
PN6     POP     BC              ; WE GOT ALL DIGITS IN
PN7     DEC     C               ; THE STACK
        LD      A,C             ; LOOK AT SPACE COUNT
        OR      A
        JP      M,PN8           ; NO LEADING BLANKS
        LD      A,' '           ; LEADING BLANKS
        CALL    OUTCH
        JR      PN7             ; MORE?
PN8     LD      A,B             ; PRINT SIGN
        OR      A
        CALL    NZ,OUTCH        ; MAYBE - CR NULL
        LD      E,L             ; LAST REMAINDER IN E
PN9     LD      A,E             ; CHECK DIGIT IN E
        CP      10              ; 10 IS FLAG FOR NO MORE
        POP     DE
        RET     Z               ; IF SO, RETURN
        ADD     A,'0'           ; ELSE CONVERT TO ASCII
        CALL    OUTCH           ; AND PRINT THE DIGIT
        JR      PN9             ; GO BACK FOR MORE
;
PRTLN   LD      A,(DE)          ; *** PRTLN ***
        LD      L,A             ; LOW ORDER LINE #
        INC     DE
        LD      A,(DE)          ; HIGH ORDER
        LD      H,A
        INC     DE
        LD      C,4             ; PRINT 4 DIGIT LINE #
        CALL    PRTNUM
        LD      A,' '           ; FOLLOWED BY A BLANK
        CALL    OUTCH
        RET
;
TAB1    DB      "LIST"          ; DIRECT COMMANDS
        ITEM    LIST
        DB      "NEW"
        ITEM    NEW
        DB      "RUN"
        ITEM    RUN
TAB2    DB      "NEXT"
        ITEM    NEXT
        DB      "LET"
        ITEM    LET
        DB      "IF"
        ITEM    IFF
        DB      "GOTO"
        ITEM    GOTO
        DB      "GOSUB"
        ITEM    GOSUB
        DB      "RETURN"
        ITEM    RETURN
        DB      "REM"
        ITEM    REM
        DB      "FOR"
        ITEM    FOR
        DB      "INPUT"
        ITEM    INPUT
        DB      "PRINT"
        ITEM    PRINT
        DB      "STOP"
        ITEM    STOP
        IFDEF   SBC
        DB      "POKE"
        ITEM    POKE
        DB      "OUT"
        ITEM    OUTP
        ENDIF
        ITEM    MOREC           ; ************************
MOREC   JP      DEFLT           ; *** JMP USER-COMMAND ***
                                ; ************************
TAB3    DB      "RND"           ; FUNCTIONS
        ITEM    RND
        DB      "ABS"
        ITEM    ABS
        DB      "SIZE"
        ITEM    SIZEB
        IFDEF   SBC
        DB      "PEEK"
        ITEM    PEEK
        DB      "USR"
        ITEM    USR
        DB      "INP"
        ITEM    INP
        ENDIF
        ITEM    MOREF           ; *************************
MOREF   JP      NOTF            ; *** JMP USER-FUNCTION ***
                                ; *************************
TAB4    DB      "TO"            ; "FOR" COMMAND
        ITEM    FR1
        IFDEF   SBC
        ITEM    Q_SN
        ELSE
        ITEM    QWHAT
        ENDIF
TAB5    DB      "STEP"          ; "FOR" COMMAND
        ITEM    FR2
        ITEM    FR3
TAB6    DB      ">="            ; RELATION OPERATORS
        ITEM    XPR1
        DB      "<>"            ; Original Tiny Basic used "#"
        ITEM    XPR2
        DB      ">"
        ITEM    XPR3
        DB      "="
        ITEM    XPR5
        DB      "<="
        ITEM    XPR4
        DB      "<"
        ITEM    XPR6
        ITEM    XPR7
RANEND  EQU     $
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** INPUT OUTPUT ROUTINES **
;
; USER MUST VERIFY AND/OR MODIFY THESE ROUTINES
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** CRLF *** OUTCH ***
;
; CRLF WILL OUTPUT A CR. ONLY A & FLAGS MAY CHANGE AT RETURN.
;
; OUTCH WILL OUTPUT THE CHARACTER IN A. IF THE CHARACTER IS CR, IT
; WILL ALSO OUTPUT A LF. FLAGS MAY CHANGE AT RETURN, OTHER REGISTERS
; DO NOT.
;
; *** CHKIO *** GETLN ***
;
; CHKIO CHECKS TO SEE IF THERE IS ANY INPUT. IF NOT INPUT, IT RETURNS
; WITH Z FLAG. IF THERE IS INPUT, IT FURTHER CHECKS WHETHER INPUT IS
; CONTROL-C. IF NOT CONTROL-C, IT RETURNS THE CHARACTER IN A WITH
; FLAG CLEARED. IF INPUT IS CONTROL-C, CHKIO JUMPS TO 'INIT' AND WILL
; NOT RETURN. ONLY A & FLAGS MAY CHANGE AT RETURN.
;
; 'GETLN' READS A INPUT LINE INTO 'BUFFER'. IF FIRST PROMPTS THE
; CHARACTER IN A (GIVEN BY THE CALLER), THEN IT FILLS THE BUFFER
; AND ECHOS. BACKSPACE IS USED TO DELETE THE LAST CHARACTER (IF THERE
; IS ONE). CR SIGNALS THE END OF THE LINE AND CAUSES 'GETLN' TO
; RETURN. WHEN BUFFER IS FULL, 'GETLN' WILL ACCEPT BACKSPACE OR CR
; ONLY AND WILL IGNORE (AND WILL NOT ECHO) OTHER CHARACTERS. AFTER
; THE INPUT LINE IS STORED IN THE BUFFER, TWO MORE BYTES OF FF ARE
; ALSO STORED AND DE POINTS TO THE LAST FF. A & FLAGS ARE ALSO
; CHANGED AT RETURN.
;
CRLF    LD      A,CR            ; CR IN A
                                ; ***********************
OUTCH   JR      OUT             ; *** JMP USER-OUTPUT ***
                                ; ***********************
CHKIO   JR      IN              ; *** JMP USER-INPUT ***
                                ; ***********************
GETLN   LD      DE,BUFFER       ; ***** MODIFY THIS *****
                                ; ***********************
GL1     CALL    OUTCH           ; PROMPT OR ECHO
GL2     CALL    CHKIO           ; GET A CHARACTER
        JR      Z,GL2           ; WAIT FOR INPUT
        CP      LF
        JR      Z,GL2
GL3     LD      (DE),A          ; SAVE CH.
        CP      BS              ; IS IT BACKSPACE?
        JR      NZ,GL4          ; NO, MORE TESTS
        LD      A,E             ; YES, DELETE?
        CP      BUFFER&0FFH
        JR      Z,GL2           ; NOTHING TO DELETE
        LD      A,(DE)          ; DELETE
        DEC     DE
        JR      GL1
GL4     CP      CR              ; WAS IT CR?
        JR      Z,GL5           ; YES, END OF LINE
        LD      A,E             ; ELSE, NO MORE FREE ROOM?
        CP      BUFEND&0FFH
        JR      Z,GL2           ; NO, WAIT FOR CR/RUB-OUT
        LD      A,(DE)          ; YES, BUMP POINTER
        INC     DE
        JR      GL1
GL5     INC     DE              ; END OF LINE
        INC     DE              ; BUMP POINTER
        LD      A,0FFH          ; PUT MARKER AFTER IT
        LD      (DE),A
        DEC     DE
        JR      CRLF
OUT     PUSH    AF              ; OUTPUT ROUTINE
        IFDEF   SBC
OT1     IN      A,(SREG)        ; PRINT WHAT IS IN A
        AND     002H            ; TDRE BIT
        ELSE
OT1     IN      A,(0)           ; PRINT WHAT IS IN A
        AND     001H            ; TBE BIT
        ENDIF
        JR      Z,OT1           ; WAIT UNTIL READY
        POP     AF 
        IFDEF   SBC
        OUT     (DREG),A
        ELSE
        OUT     (1),A
        ENDIF
        CP      CR              ; WAS IT CR?
        RET     NZ              ; NO RETURN
        LD      A,LF            ; YES, GIVE LF
        CALL    OUT
        LD      A,CR
        RET
        IFDEF   SBC
IN      IN      A,(SREG)
        AND     001H            ; RDRF BIT
        ELSE
IN      IN      A,(0)
        AND     002H            ; DAV BIT
        ENDIF
        RET     Z               ; NO INPUT, RETURN ZERO
        IFDEF   SBC
        IN      A,(DREG)        ; CHECK INPUT
        ELSE
        IN      A,(1)           ; CHECK INPUT
        ENDIF
        AND     07FH            ; CONVERT TO 7-BIT ASCII
        CP      003H            ; IS IT CONTROL-C?
        RET     NZ              ; NO, RETURN CH.
        JP      INIT            ; YES, RESTART

S_DZ    DB      "DZ ERROR",CR
S_IA    DB      "IA ERROR",CR
S_IS    DB      "IS ERROR",CR
S_LF    DB      "LF ERROR",CR
S_NV    DB      "NV ERROR",CR
S_OM    DB      "OM ERROR",CR
S_OV    DB      "OV ERROR",CR
S_SN    DB      "SN ERROR",CR

;
; Fill rest of 8K ROM
;
        DB      (02000H-$) DUP (0FFH)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Variables (size in bytes)

vars:   equ     0FF00H
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
