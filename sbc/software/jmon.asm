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
;   COPY: C <START> <END> <DEST>
;   DUMP: D <START>
;   FILL: F <START> <END> <DATA>...
;   GO: G <ADDRESS>
;   INFO: I
;   CHECKSUM: K <START> <END>
;   CLR SCREEN: L
;   REGISTERS: R
;   SEARCH: S <START> <END> <DATA>...
;   TEST: T <START> <END>
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

; To Do:
; Implement other commands

        org     0000H    ; Start at address 0 if running from ROM

; Constants

prompt: equ '?'          ; Prompt character
CR:     equ '\r'         ; Carriage Return
NL:     equ '\n'         ; Newline
stack:  equ 8000h        ; Starting address for stack

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Main program entry point

Start:

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
        jp      nz,tryD
        call    CopyCommand
        jp      mainloop
tryD:
        cp      'D'
        jp      nz,tryF
        call    DumpCommand
        jp      mainloop
tryF:
        cp      'F'
        jp      nz,tryG
        call    FillCommand
        jp      mainloop
tryG:
        cp      'G'
        jp      nz,tryI
        call    GoCommand
        jp      mainloop
tryI:
        cp      'I'
        jp      nz,tryK
        call    InfoCommand
        jp      mainloop
tryK:
        cp      'K'
        jp      nz,tryL
        call    ChecksumCommand
        jp      mainloop
tryL:
        cp      'L'
        jp      nz,tryR
        call    ClearCommand
        jp      mainloop
tryR:
        cp      'R'
        jp      nz,tryS
        call    RegistersCommand
        jp      mainloop
tryS:
        cp      'S'
        jp      nz,tryT
        call    SearchCommand
        jp      mainloop
tryT:
        cp      'T'
        jp      nz,tryV
        call    TestCommand
        jp      mainloop
tryV:
        cp      'V'
        jp      nz,tryColon
        call    VerifyCommand
        jp      mainloop
tryColon:
        cp      ':'
        jp      nz,tryEquals
        call    MemoryCommand
        jp      mainloop
tryEquals:
        cp      '='
        jp      nz,tryHelp
        call    MathCommand
        jp      mainloop
tryHelp:
        cp      '?'
        jp      nz,invalid
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


; Dump. Dumps memory in hex and ascii, as below:
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
        jp      nc,startScreen  ; Carry set indicates <ESC> pressed
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
        jp      nz,doline       ; Continue until full line displayed

; Now dump line of data in ASCII

        call    PrintSpace      ; Print space
        pop     hl              ; Get start address of line
        ld      b,BYTES         ; Counts number of bytes to be displayed
doAscii:
        ld      a,(hl)          ; Get data at current address
        call    PrintAscii      ; Print it
        inc     hl              ; Increment current address
        dec     b               ; Decrement byte count
        jp      nz,doAscii      ; Continue until full line displayed

        call    PrintCR
        dec     c               ; Decrement count of lines printed
        jp      nz,startLine    ; Do the next line
        push    hl              ; Save HL
        ld      hl,strContinue  ; Prompt whether to continue
        call    PrintString
        pop     hl              ; Restore HL
cont:   call    GetChar         ; Get key
        cp      1BH             ; Escape?
        jp      nz,trySpace        
        call    PrintCR         ; If so, return
        ret
trySpace:
        cp      ' '             ; Space?
        jp      z,startScreen   ; If so, do next screen
        jp      cont            ; Invalid key, try again


; Go.
; Prompts user for address, restores saved registers, and transfers
; control to address.

GoCommand:
        call    PrintChar       ; Echo command back
        call    PrintSpace
        call    GetAddress      ; Prompt for address
        jp      nc,contgo       ; Carry set indicates <ESC> pressed
        call    PrintCR
        ret
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
        jp      nz,z80          ; If not, assume Z80 CPU
        ld      hl,str8080      ; It is an 8080
prnt:   call    PrintString     ; Print CPU type
        call    PrintCR
        ret                     ; Return
z80:    
        ld      hl,strZ80       ; It is a Z80
        jp      prnt


; REGISTERS command.
; Example output:
; A=01 BC=4E56 DE=0000 HL=021C F=10101011 SP=6FFE PC=00C3

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
        ld      a,(save_b)
        call    PrintByte
        ld      a,(save_c)
        call    PrintByte
        call    PrintSpace
        ld      a,'D'
        call    PrintChar
        ld      a,'E'
        call    PrintChar
        call    PrintEquals
        ld      a,(save_d)
        call    PrintByte
        ld      a,(save_e)
        call    PrintByte
        call    PrintSpace
        ld      a,'H'
        call    PrintChar
        ld      a,'L'
        call    PrintChar
        call    PrintEquals
        ld      a,(save_h)
        call    PrintByte
        ld      a,(save_l)
        call    PrintByte
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
        jp      nz,nextbit      ; Repeat until all bits done

        call    PrintSpace
        ld      a,'S'
        call    PrintChar
        ld      a,'P'
        call    PrintChar
        call    PrintEquals
        ld      a,(save_sp)
        call    PrintByte
        ld      a,(save_sp+1)
        call    PrintByte
        call    PrintSpace
        ld      a,'P'
        call    PrintChar
        ld      a,'C'
        call    PrintChar
        call    PrintEquals
        ld      a,(save_pc+1)
        call    PrintByte
        ld      a,(save_pc)
        call    PrintByte
        call    PrintCR
; TODO: Add support for editing registers
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
        jp      c,finish        ; Carry set indicates <ESC> pressed
        ex      de,hl           ; Put HL (start address) in DE
        call    PrintSpace
        call    GetAddress      ; Prompt for end address
        jp      c,finish        ; Carry set indicates <ESC> pressed
        call    PrintSpace
        ex      de,hl           ; Put HL (end address) in DE, start address goes back in HL
        call    GetByte         ; Prompt for fill byte
        jp      c,finish        ; Carry set indicates <ESC> pressed
        ld      b,a             ; Store fill byte in B
fill:
        ld      (hl),b          ; Fill address with byte
        inc     hl              ; Increment current address in HL
        ld      a,h             ; Get H
        cp      d               ; Compare to D
        jp      nz,fill         ; If no match, continue filling
        ld      a,l             ; Get L
        cp      e               ; Compare to E
        jp      nz,fill         ; If no match, continue filling
        ld      (hl),b          ; We are at last address, write byte to it
finish:
        call    PrintCR
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
        jp      c,finish        ; Carry set indicates <ESC> pressed
        ld      a,l             ; Save source address in src (low,high)
        ld      (src),a
        ld      a,h
        ld      (src+1),a
        call    PrintSpace
        call    GetAddress      ; Prompt for end address
        jp      c,finish        ; Carry set indicates <ESC> pressed
        ld      a,l             ; Save destination address in dst (low,high)
        ld      (dst),a
        ld      a,h
        ld      (dst+1),a
        call    PrintSpace
        call    GetAddress      ; Prompt for number of bytes
        jp      c,finish        ; Carry set indicates <ESC> pressed
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
        jp      z,finish        ; If BC is zero, we are done, so return
        ld      a,(de)          ; Get byte from source address (DE)
        ld      (hl),a          ; Store byte in destination address (HL)
        inc     de              ; Increment source address
        inc     hl              ; Increment destination address
        dec     bc              ; Decrement count of bytes
        jp      copy            ; Repeat

; Checksum Command
; Calculate 16-bit checksum of a block of memory.

ChecksumCommand:
        call    PrintChar       ; Echo command back
        call    PrintSpace
        call    GetAddress      ; Prompt for start address
        jp      c,finish        ; Carry set indicates <ESC> pressed
        ex      de,hl           ; Swap HL and DE (put start in DE)
        call    PrintSpace
        call    GetAddress      ; Prompt for end address
        jp      c,finish        ; Carry set indicates <ESC> pressed
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
        adc     a,0              ; Add possible carry from LSB
        ld      b,a             ; Store MSB of checksum
        ld      a,h             ; See if MSB of pointer has reached end address yet
        cp      d               ; e.g. H = D
        jp      nz,inc
        ld      a,l             ; See if MSB of pointer has reached end address yet
        cp      e               ; e.g. L = E
        jp      nz,inc
        call    PrintSpace      ; Done, print checksum value
        ld      h,b             ; Put value in HL
        ld      l,c
        call    PrintAddress
        call    PrintCR
        ret
inc:    inc     hl              ; Increment address pointer
        jp      checkloop


; Unimplemented commands
SearchCommand:
TestCommand:
VerifyCommand:
MemoryCommand:
MathCommand:
        call    PrintChar        ; Echo the command back
        call    PrintCR
        ld      hl,strNotImplemented
        call    PrintString
        call    PrintCR
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
        in      a,(DREG)            ; Read character from data register
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
        jp      c,notPrintable  ; If so, not printable
        cp      '~'+1           ; Greater than tilde?
        jp      nc,notPrintable ; If so, not printable
ppr:    call    PrintChar       ; Print character
        pop     af              ; Restore A
        ret
notPrintable:
        ld      a,'.'
        jp      ppr


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
        jp      z,eos           ; If so, exit
        call    PrintChar       ; Print the character
        inc     hl              ; Advance pointer to next character
        jp      nextch          ; And repeat
eos:    pop     af              ; Restore A register
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
        jp      c,notUpper      ; If so, branch
        cp      'z'+1           ; Greater than 'z'?
        jp      nc,notUpper
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
        and     0Fh             ; Mask out lower nybble
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
        cp      1BH             ; Is it <Escape> ?
        jp      nz,next         ; Branch if not
        sub     a               ; Set A to zero
        scf                     ; Otherwise set carry and return.
        ret
next:   cp      '0'             ; Less than '0'?
        jp       c,GetHex       ; Yes, ignore and try again
        cp      '9'+1           ; Greater than 9?
        jp       c,validDigit   ; Branch if not (is 0-9)
        cp      'A'             ; Less than 'A'?
        jp       c,GetHex       ; Yes, ignore and try again
        cp      'F'+1           ; Greater than 'F'?
        jp       c,validULetter ; Branch if not (is A-F)
        cp      'a'             ; less that 'a'?
        jp       c,GetHex       ; Yes, ignore and try again
        cp      'f'+1           ; Greater than 'f'?
        jp       c,validLLetter ; Branch if not (is a-f)
        jp      GetHex          ; Invalid, try again
validDigit:
        call    PrintChar       ; Echo the character
        sub     '0'             ; Convert digit to binary
        jp      done
validLLetter:
        and     11011111b       ; Convert to lowercase letter to upper case
validULetter:
        call    PrintChar       ; Echo the character
        sub     'A'-10          ; Convert uppercase letter to binary
        jp      done
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
        add     b               ; Add upper nybble to lower
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Strings

strStartup:
        db      "JMON Monitor 0.1 by Jeff Tranter\r\n",0

strInvalid:
        db      "Invalid command. Type ? for help.\r\n",0

strHelp:
        db      "\r\n"
        db      "Valid commands:\r\n"
        db      "C <src> <dest> <num bytes> Copy memory\r\n"
        db      "D <address>                Dump memory\r\n"
        db      "F <start> <end> <data>     Fill memory\r\n"
        db      "G <address>                Go\r\n"
        db      "I                          Show info\r\n"
        db      "K <start> <end>            Checksum\r\n"
        db      "L                          Clear screen\r\n"
        db      "R                          Examine registers\r\n"
        db      "S <start> <end> <data>     Search memory\r\n"
        db      "T <start> <end>            Test memory\r\n"
        db      "V <start> <end> <dest>     Verify memory\r\n"
        db      ": <address> <data>...      Write to memory\r\n"
        db      "= <address> +/- <address>  Hex math calculation\r\n"
        db      "?                          Help\r\n",0

strClearScreen:
        db      $1B,"[2J",$1B,"[H",0       ; VT100/ANSI clear screen, cursor home

strCpuType:
        db      "CPU type: ",0
str8080:
        db      "8080",0
strZ80:
        db      "Z80",0
strContinue:
        db      "Press <Space> to continue, <ESC> to stop ",0
strNotImplemented:
        db      "Sorry, command not yet implemented",0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Variables

vars:   equ     7000H
save_a: equ     vars
save_f: equ     vars+1
save_b: equ     vars+2
save_c: equ     vars+3
save_d: equ     vars+4
save_e: equ     vars+5
save_h: equ     vars+6
save_l: equ     vars+7
save_sp: equ    vars+8
save_pc: equ    vars+10
src:    equ     vars+12        ; Used for commands like Copy
dst:    equ     vars+14
size:   equ     vars+16

        end
