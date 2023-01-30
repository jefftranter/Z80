; 5.3 A Sample File-to-File Copy Program
; See http://www.gaby.de/cpm/manuals/archive/cpm22htm/ch5.htm
;
; The following program provides a relatively simple example of file
; operations. The program source file is created as COPY.ASM using the
; CP/M ED program and then assembled using ASM or MAC, resulting in a
; HEX file. The LOAD program is used to produce a COPY.COM file that
; executes directly under the CCP. The program begins by setting the
; stack pointer to a local area and proceeds to move the second name
; from the default area at 006CH to a 33-byte File Control Block called
; DFCB. The DFCB is then prepared for file operations by clearing the
; current record field. At this point, the source and destination FCBs
; are ready for processing, because the SFCB at 005CH is properly set up
; by the CCP upon entry to the COPY program. That is, the first name is
; placed into the default FCB, with the proper fields zeroed, including
; the current record field at 007CH. The program continues by opening
; the source file, deleting any existing destination file, and creating
; the destination file. If all this is successful, the program loops at
; the label COPY until each record is read from the source file and
; placed into the destination file. Upon completion of the data
; transfer, the destination file is closed and the program returns to
; the CCP command level by jumping to BOOT.
;
;    sample file-to-file copy program
;
;
;    at the ccp level, the command
;
;
;         copy a:x.y b:u.v
;
;
        cpu     8080

boot:   equ     0000h           ; system reboot
bdos:   equ     0005h           ; bdos entry point
fcb1:   equ     005ch           ; first file name
sfcb:   equ     fcb1            ; source fcb
fcb2:   equ     006ch           ; second file name
dbuff:  equ     0080h           ; default buffer
tpa:    equ     0100h           ; beginning of tpa
;
printf: equ     9               ; print buffer func#
openf:  equ     15              ; open file func#
closef: equ     16              ; close file func#
deletef: equ    19              ; delete file func#
readf:  equ     20              ; sequential read func#
writef: equ     21              ; sequential write
makef:  equ     22              ; make file func#
;
        org     tpa             ; beginning of tpa
        lxi     sp,stack        ; set local stack
        mvi     c,16            ; half an fcb
        lxi     d,fcb2          ; source of move
        lxi     h,dfcb          ; destination fcb
mfcb:   ldax    d               ; source fcb
        inx     d               ; ready next
        mov     m,a             ; dest fcb
        inx     h               ; ready next
        dcr     c               ; count 16...0
        jnz     mfcb            ; loop 16 times
;
;    name has been removed, zero cr
        xra     a               ; a = 00h
        sta     dfcbcr          ; current rec = 0
;
;    source and destination fcb's ready
        lxi     d,sfcb          ; source file
        call    open            ; error if 255
        lxi     d,nofile        ; ready message
        inr     a               ; 255 becomes 0
        cz      finis           ; done if no file
;
;       source file open, prep destination
        lxi     d,dfcb          ; destination
        call    delete          ; remove if present
;
        lxi     d,dfcb          ; destination
        call    make            ; create the file
        lxi     d,nodir         ; ready message
        inr     a               ; 255 becomes 0
        cz      finis           ; done if no dir space
;
;    source file open, dest file open
;    copy until end of file on source
;
copy:   lxi     d,sfcb          ; source
        call    read            ; read next record
        ora     a               ; end of file?
        jnz     eofile          ; skip write if so
;
;    not end of file, write the record
        lxi     d,dfcb          ; destination
        call    write           ; write the record
        lxi     d,space         ; ready message
        ora     a               ; 00 if write ok
        cnz     finis           ; end if so
        jmp     copy            ; loop until eof
;
eofile:                         ; end of file, close destination
        lxi     d,dfcb          ; destination
        call    close           ; 255 if error
        lxi     h,wrprot        ; ready message
        inr     a               ; 255 becomes 00
        cz      finis           ; shouldn't happen
;
;    copy operation complete, end
        lxi     d,normal        ; ready message
;
finis:  ; write message given in de, reboot
        mvi     c,printf
        call    bdos            ; write message
        jmp     bdos            ; reboot system
;
;    system interface subroutines
;    (all return directly from bdos)
;
open:   mvi     c,openf
        jmp     bdos
;
close:  mvi     c,closef
        jmp     bdos
;
delete: mvi     c,deletef
        jmp     bdos
;
read:   mvi     c,readf
        jmp     bdos
;
write:  mvi     c,writef
        jmp     bdos
;
make:   mvi     c,makef
        jmp     bdos
;
;    console messages
nofile: db      "no source file$"
nodir:  db      "no directory space$"
space:  db      "out of dat space$"
wrprot: db      "write protected?$"
normal: db      "copy complete$"
;
;    data areas
dfcb:   ds      32              ; destination fcb
dfcbcr: equ     dfcb+32         ; current record
;
        ds      32              ; 16 level stack
stack:
        end
