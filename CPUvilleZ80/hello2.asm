; This is an example of the "Hello World" program.
; Uses 8080 assembler mnemonics.

        CPU     8080
        ORG     100h            ; CP/M programs start address.
        JMP     START           ; Go to program start.

; Variable storage space
MsgStr: DB      13,10,"Hello world.",13,10,0
Stack1: DW      0               ; Place to save old stack.
Sbot:   DS      32              ; Temp stack for us to use.

; Constants
STOP:   EQU     $-1             ; Top of our stack.
BDOS:   EQU     5               ; Address of BDOS entry.

; Start of code segment
START:  LXI     H, 0            ; HL = 0.
        DAD     SP              ; HL = SP.
        SHLD    Stack1          ; Save original stack.
        LXI     H, STOP         ; HL = address of new stack.
        SPHL                    ; Stack pointer = our stack.
        LXI     H, MsgStr       ; HL = address of string.
LOOP1:  MOV     A, M            ; Read string char.
        ORA     A               ; Set CPU flags.
        JZ      EXIT            ; If char = 0 done.
        MOV     E, A            ; E = char to send.
        MVI     C, 2            ; We want BDOS func 2.
        PUSH    H               ; Save HL register.
        CALL    BDOS            ; Call BDOS function.
        POP     H               ; Restore HL register
        INX     H               ; Point to next char.
        JMP     LOOP1           ; Do next char.

; Exit and return code
EXIT:   LHLD    Stack1          ; HL = entry stack address.
                                ; SP = value on entry.
        RET                     ; Return control back to CPM.
END
