; From the book "8080/Z80 Assembly Language - Techniques for Improved
; Programming" by Alan R. Miller.
;
; Changes:
; - Adapted to the ASL assembler
; - Modified to send to console rather than printer (I don't have one).
; - Changed clock card detection.
;
; Jeff Tranter <tranter@pobox.com>

; Listing 10.2 List an ASCII disk file.
;
; SEND ASCII DISK FILE TO LIST
; PUT DATE AND TIME AT TOP OF PAGE
; ENTIRE FILE LOADS INTO MEMORY FIRST
; FORMFEEDS ADDED WITH AN F ARGUMENT
; EXTRA PAGE ADDED TO MAKE TOTAL EVEN
; UNLESS A P OPTION IS GIVEN
;
; USAGE:
;       LIST <filename>
;       LIST <filename> F (add form feeds)
;       LIST <filename> P (no extra pages)
;       LIST <filename> n (skip n lines)
;
;       (date goes here)
;
        TITLE   "List an ASCII disk file."
;
;
        ORG     100H
BDOS    EQU     5               ;DOS ENTRY POINT
CONS    EQU     1               ;READ CONSOLE
TYPEF   EQU     2               ;LIST OUTPUT NOTE: ORIGINAL CODE USED 5 - PRINTER OUTPUT
PBUF    EQU     9               ;PRINT CONSOLE BUFFER
BRKF    EQU     11              ;KILL? (TRUE IF CHAR)
OPENF   EQU     15              ;FILE OPEN
READF   EQU     20              ;READ FUNCTION
CR      EQU     0DH             ;CARRIAGE RETURN
LF      EQU     0AH             ;LINE FEED
EOF     EQU     1AH             ;END OF FILE
TAB     EQU     9               ;^I
FORMFD  EQU     0CH             ;FORM FEED
LINES   EQU     58              ;LINES/PAGE
LMAX    EQU     66              ;MAX LINES
;
;
FCB     EQU     5CH             ;FILE CONTROL BLOCK
BUFF    EQU     80H             ;DISK BUFFER ADDR
;
; FILE-CONTROL BLOCK DEFINITIONS
;
FCBFN   EQU     FCB+1           ;FILE NAME
FCBRL   EQU     FCB+12          ;CURRENT REEL #
FCBCR   EQU     FCB+32          ;NEXT REC #(0-127)
;
; TIME AND DATE FROM A COMPU/TIME BOARD
;
ADATA   EQU     0C4H            ;PORT A DATA
ACONT   EQU     ADATA+1         ;PORT A CONTROL
BCONT   EQU     ADATA+3         ;PORT B CONTROL
BDATA   EQU     ADATA+2         ;PORT B DATA
;
; SAVE OLD STACK AND SET UP A NEW ONE
;
START:  LXI     H,0
        DAD     SP
        SHLD    OLDSP           ;SAVE STACK
        LXI     SP,STACK
        LXI     D,RULES
        CALL    PRINT           ;HOW TO ABORT
;
; HOW MANY LINES TO SKIP BEFORE STARTING?
;
        LXI     H,0
        LXI     B,6DH           ;3RD ARGUMENT?
SKIP2:  LDAX    B               ;GET CHARACTER
        CPI     ' '             ;BLANK AT END
        JZ      SKIP3           ;DONE
        CPI     'F'             ;NEED FORM FEED?
        JZ      FORM            ;YES
        CPI     'P'             ;EXTRA PAGE?
        JZ      NOPAGE          ;NO
        SUI     '0'             ;REMOVE ASCII BIAS
;
; CONVERT ASCII DECIMAL TO BINARY
;
        MOV     D,H             ;DUPLICATE
        MOV     E,L             ;H,L IN D,E
        DAD     H               ;TIMES 2
        DAD     H               ;TIMES 4
        DAD     D               ;TIMES 5
        DAD     H               ;TIMES 10
        MOV     E,A
        MVI     D,0
        DAD     D               ;ADD NEW BYTE
        INX     B               ;INCR POINTER
        JMP     SKIP2           ;NEXT
;
NOPAGE: STA     PFLAG           ;NO EXTRA PAGE
        JMP     ZERO
;
FORM:   STA     FFLAG           ;SET FOR FORM FEED
ZERO:   XRA     A               ;RESET COUNT
SKIP3:  SHLD    SKIPB           ;SAVE BINARY CNT
;
; READ AS MUCH AS POSSIBLE INTO MEMORY
;
        CALL    SETUP           ;SET UP INPUT FILE
        MVI     A,80H
        STA     IBP             ;SET POINTER TO 80H
        STA     TIME2           ;SET 1ST PASS
;
        LHLD    SKIPB           ;HOW MANY LINES?
MAIN6:  MOV     A,H
        ORA     L               ;H,L = 0?
        JZ      MAIN5           ;NO SKIP
MAIN7:  PUSH    H
        CALL    GNB             ;NO SKIP
        POP     H
        CPI     CR
        JNZ     MAIN7           ;LOOK FOR CR
        DCX     H               ;DECR COUNT
        JMP     MAIN6
;
MAIN5:  XRA     A
        STA     FULL            ;RESET FLAG
MAIN2:  CALL    GNB             ;GET A BYTE
        PUSH    H
        LHLD    BUFFP           ;MEMORY POINTER
        MOV     M,A             ;PUT BYTE IN
        INX     H
        SHLD    BUFFP           ;SAVE POINTER
        MOV     B,A
        MVI     A,0FFH
        CMP     L               ;L=0?
        JNZ     MAIN4           ;NO
; CHECK FDOS
        LDA     7               ;FDOS
        SUI     10              ;CCP-1
        CMP     H               ;TOO BIG?
        JNC     MAIN4           ;NO
        MVI     A,EOF
        MOV     M,A             ;PUT IN MEMORY
        STA     FULL            ;SET FOR FULL
        MOV     B,A
MAIN4:  MOV     A,B             ;GET BYTE
        POP     H
        CPI     EOF
        JNZ     MAIN2           ;NO
;
; CHECK FOR EOF AT END
;
        LHLD    BUFFP           ;GET POINTER
        DCX     H
        MVI     A,EOF
        CMP     M               ;EOF?
        JZ      MAIN3           ;YES
        INX     H
        MOV     M,A             ;PUT IN EOF
        SHLD    BUFFP
;
MAIN3:  CALL    RESET           ;POINTER
;
; PUT TIME AT START OF LISTING
; REMOVE FORMFEED IF FIRST
;
        CALL    CLOCK           ;GET TIME
        LDA     FFLAG           ;FORMFEEDS?
        ORA     A
        CNZ     TWOLN           ;YES
        CALL    GETB            ;GET BYTE
        CPI     FORMFD          ;FORMFEED?
        JNZ     GLOP2           ;NO
        CALL    TWOLN           ;SEND 2 LF
;
GLOOP:  CALL    GETB            ;GET NEXT BYTE
GLOP2:  MOV     B,A             ;SAVE BYTE
        CALL    TAB0            ;PRINT BYTE
        MOV     A,B             ;GET BYTE AGAIN
        CPI     LF              ;END OF LINE?
        JNZ     GLOOP           ;NO
        LDA     LCOUNT          ;GET COUNT
        INR     A               ;INCREMENT IT
        STA     LCOUNT          ;SAVE IT
        CPI     LMAX            ;TOO MANY?
        CNC     NPAGE           ;YES, RESET
        LDA     FFLAG           ;FORM FEEDS?
        ORA     A
        JZ      GLOOP           ;NO
        LDA     LCOUNT          ;GET COUNT
        CPI     LINES           ;END OF PAGE?
        JC      GLOOP           ;NO
        CALL    FILL            ;NEW PAGE
        CALL    TWOLN
        JMP     GLOOP
;
; CHECK FOR ABORT, ANY KEY PRESSED
;
ABORT:  PUSH    H               ;SAVE
        PUSH    D
        PUSH    B
        MVI     C,BRKF          ;CONSOLE READY?
PCHAR2: CALL    BDOS
        POP     B               ;RESTORE
        POP     D
        POP     H
        RET
;
TWOLN:  MVI     B,LF            ;TWO LINES
        LDA     LCOUNT
        INR     A               ;ADD 2 TO
        INR     A               ;COUNT
        STA     LCOUNT
OUTT2:  CALL    OUTT            ;DOUBLE OUTPUT
;
OUTT:   MOV     A,B             ;OUTPUT FROM B
;
; SEND CHARACTER FROM A TO LIST
;
PCHAR:  PUSH    H
        PUSH    D
        PUSH    B               ;SAVED
        MVI     C,TYPEF         ;LIST
        MOV     E,A
        CPI     FORMFD          ;FORMFEED?
        JNZ     PCHAR2          ;PRINT BYTE
;
; FILL OUT PAGE AFTER FORMFEED
;
        CALL    FILL
        MVI     B,LF            ;FOR FORMFEED
        CALL    OUTT
        POP     B
        POP     D
        POP     H
        RET
;
; FILL OUT END OF PAGE
;
FILL:   LDA     LCOUNT          ;LINE COUNT
        MOV     C,A
        CALL    NPAGE           ;INCR PAGE
        MVI     A,LMAX
        SUB     C
        RC                      ;TOO BIG
        RZ
        MOV     C,A
        MVI     B,LF
FILL2:  CALL    OUTT            ;SEND LF
        DCR     C
        JNZ     FILL2
        RET
;
; RESET LINE COUNT, INCREMENT PAGES
;
NPAGE:  XRA     A               ;GET A ZERO
        STA     LCOUNT          ;LINE COUNT
        LDA     PAGES           ;PAGE COUNT
        INR     A
        STA     PAGES
        RET
;
; SEND MESSAGE TO CONSOLE
;
PRINT:  MVI     C,PBUF
        JMP     BDOS
;
; GET NEXT BYTE FROM DISK BUFFER
;
GNB:    LDA     IBP
        CPI     80H
        JNZ     READ
;
; READ ANOTHER BUFFER
;
        CALL    DISKR
        XRA     A
;
; READ THE BYTE AT BUFF+REG A
;
READ:   MOV     E,A
        MVI     D,0
        INR     A
        STA     IBP
; POINTER IS INCREMENTED
; SAVE THE CURRENT FILE ADDRESS
        PUSH    H
        LXI     H,BUFF
        DAD     D
        MOV     A,M
; BYTE IS IN THE ACCUMULATOR
; RESTORE FILE ADDRESS AND INCREMENT
        POP     H
        INX     H
        RET
;
; GET A BYTE FROM MEMORY BUFFER
;
GETB:   PUSH    H
        LHLD    BUFFP
        MOV     A,M
        INX     H
        SHLD    BUFFP
        POP     H
        ANI     7FH             ;STRIP PARITY
        CPI     EOF
        RNZ
        POP     PSW             ;RAISE STACK
;
        LDA     FULL            ;CHECK FLAG
        ORA     A               ;ZERO?
        JZ      FINIS           ;YES, DONE
        CALL    RESET           ;POINTER
        JMP     MAIN5           ;GET MORE
;
; RESET MEMORY POINTER
;
RESET:  PUSH    H
        LXI     H,BUFFER
        SHLD    BUFFP
        POP     H
        RET
NONAME: LXI     D,MES1          ;POINT TO MESSAGE
FINI3:  CALL    PRINT
        JMP     ABOR2
;
; NORMAL END OF LISTING
;
FINIS:  STA     EOFFL           ;SET EOF FLAG
;
        CALL    FILL            ;OUT PAGE
;
; ADD AN EXTRA PAGE IF THERE IS AN ODD NUMBER
; AND P FLAG IS NOT SET
;
        LDA     PFLAG
        ORA     A               ;ZERO?
        JNZ     ABOR2           ;NO
        LDA     PAGES           ;HOW MANY?
        ANI     1               ;ODD?
        JZ      ABOR2           ;NO
;
; ADD BLANK PAGE TO MAKE EVEN
;
; (CAN WE CALL FILL?)
        MVI     B,LMAX          ;LINES
EPAGE:  MVI     A,LF
        CALL    PCHAR
        DCR     B
        JNZ     EPAGE
;
ABOR2:
ABOR3:
        LHLD    OLDSP           ;OLD STACK POINTER
        SPHL
        RET
;
; SETUP FILE AND OPEN FOR INPUT
;
SETUP:  LXI     D,FCB
        MVI     C,OPENF
        CALL    BDOS
;
; CHECK FOR ERRORS
;
        CPI     255
        JZ      BADOPN          ;NO GOOD
;
; OPEN IS OK
;
        XRA     A
        STA     FCBCR
        RET
;
; BAD OPEN
;
BADOPN: LXI     H,FCBFN         ;1ST CHAR
        MOV     A,M             ;GET IT
        CPI     ' '             ;FILE NAME?
        JZ      NONAME          ;NO
        LXI     H,'$?'          ;SET UP FOR PRINT
        SHLD    FCBRL           ;USE INPUT FILENAME
        LXI     D,FCBFN         ;FILENAME
        JMP     FINI3
;
; READ DISK FILE RECORD
;
DISKR:  PUSH    H
        PUSH    D
        PUSH    B
        LXI     D,FCB
        MVI     C,READF
        CALL    BDOS
        POP     B
        POP     D
        POP     H
        ORA     A               ;CHECK FOR ERRS
        RZ                      ;OK
; MAY BE EOF
        CPI     1
        JZ      FEND            ;EOF
;
        LXI     D,MES2
        JMP     ABOR3
;
; FOUND DISK EOF
;
FEND:   LHLD    BUFFP           ;GET POINTER
        MVI     M,EOF
        JMP     MAIN3
;
; TAB COUNTER ROUTINE
; JUMP HERE WITH BYTE IN B
;
TAB0:   MOV     A,B             ;GET BYTE
        CPI     ' '             ;CONTROL CHAR?
        JC      TABCR           ;YES
        CALL    TABN            ;INCR COUNTER
        JMP     OUTT            ;SEND BYTE
;
; INCREMENT TAB COUNTER
; MAKE MODULO 8
;
TABN:   LDA     TABC            ;GET TAB COUNT
        INR     A               ;INCREMENT IT
        ANI     7               ;MODULO 8
        STA     TABC            ;SAVE IT
        RET
;
; READ THE BYTE AFTER ABORT
;
DONE:   CALL    FILL            ;OUT PAGE
        MVI     C,CONS          ;READ CONSOLE
        CALL    BDOS
        JMP     ABOR3           ;RETURN
;
; IF CARRIAGE RETURN THEN ZERO COUNT
;
TABCR:  CPI     CR
        JNZ     TABI            ;NOT CR
        CALL    ABORT
        RRC                     ;KEY PRESSED?
        JC      DONE            ;QUIT
        XRA     A               ;GET A ZERO
        STA     TABC            ;SET TO ZERO
        JMP     OUTT            ;SEND CR
;
; CHECK FOR TAB (CONTROL-I)
;
TABI:   CPI     TAB
        JNZ     TFORM           ;NOT TAB
TAB2:   CALL    BLANK           ;SEND A BLANK
        CALL    TABN            ;INCR TAB COUNT
        JNZ     TAB2            ;MORE
        RET                     ;NO PRINT
;
; CHECK FOR FORMFEED
;
TFORM:  CPI     FORMFD
        JNZ     OUTT            ;NO
        LDA     FFLAG           ;FORM FEED OPTION?
        ORA     A
        JNZ     TFOR2           ;OTHER CONTROL
        MVI     B,LF
        CALL    OUTT
        JMP     FILL            ;NORMAL FORMFEED
TFOR2:  POP     PSW             ;RESTORE STACK
        JMP     GLOOP           ;NEXT BYTE
;
; SEND A BLANK
;
BLANK:  MVI     A,' '
        JMP     PCHAR           ;SEND IT
;
CLOCK:  IN      ADATA           ;BOARD PRESENT?
        CPI     0C4H            ;NOTE: ORIGINAL CODE USED 0FFH
        RZ                      ;NO
        LDA     TIME2           ;PASS?
        ORA     A               ;ZERO?
        RZ                      ;NOT 1ST
        XRA     A
        STA     TIME2           ;SET 1ST
        LXI     H,MON
        CALL    RDATE           ;GET IT
        LXI     H,HOUR
        CALL    TIME1
        LXI     H,PDATE         ;POINT DATE/TIME
        CALL    SEND
        XRA     A               ;GET A ZERO
        STA     FCB+13          ;NAME END
        LXI     H,FCBFN         ;NAME START
        CALL    SEND            ;SHOW
        LXI     H,SCRLF
;
; SEND MESSAGE TO LIST
;
SEND:   MOV     A,M             ;GET BYTE
        ORA     A               ;ZERO AT END
        RZ                      ;DONE
        CALL    PCHAR           ;SEND CHARACTER
        INX     H               ;INCREMENT POINTER
        JMP     SEND
;
; READ A DIGIT
;
RDIGIT: MOV     A,D             ;SELECT DIGIT
        OUT     ADATA
        IN      ADATA           ;RESET INTERRUPT
DWAIT:  IN      ACONT           ;DIGIT PRESENT?
        ANI     80H
        JZ      DWAIT           ;LOOP UNTIL READY
        IN      ADATA           ;READ A DIGIT
        ANI     0FH             ;MASK
        ORI     30H             ;CONVERT TO ASCII
        RET
;
; READ-DATE ROUTINE
;
RDATE:  XRA     A               ;DATE DISPLAY MODE
        OUT     BDATA
        MOV     C,A             ;THIS IS DATE
;
; READ FOUR DIGITS
;
READ4:  MVI     D,0             ;SELECT FIRST DIGIT
RD4:    CALL    RDIGIT          ;DELAY ONE DIGIT SCAN
        CALL    RSDIG           ;READ & STORE DIGIT
        MOV     A,D
        CPI     20H             ;TWO DIGITS DONE?
        JNZ     SKIP            ;SKIP A PLACE
        INX     H               ;SKIP : OR /
SKIP:   CPI     40H
        JNZ     RD4             ;GET ANOTHER DIGIT
        RET
;
; READ TIME & READ AND STORE DIGIT
;
TIME1:  MVI     A,40H           ;TIME DISPLAY MODE
        OUT     BDATA
        MVI     C,1             ;THIS IS TIME
        CALL    READ4           ;GET 4 DIGITS
        INX     H               ;SKIP COLON
        CALL    RSDIG
RSDIG:  CALL    RDIGIT
        MOV     M,A             ;STORE BYTE
        INX     H               ;INCR POINTER
        MOV     A,D
        ADI     10H
        MOV     D,A
        RET
;
; STORAGE AREA
;
PDATE:
        DB      CR,LF,"Date "
MON:    DB      "xx/"           ;MONTH
        DB      "xx/"           ;DAY
        DB      "80"            ;YEAR
        DB      "  Time "
HOUR:   DB      "xx:"           ;HOURS
        DB      "xx:"           ;MINUTES
        DB      "xx"            ;SECONDS
        DB      "   File: ",0
SCRLF:  DB      CR,LF,0
MES1:   DB      CR,LF,"No file name$"
MES2:   DB      CR,LF,"Disk error$"
RULES:  DB      CR,LF
        DB      "Program to list ASCII files"
        DB      CR,LF," Options:"
        DB      " (choose one)",CR,LF
        DB      "  F adds form feeds",CR,LF
        DB      "  P skips extra even page"
        DB      " at end",CR,LF
        DB      "  Decimal number skips "
        DB      "lines at beginning",CR,LF,LF
        DB      " Press any key to abort."
        DB      LF,'$'
TABC:   DB      0               ;TAB COUNTER
TIME2:  DB      80H             ;PASS
EOFFL:  DB      0FFH            ;EOF FLAG
FULL:   DB      0               ;FULL FLAG
LCOUNT: DB      2               ;LINE COUNT
PAGES:  DB      0               ;PAGE COUNT
FFLAG:  DB      0               ;FORMFEED FLAG
PFLAG:  DB      0               ;EXTRA-PAGE FLAG
SKIPB:  DW      0               ;LINES TO SKIP
BUFFP:  DW      BUFFER          ;POINTER
IBP:    DS      2               ;INPUT BUFF POINTER
;
; STACK AREA
OLDSP:  DS      2               ;OLD STACK
        DS      30              ;STACK SPACE
STACK:                          ;NEW STACK
BUFFER: DS      1               ;MEMORY BUFFER
;
        END
