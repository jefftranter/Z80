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

CR      EQU     0DH             ; CARRIAGE RETURN
LF      EQU     0AH             ; LINE FEED
BS      EQU     08H             ; BACKSPACE

        IFDEF SBC
; 6850 UART I/O registers
SREG    PORT    80H
CREG    PORT    80H
DREG    PORT    81H
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
BOTROM  EQU     00000H
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
STACK                           ; STACK STARTS HERE
        ORG     BOTRAM
TXTUNF  DS      2
TEXT    DS      2

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** INITIALIZE ***
;
        ORG     BOTROM

        IFDEF   SBC

; Reset/RST 00 vector: jump to start entry point
RESET:  JP      INIT

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
        DB      (0066H-$) DUP 0FFH
NMI:    RETN                    ; Return from NMI

; Start actual code at $0100

        DB      (0100H-$) DUP 0FFH
        ENDIF
INIT
        IFDEF   SBC
        DI                      ; Disable interrupts
        LD      A,016H          ; Initialize 6850 ACIA
        OUT     (CREG),A
        ENDIF

        LD      SP,STACK
        CALL    CRLF
        LD      HL,KEYWRD       ; AT POWER ON KEYWRD IS
        LD      A,0C3H          ; PROBABLY NOT C3
        CP      (HL)
        JP      Z,TELL          ; IT IS C3, CONTINUE
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
        JP      RSTART          ; ***** JMP USER-INIT ***
MSG     DB      "TINY "         ; ***********************
        DB      "BASIC"
        DB      " V3.0 12-NOV-2021",CR
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
RSTART  LD      SP,STACK        ; RE-INITIALIZE STACK
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
        JP      Z,DIRECT
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
        JP      NZ,ST3          ; NZ:NOT FOUND. INSERT
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
        JP      Z,RSTART        ; MUST CLEAR THE STACK
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
        JP      NC,QSORRY       ; SORRY, NO ROOM FOR IT
        ENDIF
        LD      (TXTUNF),HL     ; OK, UPDATE TXTUNF
        POP     DE              ; DE->OLD UNFILLED AREA
        CALL    MVDOWN
        POP     DE              ; DE->BEGIN, HL->END
        POP     HL
        CALL    MVUP            ; MOVE NEW LINE TO SAVE
        JP      ST2             ; AREA
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
        JP      Z,EX3
        INC     HL              ; HL->TABLE
        CP      (HL)            ; IF MATCH, TEST NEXT
        JP      Z,EX1
        LD      A,07FH          ; ELSE, SEE IF BIT 7
        DEC     DE              ; OF TABLE IS SET, WHICH
        CP      (HL)            ; IS THE JUMP ADDR, (HI)
        JP      C,EX5           ; C:YES, MATCHED
EX2     INC     HL              ; NC:NO, FIND JUMP ADDR.
        CP      (HL)
        JP      NC,EX2
        INC     HL              ; BUMP TO NEXT TAB. ITEM
        POP     DE              ; RESTORE STRING POINTER
        JP      EXEC            ; TEST AGAINST NEXT ITEM
EX3     LD      A,7FH           ; PARTIAL MATCH, FIND
EX4     INC     HL              ; JUMP ADDR., WHICH IS
        CP      (HL)            ; FLAGGED BY BIT 7
        JP      NC,EX4
EX5     LD      A,(HL)          ; LOAD HL WITH THE JUMP
        INC     HL              ; ADDRESS FROM THE TABLE
        LD      L,(HL)          ; ****************

; The jump table has the high bit set, so we need to clear it to get
; the real address. However, if the real jump table address does have
; the high bit set (which is the case when running from RAM), we
; should not change it.

        IF      BOTROM < 08000H
        AND     07FH            ; CLEAR HIGH BIT TO GET REAL JUMP ADDRESS
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
        JP      EXEC            ; AND EXECUTE IT
;
GOTO    CALL    EXPR            ; *** GOTO EXPR ***
        PUSH    DE              ; SAVE FOR ERROR ROUTINE
        CALL    ENDCHK          ; MUST FIND A CR
        CALL    FNDLN           ; FIND THE TARGET LINE #
        IFDEF   SBC
        JP      NZ,A_LF         ; LINE NOT FOUND ERROR
        ELSE
        JP      NZ,AHOW         ; NO SUCH LINE NUMBER
        ENDIF
        POP     AF              ; CLEAR THE "PUSH DE"
        JP      RUNTSL
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
        JP      LS2             ; AND LOOP BACK
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
        JP      NZ,QHOW
        ENDIF
        LD      C,L             ; AND SAVE IT IN C
        JP      PR5             ; LOOK FOR MORE TO PRINT
PR4     CALL    QTSTG           ; OR IS IT A STRING?
        JP      PR9             ; IF NOT, MUST BE EXPR.
PR5     TSTC    ',',PR8         ; IF ",", GO FIND NEXT
PR6     TSTC    '.',PR7
        LD      A,' '
        CALL    OUTCH
        JP      PR6
PR7     CALL    FIN             ; IN THE LIST
        JP      PR2             ; LIST CONTINUES
PR8     CALL    CRLF            ; LIST ENDS
        JP      FINISH
PR9     CALL    EXPR            ; EVALUATE THE EXPR
        PUSH    BC
        CALL    PRTNUM          ; PRINT THE VALUE
        POP     BC
        JP      PR5             ; MORE TO PRINT?
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
        JP      NZ,AHOW         ; NOT THERE. SAY "HOW?"
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
        JP      FR4
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
        JP      FR6
FR5     ADD     HL,BC           ; EACH LEVEL IS 10 DEEP
FR6     LD      A,(HL)          ; GET THAT OLD 'LOPVAR'
        INC     HL
        OR      (HL)
        JP      Z,FR7           ; 0 SAYS NO MORE IN IT
        LD      A,(HL)
        DEC     HL
        CP      D               ; SAME AS THIS ONE?
        JP      NZ,FR5
        LD      A,(HL)          ; THE OTHER HALF?
        CP      E
        JP      NZ,FR5
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
        JP      Z,NX2           ; OK, THEY AGREE
        POP     DE              ; NO, LET'S SEE
        CALL    POPA            ; PURGE CURRENT LOOP
        LD      HL,(VARNXT)     ; AND POP ONE LEVEL
        JP      NX1             ; GO CHECK AGAIN
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
        JP      C,NX6           ; OUTSIDE LIMIT
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
        JP      IF1             ; THIS IS LIKE 'IF 0'
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
        JP      IP8             ; NO
IP2     CALL    TSTV            ; YES. BUT FOLLOWED BY A
        JP      C,IP5           ; VARIABLE? NO.
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
        JP      INPUT           ; YES, MORE ITEMS.
IP7     JP      FINISH
IP8     PUSH    DE               ; SAVE FOR 'PRTSTG'
        CALL    TSTV            ; MUST BE VARIABLE NOW
        JP      NC,IP11
        IFDEF   SBC
        JP      Q_SN            ; SYNTAX ERROR
        ELSE
        JP      QWHAT           ; "WHAT?" IT IS NOT?
        ENDIF
IP11    LD      B,E
        POP     DE
        CALL    PRTCHS          ; PRINT THOSE AS PROMPT
        JP      IP3             ; YES.INPUT VARIABLE
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
        JP      Z,LT4           ; ELSE IT IS 'LET'
;
LET                             ; *** LET ***
LT2     CALL    SETVAL
LT3     TSTC    ',',LT4         ; SET VALUE TO VAR.
        JP      LET             ; ITEM BY ITEM
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
        JP      XP16            ; TREAT LIKE SUBTRACT
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
        JP      XP14            ; AND ADD THEM
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
        JP      Z,XP22          ; NO
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
        JP      Z,XP25
XP23    ADD     HL,DE
        IFDEF   SBC
        JP      C,Q_OV          ; OVERFLOW ERROR
        ELSE
        JP      C,AHOW          ; OVERFLOW
        ENDIF
        DEC     A
        JP      NZ,XP23
        JP      XP25            ; FINISHED
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
        JP      XP21            ; LOOK FOR MORE TERMS
;
EXPR3   LD      HL,TAB3-1       ; FIND FUNCTION IN TAB3
        JP      EXEC            ; AND GO DO IT
NOTF    CALL    TSTV            ; NO, NOT A FUNCTION
        JP      C,XP32          ; NOR A VARIABLE
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
        JP      C,RA1           ; WRAP AROUND IF LAST
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
SIZE    LD      HL,(TXTUNF)     ; *** SIZE ***
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
        JP      NC,DV2          ; AND COUNT
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
        JP      C,Q_SN          ; SYNTAX ERROR
        ELSE
        JP      C,QWHAT         ; "WHAT?" NO VARIABLE
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
        JP      Q_SN            ; SYNTAX ERROR
        ELSE
        JP      QWHAT           ; PRINT "WHAT?" IF WRONG
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
        JP      IGNBLK          ; NON-BLANK CHAR. IN A
;
ENDCHK  CALL    IGNBLK          ; *** ENDCHK ***
        CP      CR              ; END WITH CR?
        RET     Z               ; OK, ELSE SAY: "WHAT?"
;
        IFDEF   SBC
        JP      Q_SN            ; SYNTAX ERROR
        ELSE
QWHAT   PUSH    DE              ; *** QWHAT ***
AWHAT   LD      DE,WHAT         ; **  AWHAT ***
        ENDIF
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
        JP      ERROR
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
        JP      C,FL1           ; NO, NOT THERE YET
        DEC     DE              ; ELSE WE EITHER FOUND
        OR      B               ; IT, OR IT IS NOT THERE
        RET                     ; NC,Z:FOUND; NC,NZ:NO
;
FNDNXT  INC     DE              ; FIND NEXT LINE
FL1     INC     DE              ; JUST PASSED BYTE 1 & 2
;
FNDSKP  LD      A,(DE)          ; *** FNDSKP ***
        CP      CR              ; TRY TO FIND CR
        JP      NZ,FL1          ; KEEP LOOKING
        INC     DE              ; FOUND CR, SKIP OVER
        JP      FNDLP           ; CHECK IF END OF TEXT
;
TSTV    CALL    IGNBLK          ; *** TSTV ***
        SUB     '@'             ; TEST VARIABLES
        RET     C               ; C:NOT A VARIABLE
        JP      NZ,TV1          ; NOT "@" ARRAY
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
        CALL    SIZE            ; FIND SIZE OF FREE
        CALL    COMP            ; AND CHECK THAT
        IFDEF   SBC
        JP      C,A_OM          ; OUT OF MEMORY ERROR
        ELSE
        JP      C,ASORRY        ; IF NOT, SAY "SORRY"
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
        JP      Z,TC1           ; FOLLOWS THE CALL INST.
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
        JP      NZ,QHOW         ; ROOM FOR NEXT DIGIT
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
        JP  ERROR
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
        JP      MVUP            ; UNTIL DONE
;
MVDOWN  LD      A,B             ; *** MVDOWN **
        SUB     D               ; TEST IF DE = BC
        JP      NZ,MD1          ; NO, GO MOVE
        LD      A,C             ; MAYBE, OTHER BYTE?
        SUB     E
        RET     Z               ; YES, RETURN
MD1     DEC     DE              ; ELSE MOVE A BYTE
        DEC     HL              ; BUT FIRST DECREASE
        LD      A,(DE)          ; BOTH POINTERS AND
        LD      (HL),A          ; THEN DO IT
        JP      MVDOWN          ; LOOP BACK
;
POPA    POP     BC              ; BC = RETURN ADDR.
        POP     HL              ; RESTORE LOPVAR, BUT
        LD      (LOPVAR),HL     ; =0 MEANS NO MORE
        LD      A,H
        OR      L
        JP      Z,PP1           ; YEP, GO RETURN
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
        JP      NC,QSORRY       ; YES, SORRY FOR THAT.
        ENDIF
        LD      HL,(LOPVAR)     ; ELSE SAVE LOOP VAR.S
        LD      A,H             ; BUT IF LOPVAR IS 0
        OR      L               ; THAT WILL BE ALL
        JP      Z,PU1
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
        JP      NZ,PS2          ; NO, NEXT
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
        JP      QT1             ; AS IN "
QT4     TSTC    '^',QT5         ; IS IT AN UP ARROW?
        LD      A,(DE)          ; YES, CONVERT CHARACTER
        XOR     040H            ; TO CONTROL-CH.
        CALL    OUTCH
        LD      A,(DE)          ; JUST IN CASE IT IS A CR
        INC     DE
        JP      QT2
QT5     RET                     ; NONE OF ABOVE
PRTCHS  LD      A,E
        CP      B
        RET     Z
        LD      A,(DE)
        CALL    OUTCH
        INC     DE
        JP      PRTCHS
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
        JP      Z,PN6           ; YES, WE GOT ALL
        EX      (SP),HL         ; NO, SAVE REMAINDER
        DEC     L               ; AND COUNT SPACE
        PUSH    HL              ; HL IS OLD BC
        LD      H,B             ; MOVE RESULT TO BC
        LD      L,C
        JP      PN5             ; AND DIVIDE BY 10
PN6     POP     BC              ; WE GOT ALL DIGITS IN
PN7     DEC     C               ; THE STACK
        LD      A,C             ; LOOK AT SPACE COUNT
        OR      A
        JP      M,PN8           ; NO LEADING BLANKS
        LD      A,' '           ; LEADING BLANKS
        CALL    OUTCH
        JP      PN7             ; MORE?
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
        JP      PN9             ; GO BACK FOR MORE
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
        ITEM    SIZE
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
OUTCH   JP      OUT             ; *** JMP USER-OUTPUT ***
                                ; ***********************
CHKIO   JP      IN              ; *** JMP USER-INPUT ***
                                ; ***********************
GETLN   LD      DE,BUFFER       ; ***** MODIFY THIS *****
                                ; ***********************
GL1     CALL    OUTCH           ; PROMPT OR ECHO
GL2     CALL    CHKIO           ; GET A CHARACTER
        JP      Z,GL2           ; WAIT FOR INPUT
        CP      LF
        JP      Z,GL2
GL3     LD      (DE),A          ; SAVE CH.
        CP      BS              ; IS IT BACKSPACE?
        JP      NZ,GL4          ; NO, MORE TESTS
        LD      A,E             ; YES, DELETE?
        CP      BUFFER&0FFH
        JP      Z,GL2           ; NOTHING TO DELETE
        LD      A,(DE)          ; DELETE
        DEC     DE
        JP      GL1
GL4     CP      CR              ; WAS IT CR?
        JP      Z,GL5           ; YES, END OF LINE
        LD      A,E             ; ELSE, NO MORE FREE ROOM?
        CP      BUFEND&0FFH
        JP      Z,GL2           ; NO, WAIT FOR CR/RUB-OUT
        LD      A,(DE)          ; YES, BUMP POINTER
        INC     DE
        JP      GL1
GL5     INC     DE              ; END OF LINE
        INC     DE              ; BUMP POINTER
        LD      A,0FFH          ; PUT MARKER AFTER IT
        LD      (DE),A
        DEC     DE
        JP      CRLF
OUT     PUSH    AF              ; OUTPUT ROUTINE
        IFDEF   SBC
OT1     IN      A,(SREG)        ; PRINT WHAT IS IN A
        AND     002H            ; TDRE BIT
        ELSE
OT1     IN      A,(0)           ; PRINT WHAT IS IN A
        AND     001H            ; TBE BIT
        ENDIF
        JP      Z,OT1           ; WAIT UNTIL READY
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
        IFDEF SBC

; Optional detailed error messages. More explanatory than Tiny Basic's
; defaults of just "SORRY", "HOW?", and "WHAT?"
; TODO: Refactor common code to make it shorter and less repetitive.

Q_DZ    PUSH    DE
A_DZ    LD      DE,S_DZ
        JP      ERROR

Q_IA    PUSH    DE
A_IA    LD      DE,S_IA
        JP      ERROR

Q_IS    PUSH    DE
A_IS    LD      DE,S_IS
        JP      ERROR

Q_LF    PUSH    DE
A_LF    LD      DE,S_LF
        JP      ERROR

Q_NV    PUSH    DE
A_NV    LD      DE,S_NV
        JP      ERROR

Q_OM    PUSH    DE
A_OM    LD      DE,S_OM
        JP      ERROR

Q_OV    PUSH    DE
A_OV    LD      DE,S_OV
        JP      ERROR

Q_SN    PUSH    DE
A_SN    LD      DE,S_SN
        JP      ERROR

S_DZ    DB      "DIVIDE BY ZERO",CR
S_IA    DB      "INVALID ARGUMENT",CR
S_IS    DB      "INVALID ARRAY SUBSCRIPT",CR
S_LF    DB      "LINE NOT FOUND",CR
S_NV    DB      "NO SUCH VARIABLE",CR
S_OM    DB      "OUT OF MEMORY",CR
S_OV    DB      "OVERFLOW ERROR",CR
S_SN    DB      "SYNTAX ERROR",CR

; Fill remainder of ROM with FF
        DB      (02000H-$) DUP (0FFH)
        ENDIF
        END
