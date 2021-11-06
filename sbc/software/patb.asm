; To Do:
; Enter all source code.
; Get it to assemble.
; Get same binary as original article.
; Fix any warnings.
; Port to SBC (RAM or ROM?)

; Port of Palo Alto Tiny BASIC Version Three, by Li-Chen Wang.

; Source taken from "PCC's Reference Book of Personal and Home
; Computing" with minor changes to correct some spelling and
; grammatical errors in comments.
; Adapted to ASL assembler and ported to my Z80 SBC.
;
; Possible enhancements:
; - Use more standard statement separator ":" rather than ";".
; - Use more standard operator "<>" rather than "#" for not equal.
; - Make error messages longer/more descriptive.
; - Add support for more commands, e.g. PEEK, POKE, USR().
; - Convert from 8080 to Z80 mnemonics.

        CPU     8080

CR      EQU     0DH             ; CARRIAGE RETURN

TSTC    MACRO   P1,P2
        CALL    TSTCH
        DB      P1
        DB      P2-$
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
;  0080-01FF  ARE FOR VARIABLES, INPUT LINE, AND STACK
;  2000-3FFF  ARE FOR TINY BASIC TEXT & ARRAY
;  F000-F7FF  ARE FOR TBI CODE
;

BOTSCR  EQU     00080H
TOPSCR  EQU     00200H
BOTRAM  EQU     02999H
DFTLMT  EQU     04000H
BOTROM  EQU     0F000H

;
;  DEFINE VARIABLES, BUFFER, AND STACK IN RAM

        ORG     BOTSCR
KEYWRD  DS      1               ; WAS INIT DONE?
TXTLMT  DS      1               ; ->LIMIT OF TEXT AREA
VARBGN  DS      2*26            ; TB VARIABLES A-Z
CURRNT  DS      2               ; POINTS TO CURRENT LINE
STKGOS  DS      2               ; SAVES SP IN 'GOSUB'
VARNXT  DS      0               ; TEMP STORAGE
STKINP  DS      2               ; SAVES SP IN 'INPUT'
LOPVAR  DS      2               ; 'FOR' LOOP SAVE AREA
LOPINC  DS      2               ; INCREMENT
LOPLMT  DS      2               ; LIMIT
LOPLN   DS      2               ; LINE NUMBER
LOPPT   DS      2               ; TEXT POINTER
RANPNT  DS      2               ; RANDOM NUMBER POINTER
        DS      1               ; EXTRA BYTE FOR BUFFER
BUFFER  DS      132             ; INPUT BUFFER
BUFEND  DS      0               ; BUFFER ENDS
        DS      4               ; EXTRA BYTES FOR STACK
STKLMT  DS      0               ; SOFT LIMIT FOR STACK
        ORG     TOPSCR
STACK   DS      0               ; STACK STARTS HERE
        ORG     BOTRAM
TXTONF  DS      2
TEXT    DS      2

;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; *** INITIALIZE
;

        ORG     BOTROM
INIT    LXI     SP,STACK
        CALL    CRLF
        LXI     H,KEYWRD        ; AT POWER ON KEYWRD IS
        MVI     A,0C3H          ; PROBABLY NOT C3
        CMP     M
        JZ      TELL            ; IT IS C3, CONTINUE
        MOV     M,A             ; AND SET DEFAULT VALUE
        SHLD    TXTLMT          ; IN 'TXTLMT'
        MVI     A,BOTROM>>8     ; INITIALIZE RANPNT
        STA     RANPNT+1
PURGE   LXI     H,TEXT+4        ; PURGE TEXT AREA
        SHLD    TXTUNF
        MVI     H,0FFH
        SHLD    TEXT
TELL    LXI     D,MSG           ; TELL USER
        CALL    PRTSTG          ; ***********************
        JMP     RSTART          ; ***** JMP USER-INIT ***
MSG     DB      "TINY "         ; ***********************
        DB      "BASIC"

        DB      " V3.0",CR
OK      DB      "OK",CR
WHAT    DB      "WHAT?",CR
HOW     DB      "HOW?",CR
SORRY   DB      "SORRY",CR
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
RSTART  LXI     SP,STACK        ; RE-INITIALIZE STACK
        LXI     H,ST1+1         ; LITERAL 0
        SHLD    CURRNT          ; CURRNT->LINE # = 0
ST1     LXI     H,0
        SHLD    LOPVAR
        SHLD    STKGOS
        LXI     D,OK            ; DE->STRING
        CALL    PRTSTG          ; PRINT STRING UNTIL CR
ST2     MVI     A,'>'           ; PROMPT '>' AND
        CALL    GETLN           ; READ A LINE
        PUSH    D               ; DE->END OF LINE
        LXI     D,BUFFER        ; DE->BEGINNING OF LINE
        CALL    TSTNUM          ; TEST IF IT IS A NUMBER
        CALL    IGNBLK
        MOV     A,H             ; HL=VALUE OF THE # OR
        ORA     L               ; 0 IF NO # WAS FOUND
        POP     B               ; BC->END OF LINE
        JZ      DIRECT
        DCX     D               ; BACKUP DE AND SAVE
        MOV     A,H             ; VALUE OF LINE # THERE
        STAX    D
        DCX     D
        MOV     A,L
        STAX    D
        PUSH    B               ; BC,DE->BEGIN, END
        PUSH    D
        MOV     A,C
        SUB     E
        PUSH    PSW             ; A=# OF BYTES IN LINE
        CALL    FNDLN           ; FIND THIS LINE IN SAVE
        PUSH    D               ; AREA, DE->SAVE AREA
        JNZ     ST3             ; NZ:NOT FOUND. INSERT
        PUSH    D               ; Z:FOUND. DELETE IT
        CALL    FNDNXT          ; SET DE->NEXT LINE
        POP     B               ; BC->LINE TO BE DELETED
        LHLD    TXTUNF          ; HL->UNFILLED SAVE AREA

        CALL    MVUP            ; MOVE UP TO DELETE
        MOV     H,B             ; TXTUNF->UNFILLED AREA
        MOV     L,C
        SHLD    TXTUNF          ; UPDATE
ST3     POP     B               ; GET READY TO INSERT
        LHLD    TXTUNF          ; BUT FIRST CHECK IF
        POP     PSW             ; THE LENGTH OF NEW LINE
        PUSH    H               ; IS 3 (LINE # AND CR)
        CPI     3               ; THEN DO NOT INSERT
        JZ      RSTART          ; MUST CLEAR THE STACK
        ADD     L               ; COMPUTE NEW TXTUNF
        MOV     E,A
        MVI     A,0
        ADC     H
        MOV     D,A             ; DE->NEW UNFILLED AREA
        LHLD    TXTLMT          ; CHECK TO SEE IF THERE
        XCHG
        CALL    COMP            ; IS ENOUGH SPACE
        JNC     GSORRY          ; SORRY, NO ROOM FOR IT
        SHLD    TXTUNF          ; OK, UPDATE TXTUNF
        POP     D               ; DE->OLD UNFILLED AREA
        CALL    MVDOWN
        POP     D               ; DE->BEGIN, HL->END
        POP     H
        CALL    MVUP            ; MOVE NEW LINE TO SAVE
        JMP     ST2             ; AREA
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
DIRECT  LXI     H,TAB1-1        ; *** DIRECT ***
;
EXEC    CALL    IGNBLK          ; *** EXEC **
        PUSH    D               ; SAVE POINTER
EX1     LDAX    D               ; IF FOUND '.' IN STRING
        INX     D               ; BEFORE ANY MISMATCH
        CPI     '.'             ; WE DECLARE A MATCH
        JZ      EX3
        INX     H               ; HL->TABLE
        CMP     M               ; IF MATCH, TEST NEXT
        JZ      EX1
        MVI     A,07FH          ; ELSE, SEE IF BIT 7
        DCX     D               ; OF TABLE IS SET, WHICH
        CMP     M               ; IS THE JUMP ADDR, (HI)
        JC      EX5             ; C:YES, MATCHED
EX2     INX     H               ; NC:NO, FIND JUMP ADDR.
        CMP     M
        JNC     EX2
        INX     H               ; BUMP TO NEXT TAB. ITEM
        POP     D               ; RESTORE STRING POINTER
        JMP     EXEC            ; TEST AGAINST NEXT ITEM
EX3     MVI     A,7FH           ; PARTIAL MATCH, FIND
EX4     INX     H               ; JUMP ADDR., WHICH IS

        CMP     M               ; FLAGGED BY BIT 7
        JNC     EX4
EX5     MOV     A,M             ; LOAD HL WITH THE JUMP
        INX     H               ; ADDRESS FROM THE TABLE
        MOV     L,M             ; ****************
        ANI     0FFH            ; ***** ANI 07FH *****
        MOV     H,A             ; ****************
        POP     PSW             ; CLEAN UP THE GARBAGE
        PCHL                    ; AND WE GO DO IT
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
; *** NEW *** STOP ** RUN (& FRIENDS) *** & GOTO
;
; 'NEW(CR)' RESETS 'TTXTUNF'
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
NEW     CALL    ENDCHK          ; ** NEW(CR) **
        JMP     PURGE
;
STOP    CALL    ENDCHK          ; *** STOP(CR) *
        JMP     RSTART
;
RUN     CALL    ENDCHK          ; *** RUN (CR) ***
        LXI     D,TEXT          ; FIRST SAVED LINE
;
RUNNXL  LXI     H,0             ; *** RUNNXL ***
        CALL    FNDLP           ; FIND WHATEVER LINE #
        JC      RSTART          ; C:PASSED TXTUNF, QUIT
;
RUNTSL  XCHG                    ; *** RUNTSL **
        SHLD    CURRNT          ; SET 'CURRNT'->LINE #
        XCHG
        INX     D               ; BUMP PASS LINE #
        INX     D
;
RUNSML  CALL    CHKIO           ; *** RUNSML **
        LXI     H,TAB2-1        ; FIND COMMAND IN TAB2
        JMP     EXEC            ; AND EXECUTE IT
;
GOTO    CALL    EXPR            ; GOTO EXPR ***
        PUSH    D               ; SAVE FOR ERROR ROUTINE
        CALL    ENDCHK          ; MUST FIND A CR
        CALL    FNDLN           ; FIND THE TARGET LINE #
        JNZ     AHOW            ; NO SUCH LINE NUMBER
        POP     PSW             ; CLEAR THE "PUSH DE"
        JMP     RUNTSL

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
; THE OUTPUT IS TO USE THE UP-ARROW CHARACTER FOLLOWED BY A LETTER. |L
; MEANS FF, |I MEANS HT, |G MEANS BELL ETC.
;
; A (CRLF) IS GENERATED AFTER THE ENTIRE LIST HAS BEEN PRINTED OR IF
; THE LIST IS A NULL LIST. HOWEVER, IF THE LIST ENDS WITH COMMA, NO
; (CRLF) IS GENERATED.
;
LIST    CALL    TSTNUM          ; TEST IF THERE IS A #
        PUSH    H
        LXI     H,0FFFFH
        TSTC    ',',LS1
        CALL    TSTNUM
LS1     XTHL
        CALL    ENDCHK          ; IF NO # WE GET A 0
        CALL    FNDLN           ; FINDS THIS OR NEXT LINE
LS2     JC      RSTART          ; C:PASSED TXTUNF
        XTHL
        MOV     A,H
        ORA     L
        JZ      RSTART
        DCX     H
        XTHL
        CALL    PRTLN           ; PRINT THE LINE
        CALL    PRTSTG
        CALL    CHKIO
        CALL    FNDLP           ; FIND NEXT LINE
        JMP     LS2             ; AND LOOP BACK
;
PRINT   MVI     C,8             ; C= # OF SPACES
        TSTC    ';',PR1         ; IF NULL LIST & ";"
        CALL    CRLF            ; GIVE CR-LF AND
        JMP     RUNSML          ; CONTINUE SAME LINE
PR1     TSTC    ATCR,PR6        ; IF NULL LIST (CR)
        CALL    CRLF            ; ALSO GIVE CR-LF AND
        JMP     RUNNXL          ; GO TO NEXT LINE
PR2     TSTC    '#',PR4         ; ELSE IS IT FORMAT?
PR3     CALL    EXPR            ; YES, EVALUATE EXPR.
        MVI     A,0C0H
        ANA     L
        ORA     H
        JNZ     QHOW

