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

; Macro for character testng used by parser.
; See comments in routine TSTCH.
; Source was not shown in original Tiny basic article.

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
        MOV     C,L             ; AND SAVE IT IN C
        JMP     PR5             ; LOOK FOR MORE TO PRINT
PR4     CALL    QTSTG           ; OR IS IT A STRING?
        JMP     PR9             ; IF NOT, MUST BE EXPR.
PR5     TSTC    ',',PR8         ; IF ",", GO FIND NEXT
PR6     TSTC    '.',PR7
        MVI     A,' '
        CALL    OUTCH
        JMP     PR6
PR7     CALL    FIN             ; IN THE LIST
        JMP     PR2             ; LIST CONTINUES
PR8     CALL    CRLF            ; LIST ENDS
        JMP     FINISH
PR9     CALL    EXPR            ; EVALUATE THE EXPR
        PUSH    B
        CALL    PRTNUM          ; PRINT THE VALUE
        POP     B
        JMP     PR5             ; MORE TO PRINT?
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
        PUSH    D               ; AND TEXT POINTER
        CALL    FNDLN           ; FIND THE TARGET LINE
        JNZ     AHOW            ; NOT THERE. SAY "HOW?"
        LHLD    CURRNT          ; SAVE OLD
        PUSH    H               ; 'CURRNT' OLD 'STKGCS'
        LHLD    STKGOS
        PUSH    H
        LXI     H,0             ; AND LOAD NEW ONES
        SHLD    LOPVAR
        DAD     SP
        SHLD    STKGOS
        JMP     RUNTSL          ; THEN RUN THAT LINE
RETURN  CALL    ENDCHK          ; THERE MUST BE A CR
        LHLD    STKGOS          ; OLD STACK POINTER
        MOV     A,H             ; 0 MEANS NOT EXIST
        ORA     L
        JZ      CWHAT           ; SO WE SAY "WHAT?"
        SPHL                    ; ELSE, RESTORE IT
RESTOR  POP     H
        SHLD    STKGOS          ; AND THE OLD 'STKGOS'
        POP     H
        SHLD    CURRNT          ; AND THE OLD 'CURRNT'
        POP     D               ; OLD TEXT POINTER
        CALL    POPA            ; OLD "FOR" PARAMETERS
        JMP     FINISH
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
        DCX     H               ; HL IS ITS ADDRESS
        SHLD    LOPVAR          ; SAVE THAT
        LXI     H,TAB4-1        ; USE 'EXEC' TO LOOK
        JMP     EXEC            ; FOR THE WORD 'TO'
FR1     CALL    EXPR            ; EVALUATE THE LIMIT
        SHLD    LOPLMT          ; SAVE THAT
        LXI     H,TAB5-1        ; USE 'EXEC' TO LOOK
        JMP     EXEC            ; FOR THE WORD 'STEP'
FR2     CALL    EXPR            ; FOUND IT, GET STEP
        JMP     FR4
FR3     LXI     H,1             ; NOT FOUND, SET TO 1
FR4     SHLD    LOPINC          ; SAVE THAT TOO
        LHLD    CURRNT          ; SAVE CURRENT LINE #
        SHLD    LOPLN
        XCHG                    ; AND TEXT POINTER
        SHLD    LOPPT
        LXI     B,10            ; DIG INTO STACK TO
        LHLD    LOPVAR          ; FIND 'LOPVAR'
        XCHG
        MOV     H,B
        MOV     L,B             ; HL=0 NOW
        DAD     SP              ; HERE IS THE STACK
        JMP     FR6
FR5     DAD     B               ; EACH LEVEL IS 10 DEEP
FR6     MOV     A,M             ; GET THAT OLD 'LOPVAR'
        INX     H
        ORA     M
        JZ      FR7             ; 0 SAYS NO MORE IN IT
        MOV     A,M
        DCX     H
        CMP     D               ; SAME AS THIS ONE?
        JNZ     FR5
        MOV     A,M             ; THE OTHER HALF?
        CMP     E
        JNZ     FR5
        XCHG                    ; YES, FOUND ONE
        LXI     H,0
        DAD     SP              ; TRY TO MOVE SP
        MOV     B,H
        MOV     C,L
        LXI     H,10
        DAD     D
        CALL    MVDOWN          ; AND PURGE 10 WORDS
        SPHL                    ; IN THE STACK
FR7     LHLD    LOPPT           ; JOB DONE, RESTORE DE
        XCHG
        JMP     FINISH          ; AND CONTINUE
;
NEXT    CALL    TSTV            ; GET ACCESS OF VAR.
        JC      QWHAT           ; NO VARIABLE, "WHAT?"
        SHLD    VARNXT          ; YES, SAVE IT
NX1     PUSH    D               ; SAVE TEXT POINTER
        XCHG
        LHLD    LOPVAR          ; GET VAR. IN 'FOR'
        MOV     A,H
        ORA     L               ; 0 SAYS NEVER HAD ONE
        JZ      AWHAT           ; SO WE ASK: "WHAT?"
        CALL    COMP            ; ELSE WE CHECK THEM
        JZ      NX2             ; OK, THEY AGREE
        POP     D               ; NO, LET'S SEE
        CALL    POPA            ; PURGE CURRENT LOOP
        LHLD    VARNXT          ; AND POP ONE LEVEL
        JMP     NX1             ; GO CHECK AGAIN
NX2     MOV     E,M             ; COME HERE WHEN AGREED
        INX     H
        MOV     D,M             ; DE=VALUE OF VAR.
        LHLD    LOPINC
        PUSH    H
        MOV     A,H
        XRA     D               ; S=SIGN DIFFER
        MOV     A,D             ; A=SIGN OF DE
        DAD     D               ; ADD ONE STEP
        JM      NX3             ; CANNOT OVERFLOW
        XRA     H               ; MAY OVERFLOW
        JM      NX5             ; AND IT DID
NX3     XCHG
        LHLD    LOPVAR          ; PUT IT BACK
        MOV     M,E
        INX     H
        MOV     M,D
        LHLD    LOPLMT          ; HL=LIMIT
        POP     PSW             ; OLD HL
        ORA     A
        JP      NX4             ; STEP > 0
        XCHG                    ; STEP < 0
NX4     CALL    CKHLDE          ; COMPARE WITH LIMIT
        POP     D               ; RESTORE TEXT POINTER
        JC      NX6             ; OUTSIDE LIMIT
        LHLD    LOPLN           ; WITHIN LIMIT, GO
        SHLD    CURRNT          ; BACK TO THE SAVED
        LHLD    LOPPT           ; 'CURRNT' AND TEXT
        XCHG                    ; POINTER
        JMP     FINISH
NX5     POP     H               ; OVERFLOW, PURGE
        POP     D               ; GARBAGE IN STACK
NX6     CALL    POPA            ; PURGE THIS LOOP
        JMP     FINISH
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
