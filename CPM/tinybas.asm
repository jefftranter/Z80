;**************************************************************
;* 
;*                TINY BASIC FOR INTEL 8080
;*                      VERSION 1.0
;*                    BY LI-CHEN WANG
;*                     10 JUNE, 1976 
;*                       @COPYLEFT 
;*                  ALL WRONGS RESERVED
;* 
;**************************************************************
;* 
;*  ;*** ZERO PAGE SUBROUTINES ***
;* 
;*  THE 8080 INSTRUCTION SET LETS YOU HAVE 8 ROUTINES IN LOW 
;*  MEMORY THAT MAY BE CALLED BY RST N, N BEING 0 THROUGH 7. 
;*  THIS IS A ONE BYTE INSTRUCTION AND HAS THE SAME POWER AS 
;*  THE THREE BYTE INSTRUCTION CALL LLHH.  TINY BASIC WILL 
;*  USE RST 0 AS START AND RST 1 THROUGH RST 7 FOR 
;*  THE SEVEN MOST FREQUENTLY USED SUBROUTINES.
;*  TWO OTHER SUBROUTINES (CRLF AND TSTNUM) ARE ALSO IN THIS 
;*  SECTION.  THEY CAN BE REACHED ONLY BY 3-BYTE CALLS.
;*  IN ORDER TO CONFIGURE THE SYSTEM FOR USE WITH CPM I HAVE
;*  MOVED SOME OF THE ROUTINES AROUND.  START WILL NOW BE AT
;*  LOCATION 100H AND THIS SECTION WILL END AT LOCATION 3FH
;*  WITH A JUMP TO 108H.
;* 
;       ORG  8H
;       XTHL           ;*** TSTC OR RST 1 *** 
;       RST  5         ;IGNORE BLANKS AND 
;       CMP  M         ;TEST CHARACTER
;       JMP  TC1       ;REST OF THIS IS AT TC1
;* 
;CRLF   MVI  A,0DH     ;*** CRLF ***
;* 
;       PUSH PSW       ;*** OUTC OR RST 2 *** 
;       LDA  OCSW      ;PRINT CHARACTER ONLY
;       ORA  A         ;IFF OCSW SWITCH IS ON
;       JMP  OC2       ;REST OF THIS IS AT OC2
;* 
;       CALL EXPR2     ;*** EXPR OR RST 3 *** 
;       PUSH H         ;EVALUATE AN EXPRESION 
;       JMP  EXPR1     ;REST OF IT IS AT EXPR1
;       DB   'W' 
;* 
;       MOV  A,H       ;*** COMP OR RST 4 *** 
;       CMP  D         ;COMPARE HL WITH DE
;       RNZ            ;RETURN CORRECT C AND
;       MOV  A,L       ;Z FLAGS 
;       CMP  E         ;BUT OLD A IS LOST 
;       RET
;       DB   'AN'
;* 
;SS1    LDAX D         ;*** IGNBLK/RST 5 ***
;       CPI  40Q       ;IGNORE BLANKS 
;       RNZ            ;IN TEXT (WHERE DE->)
;       INX  D         ;AND RETURN THE FIRST
;       JMP  SS1       ;NON-BLANK CHAR. IN A
;* 
;       POP  PSW       ;*** FINISH/RST 6 ***
;       CALL FIN       ;CHECK END OF COMMAND
;       JMP  QWHAT     ;PRINT "WHAT?" IFF WRONG
;       DB   'G' 
;* 
;       RST  5         ;*** TSTV OR RST 7 *** 
;       SUI  100Q      ;TEST VARIABLES
;       RC             ;C:NOT A VARIABLE
;       JMP  TSTV1     ;JUMP AROUND RESERVED AREA
       CPU  8080
       ORG  100H      ;OF CPM.
START  JMP  NINIT      ;GO TO INITIALIZATION ROUTINE.	JIF
TSTV1  JNZ  TV1       ;NOT "@" ARRAY 
       INX  D         ;IT IS THE "@" ARRAY 
       CALL PARN      ;@ SHOULD BE FOLLOWED
       DAD  H         ;BY (EXPR) AS ITS INDEX
       JC   QHOW      ;IS INDEX TOO BIG? 
       PUSH D         ;WILL IT OVERWRITE 
       XCHG           ;TEXT? 
       CALL SIZE      ;FIND SIZE OF FREE 
       RST  4         ;AND CHECK THAT
       JC   ASORRY    ;IFF SO, SAY "SORRY"
SS1A   LXI  H,VARBGN  ;IFF NOT, GET ADDRESS 
       CALL SUBDE     ;OF @(EXPR) AND PUT IT 
       POP  D         ;IN HL 
       RET            ;C FLAG IS CLEARED 
TV1    CPI  33Q       ;NOT @, IS IT A TO Z?
       CMC            ;IFF NOT RETURN C FLAG
       RC 
       INX  D         ;IFF A THROUGH Z
TV1A   LXI  H,VARBGN  ;COMPUTE ADDRESS OF
       RLC            ;THAT VARIABLE 
       ADD  L         ;AND RETURN IT IN HL 
       MOV  L,A       ;WITH C FLAG CLEARED 
       MVI  A,0 
       ADC  H 
       MOV  H,A 
       RET
;* 
;*                 TSTC   XCH  HL,(SP)   ;*** TSTC OR RST 1 *** 
;*                        IGNBLK         THIS IS AT LOC. 8 
;*                        CMP  M         AND THEN JMP HERE 
TC1    INX  H         ;COMPARE THE BYTE THAT 
       JZ   TC2       ;FOLLOWS THE RST INST. 
       PUSH B         ;WITH THE TEXT (DE->)
       MOV  C,M       ;IFF NOT =, ADD THE 2ND 
       MVI  B,0       ;BYTE THAT FOLLOWS THE 
       DAD  B         ;RST TO THE OLD PC 
       POP  B         ;I.E., DO A RELATIVE 
       DCX  D         ;JUMP IFF NOT = 
TC2    INX  D         ;IFF =, SKIP THOSE BYTES
       INX  H         ;AND CONTINUE
       XTHL 
       RET
;* 
TSTNUM LXI  H,0       ;*** TSTNUM ***
       MOV  B,H       ;TEST IFF THE TEXT IS 
       RST  5         ;A NUMBER
TN1    CPI  60Q       ;IFF NOT, RETURN 0 IN 
       RC             ;B AND HL
       CPI  72Q       ;IFF NUMBERS, CONVERT 
       RNC            ;TO BINARY IN HL AND 
       MVI  A,360Q    ;SET A TO # OF DIGITS
       ANA  H         ;IFF H>255, THERE IS NO 
       JNZ  QHOW      ;ROOM FOR NEXT DIGIT 
       INR  B         ;B COUNTS # OF DIGITS
       PUSH B 
       MOV  B,H       ;HL=10;*HL+(NEW DIGIT)
       MOV  C,L 
       DAD  H         ;WHERE 10;* IS DONE BY
       DAD  H         ;SHIFT AND ADD 
       DAD  B 
       DAD  H 
       LDAX D         ;AND (DIGIT) IS FROM 
       INX  D         ;STRIPPING THE ASCII 
       ANI  17Q       ;CODE
       ADD  L 
       MOV  L,A 
       MVI  A,0 
       ADC  H 
       MOV  H,A 
       POP  B 
       LDAX D         ;DO THIS DIGIT AFTER 
       JP   TN1       ;DIGIT. S SAYS OVERFLOW
QHOW   PUSH D         ;*** ERROR: "HOW?" *** 
AHOW   LXI  D,HOW 
       JMP  ERROR 
HOW    DB   "HOW?",0DH 
OK     DB   "OK",0DH 
WHAT   DB   "WHAT?",0DH 
SORRY  DB   "SORRY",0DH 
;* 
;**************************************************************
;* 
;* *** MAIN ***
;* 
;* THIS IS THE MAIN LOOP THAT COLLECTS THE TINY BASIC PROGRAM
;* AND STORES IT IN THE MEMORY.
;* 
;* AT START, IT PRINTS OUT "(CR)OK(CR)", AND INITIALIZES THE 
;* STACK AND SOME OTHER INTERNAL VARIABLES.  THEN IT PROMPTS 
;* ">" AND READS A LINE.  IFF THE LINE STARTS WITH A NON-ZERO 
;* NUMBER, THIS NUMBER IS THE LINE NUMBER.  THE LINE NUMBER
;* (IN 16 BIT BINARY) AND THE REST OF THE LINE (INCLUDING CR)
;* IS STORED IN THE MEMORY.  IFF A LINE WITH THE SAME LINE
;* NUMBER IS ALREDY THERE, IT IS REPLACED BY THE NEW ONE.  IF
;* THE REST OF THE LINE CONSISTS OF A 0DHONLY, IT IS NOT STORED
;* AND ANY EXISTING LINE WITH THE SAME LINE NUMBER IS DELETED. 
;* 
;* AFTER A LINE ISs INSERTED, REPLACED, OR DELETED, THE PROGRAM 
;* LOOPS BACK AND ASK FOR ANOTHER LINE.  THIS LOOP WILL BE 
;* TERMINATED WHEN IT READS A LINE WITH ZERO OR NO LINE
;* NUMBER; AND CONTROL IS TRANSFERED TO "DIRCT".
;* 
;* TINY BASIC PROGRAM SAVE AREA STARTS AT THE MEMORY LOCATION
;* LABELED "TXTBGN" AND ENDED AT "TXTEND".  WE ALWAYS FILL THIS
;* AREA STARTING AT "TXTBGN", THE UNFILLED PORTION IS POINTED
;* BY THE CONTENT OF A MEMORY LOCATION LABELED "TXTUNF". 
;* 
;* THE MEMORY LOCATION "CURRNT" POINTS TO THE LINE NUMBER
;* THAT IS CURRENTLY BEING INTERPRETED.  WHILE WE ARE IN 
;* THIS LOOP OR WHILE WE ARE INTERPRETING A DIRECT COMMAND 
;* (SEE NEXT SECTION), "CURRNT" SHOULD POINT TO A 0. 
;* 
RSTART LXI  SP,STACK  ;SET STACK POINTER
ST1    CALL CRLF      ;AND JUMP TO HERE
       LXI  D,OK      ;DE->STRING
       SUB  A         ;A=0 
       CALL PRTSTG    ;PRINT STRING UNTIL 0DH
       LXI  H,ST2+1   ;LITERAL 0 
       SHLD CURRNT    ;CURRNT->LINE # = 0
ST2    LXI  H,0 
       SHLD LOPVAR
       SHLD STKGOS
ST3    MVI  A,76Q     ;PROMPT '>' AND
       CALL GETLN     ;READ A LINE 
       PUSH D         ;DE->END OF LINE 
ST3A   LXI  D,BUFFER  ;DE->BEGINNING OF LINE 
       CALL TSTNUM    ;TESt IFF IT IS A NUMBER
       RST  5 
       MOV  A,H       ;HL=VALUE OF THE # OR
       ORA  L         ;0 IFF NO # WAS FOUND 
       POP  B         ;BC->END OF LINE 
       JZ   DIRECT
       DCX  D         ;BACKUP DE AND SAVE
       MOV  A,H       ;VALUE OF LINE # THERE 
       STAX D 
       DCX  D 
       MOV  A,L 
       STAX D 
       PUSH B         ;BC,DE->BEGIN, END 
       PUSH D 
       MOV  A,C 
       SUB  E 
       PUSH PSW       ;A=# OF BYTES IN LINE
       CALL FNDLN     ;FIND THIS LINE IN SAVE
       PUSH D         ;AREA, DE->SAVE AREA 
       JNZ  ST4       ;NZ:NOT FOUND, INSERT
       PUSH D         ;Z:FOUND, DELETE IT
       CALL FNDNXT    ;FIND NEXT LINE
;*                                       DE->NEXT LINE 
       POP  B         ;BC->LINE TO BE DELETED
       LHLD TXTUNF    ;HL->UNFILLED SAVE AREA
       CALL MVUP      ;MOVE UP TO DELETE 
       MOV  H,B       ;TXTUNF->UNFILLED AREA 
       MOV  L,C 
       SHLD TXTUNF    ;UPDATE
ST4    POP  B         ;GET READY TO INSERT 
       LHLD TXTUNF    ;BUT FIRT CHECK IF
       POP  PSW       ;THE LENGTH OF NEW LINE
       PUSH H         ;IS 3 (LINE # AND CR)
       CPI  3         ;THEN DO NOT INSERT
       JZ   RSTART    ;MUST CLEAR THE STACK
       ADD  L         ;COMPUTE NEW TXTUNF
       MOV  L,A 
       MVI  A,0 
       ADC  H 
       MOV  H,A       ;HL->NEW UNFILLED AREA 
ST4A   LXI  D,TXTEND  ;CHECK TO SEE IF THERE 
       RST  4         ;IS ENOUGH SPACE 
       JNC  QSORRY    ;SORRY, NO ROOM FOR IT 
       SHLD TXTUNF    ;OK, UPDATE TXTUNF 
       POP  D         ;DE->OLD UNFILLED AREA 
       CALL MVDOWN
       POP  D         ;DE->BEGIN, HL->END
       POP  H 
       CALL MVUP      ;MOVE NEW LINE TO SAVE 
       JMP  ST3       ;AREA
;* 
;**************************************************************
;* 
;* *** TABLES *** DIRECT *** & EXEC ***
;* 
;* THIS SECTION OF THE CODE TESTS A STRING AGAINST A TABLE.
;* WHEN A MATCH IS FOUND, CONTROL IS TRANSFERED TO THE SECTION 
;* OF CODE ACCORDING TO THE TABLE. 
;* 
;* AT 'EXEC', DE SHOULD POINT TO THE STRING AD HL SHOULD POINT
;* TO THE TABLE-1.  AT 'DIRECT', DE SHOULD POINT TO THE STRING,
;* HL WILL BE SET UP TO POINT TO TAB1-1, WHICH IS THE TABLE OF 
;* ALL DIRECT AND STATEMENT COMMANDS.
;* 
;* A '.' IN THE STRING WILL TERMINATE THE TEST AND THE PARTIAL 
;* MATCH WILL BE CONSIDERED AS A MATCH.  E.G., 'P.', 'PR.',
;* 'PRI.', 'PRIN.', OR 'PRINT' WILL ALL MATCH 'PRINT'. 
;* 
;* THE TABLE CONSISTS OF ANY NUMBER OF ITEMS.  EACH ITEM 
;* IS A STRING OF CHARACTERS WITH BIT 7 SET TO 0 AND 
;* A JUMP ADDRESS STORED HI-LOW WITH BIT 7 OF THE HIGH 
;* BYTE SET TO 1.
;* 
;* END OF TABLE IS AN ITEM WITH A JUMP ADDRESS ONLY.  IFF THE 
;* STRING DOES NOT MATCH ANY OF THE OTHER ITEMS, IT WILL 
;* MATCH THIS NULL ITEM AS DEFAULT.
;* 
TAB1   EQU  $         ;DIRECT COMMANDS 
       DB   "LIST"
       DB   LIST >> 8 + 128,LIST & 0FFH
       DB   "RUN"
       DB   RUN >> 8 + 128,RUN & 255
       DB   "NEW"
       DB   NEW >> 8 + 128,NEW & 255
       DB   "LOAD"
       DB   DLOAD >> 8 + 128,DLOAD & 255
       DB   "SAVE"
       DB   DSAVE >> 8 + 128,DSAVE & 255
       DB   "BYE",80H,0H   ;GO BACK TO CPM
TAB2   EQU  $         ;DIRECT/TATEMENT
       DB   "NEXT"
       DB   NEXT >> 8 + 128,NEXT & 255
       DB   "LET"
       DB   LET >> 8 + 128,LET & 255
       DB   "OUT"
       DB   OUTCMD >> 8 + 128,OUTCMD & 255 
       DB   "POKE"
       DB   POKE >> 8 + 128,POKE & 255
       DB   "WAIT"
       DB   WAITCM >> 8 + 128,WAITCM & 255
       DB   "IF"
       DB   IFF >> 8 + 128,IFF & 255
       DB   "GOTO"
       DB   GOTO >> 8 + 128,GOTO & 255
       DB   "GOSUB"
       DB   GOSUB >> 8 + 128,GOSUB & 255
       DB   "RETURN"
       DB   RETURN >> 8 + 128,RETURN & 255
       DB   "REM"
       DB   REM >> 8 + 128,REM & 255
       DB   "FOR"
       DB   FOR >> 8 + 128,FOR & 255
       DB   "INPUT"
       DB   INPUT >> 8 + 128,INPUT & 255
       DB   "PRINT"
       DB   PRINT >> 8 + 128,PRINT & 255
       DB   "STOP"
       DB   STOP >> 8 + 128,STOP & 255
       DB   DEFLT >> 8 + 128,DEFLT & 255
       DB   "YOU CAN ADD MORE" ;COMMANDS BUT
            ;REMEMBER TO MOVE DEFAULT DOWN.
TAB4   EQU  $         ;FUNCTIONS 
       DB   "RND"
       DB   RND >> 8 + 128,RND & 255
       DB   "INP"
       DB   INP >> 8 + 128,INP & 255
       DB   "PEEK"
       DB   PEEK >> 8 + 128,PEEK & 255
       DB   "USR"
       DB   USR >> 8 + 128,USR & 255
       DB   "ABS"
       DB   ABS >> 8 + 128,ABS & 255
       DB   "SIZE"
       DB   SIZE >> 8 + 128,SIZE & 255
       DB   XP40 >> 8 + 128,XP40 & 255
       DB   "YOU CAN ADD MORE" ;FUNCTIONS BUT REMEMBER
                      ;TO MOVE XP40 DOWN
TAB5   EQU  $         ;"TO" IN "FOR" 
       DB   "TO"
       DB   FR1 >> 8 + 128,FR1 & 255
       DB   QWHAT >> 8 + 128,QWHAT & 255
TAB6   EQU  $         ;"STEP" IN "FOR" 
       DB   "STEP"
       DB   FR2 >> 8 + 128,FR2 & 255
       DB   FR3 >> 8 + 128,FR3 & 255
TAB8   EQU  $         ;RELATION OPERATORS
       DB   ">="
       DB   XP11 >> 8 + 128,XP11 & 255
       DB   '#'
       DB   XP12 >> 8 + 128,XP12 & 255
       DB   '>'
       DB   XP13 >> 8 + 128,XP13 & 255
       DB   '='
       DB   XP15 >> 8 + 128,XP15 & 255
       DB   "<="
       DB   XP14 >> 8 + 128,XP14 & 255
       DB   '<'
       DB   XP16 >> 8 + 128,XP16 & 255
       DB   XP17 >> 8 + 128,XP17 & 255
;* 
DIRECT LXI  H,TAB1-1  ;*** DIRECT ***
;* 
EXEC   EQU  $         ;*** EXEC ***
EX0    RST  5         ;IGNORE LEADING BLANKS 
       PUSH D         ;SAVE POINTER
EX1    LDAX D         ;IFF FOUND '.' IN STRING
       INX  D         ;BEFORE ANY MISMATCH 
       CPI  56Q       ;WE DECLARE A MATCH
       JZ   EX3 
       INX  H         ;HL->TABLE 
       CMP  M         ;IFF MATCH, TEST NEXT 
       JZ   EX1 
       MVI  A,177Q    ;ELSE, SEE IFF BIT 7
       DCX  D         ;OF TABLEIS SET, WHICH
       CMP  M         ;IS THE JUMP ADDR. (HI)
       JC   EX5       ;C:YES, MATCHED
EX2    INX  H         ;NC:NO, FIND JUMP ADDR.
       CMP  M 
       JNC  EX2 
       INX  H         ;BUMP TO NEXT TAB. ITEM
       POP  D         ;RESTORE STRING POINTER
       JMP  EX0       ;TEST AGAINST NEXT ITEM
EX3    MVI  A,177Q    ;PARTIAL MATCH, FIND 
EX4    INX  H         ;JUMP ADDR., WHICH IS
       CMP  M         ;FLAGGED BY BIT 7
       JNC  EX4 
EX5    MOV  A,M       ;LOAD HL WITH THE JUMP 
       INX  H         ;ADDRESS FROM THE TABLE
       MOV  L,M 
       ANI  177Q      ;MASK OFF BIT 7
       MOV  H,A 
       POP  PSW       ;CLEAN UP THE GABAGE 
       PCHL           ;AND WE GO DO IT 
;* 
;**************************************************************
;* 
;* WHAT FOLLOWS IS THE CODE TO EXECUTE DIRECT AND STATEMENT
;* COMMANDS.  CONTROL IS TRANSFERED TO THESE POINTS VIA THE
;* COMMAND TABLE LOOKUP CODE OF 'DIRECT' AND 'EXEC' IN LAST
;* SECTION.  AFTER THE COMMAND IS EXECUTED, CONTROL IS 
;* TANSFERED TO OTHER SECTIONS AS FOLLOWS:
;* 
;* FOR 'LIST', 'NEW', AND 'STOP': GO BACK TO 'RSTART'
;* FOR 'RUN': GO EXECUTE THE FIRST STORED LINE IFF ANY; ELSE
;* GO BACK TO 'RSTART'.
;* FOR 'GOTO' AND 'GOSUB': GO EXECUTE THE TARGET LINE. 
;* FOR 'RETURN' AND 'NEXT': GO BACK TO SAVED RETURN LINE.
;* FOR ALL OTHERS: IFF 'CURRNT' -> 0, GO TO 'RSTART', ELSE
;* GO EXECUTE NEXT COMMAND.  (THIS IS DONE IN 'FINISH'.) 
;* 
;**************************************************************
;* 
;* *** NEW *** STOP *** RUN (& FRIENDS) *** & GOTO *** 
;* 
;* 'NEW(CR)' SETS 'TXTUNF' TO POINT TO 'TXTBGN'
;* 
;* 'STOP(CR)' GOES BACK TO 'RSTART'
;* 
;* 'RUN(CR)' FINDS THE FIRST STORED LINE, STORE ITS ADDRESS (IN
;* 'CURRNT'), AND START EXECUTE IT.  NOTE THAT ONLY THOSE
;* COMMANDS IN TAB2 ARE LEGAL FOR STORED PROGRAM.
;* 
;* THERE ARE 3 MORE ENTRIES IN 'RUN':
;* 'RUNNXL' FINDS NEXT LINE, STORES ITS ADDR. AND EXECUTES IT. 
;* 'RUNTSL' STORES THE ADDRESS OF THIS LINE AND EXECUTES IT. 
;* 'RUNSML' CONTINUES THE EXECUTION ON SAME LINE.
;* 
;* 'GOTO EXPR(CR)' EVALUATES THE EXPRESSION, FIND THE TARGET 
;* LINE, AND JUMP TO 'RUNTSL' TO DO IT.
;* 'DLOAD' LOADS A NAMED PROGRAM FROM DISK.
;* 'DSAVE' SAVES A NAMED PROGRAM ON DISK.
;* 'FCBSET' SETS UP THE FILE CONTROL BLOCK FOR SUBSEQUENT DISK I/O.
;* 
NEW    CALL ENDCHK    ;*** NEW(CR) *** 
       LXI  H,TXTBGN
       SHLD TXTUNF
;* 
STOP   CALL ENDCHK    ;*** STOP(CR) ***
       JMP RSTART
;* 
RUN    CALL ENDCHK    ;*** RUN(CR) *** 
       LXI  D,TXTBGN  ;FIRST SAVED LINE
;* 
RUNNXL LXI  H,0       ;*** RUNNXL ***
       CALL FNDLNP    ;FIND WHATEVER LINE #
       JC   RSTART    ;C:PASSED TXTUNF, QUIT 
;* 
RUNTSL XCHG           ;*** RUNTSL ***
       SHLD CURRNT    ;SET 'CURRNT'->LINE #
       XCHG 
       INX  D         ;BUMP PASS LINE #
       INX  D 
;* 
RUNSML CALL CHKIO     ;*** RUNSML ***
       LXI  H,TAB2-1  ;FIND COMMAND IN TAB2
       JMP  EXEC      ;AND EXECUTE IT
;* 
GOTO   RST  3         ;*** GOTO EXPR *** 
       PUSH D         ;SAVE FOR ERROR ROUTINE
       CALL ENDCHK    ;MUST FIND A 0DH
       CALL FNDLN     ;FIND THE TARGET LINE
       JNZ  AHOW      ;NO SUCH LINE #
       POP  PSW       ;CLEAR THE "PUSH DE" 
       JMP  RUNTSL    ;GO DO IT
CPM    EQU  5         ;DISK PARAMETERS
FCB    EQU  5CH
SETDMA EQU  26
OPEN   EQU  15
READD  EQU  20
WRITED EQU  21
CLOSE  EQU  16
MAKE   EQU  22
DELETE EQU  19
;*
DLOAD  RST  5         ;IGNORE BLANKS
       PUSH H         ;SAVE H
       CALL FCBSET    ;SET UP FILE CONTROL BLOCK
       PUSH D         ;SAVE THE REST
       PUSH B
       LXI  D,FCB     ;GET FCB ADDRESS
       MVI  C,OPEN    ;PREPARE TO OPEN FILE
       CALL CPM       ;OPEN IT
       CPI  0FFH      ;IS IT THERE?
       JZ   QHOW      ;NO, SEND ERROR
       XRA  A         ;CLEAR A
       STA  FCB+32    ;START AT RECORD 0
       LXI  D,TXTUNF  ;GET BEGINNING
LOAD   PUSH D         ;SAVE DMA ADDRESS
       MVI  C,SETDMA  ;
       CALL CPM       ;SET DMA ADDRESS
       MVI  C,READD   ;
       LXI  D,FCB
       CALL CPM       ;READ SECTOR
       CPI  1         ;DONE?
       JC   RDMORE    ;NO, READ MORE
       JNZ  QHOW      ;BAD READ
       MVI  C,CLOSE
       LXI  D,FCB 
       CALL CPM       ;CLOSE FILE
       POP  D         ;THROW AWAY DMA ADD.
       POP  B         ;GET OLD REGISTERS BACK
       POP  D
       POP  H
       RST  6         ;FINISH
RDMORE POP  D         ;GET DMA ADDRESS
       LXI  H,80H     ;GET 128
       DAD  D         ;ADD 128 TO DMA ADD.
       XCHG           ;PUT IT BACK IN D
       JMP  LOAD      ;AND READ SOME MORE
;*
DSAVE  RST  5         ;IGNORE BLANKS
       PUSH H         ;SAVE H
       CALL FCBSET    ;SETUP FCB
       PUSH D
       PUSH B         ;SAVE OTHERS
       LXI  D,FCB
       MVI  C,DELETE
       CALL CPM       ;ERASE FILE IF IT EXISTS
       LXI  D,FCB
       MVI  C,MAKE
       CALL CPM       ;MAKE A NEW ONE
       CPI  0FFH      ;IS THERE SPACE?
       JZ   QHOW      ;NO, ERROR
       XRA  A         ;CLEAR A
       STA  FCB+32    ;START AT RECORD 0
       LXI  D,TXTUNF  ;GET BEGINNING
SAVE   PUSH D         ;SAVE DMA ADDRESS
       MVI  C,SETDMA  ;
       CALL CPM       ;SET DMA ADDRESS
       MVI  C,WRITED
       LXI  D,FCB 
       CALL CPM       ;WRITE SECTOR
       ORA  A         ;SET FLAGS
       JNZ  QHOW      ;IF NOT ZERO, ERROR
       POP  D         ;GET DMA ADD. BACK
       LDA  TXTUNF+1  ;AND MSB OF LAST ADD.
       CMP  D         ;IS D SMALLER?
       JC   SAVDON    ;YES, DONE
       JNZ  WRITMOR   ;DONT TEST E IF NOT EQUAL
       LDA  TXTUNF    ;IS E SMALLER?
       CMP  E
       JC   SAVDON    ;YES, DONE
WRITMOR LXI  H,80H 
       DAD  D         ;ADD 128 TO DMA ADD.
       XCHG           ;GET IT BACK IN D
       JMP  SAVE      ;WRITE SOME MORE
SAVDON MVI  C,CLOSE
       LXI  D,FCB 
       CALL CPM       ;CLOSE FILE
       POP  B         ;GET REGISTERS BACK
       POP  D
       POP  H
       RST  6         ;FINISH
;*
FCBSET LXI  H,FCB     ;GET FILE CONTROL BLOCK ADDRESS
       MVI  M,0       ;CLEAR ENTRY TYPE
FNCLR  INX  H         ;NEXT LOCATION
       MVI  M,' '     ;CLEAR TO SPACE
       MVI  A,FCB+8 & 255
       CMP  L         ;DONE?
       JNZ  FNCLR     ;NO, DO IT AGAIN
       INX  H         ;NEXT
       MVI  M,'T'     ;SET FILE TYPE TO 'TBI'
       INX  H
       MVI  M,'B'
       INX  H
       MVI  M,'I'
EXRC   INX  H         ;CLEAR REST OF FCB
       MVI  M,0
       MVI  A,FCB+15 & 255
       CMP  L         ;DONE?
       JNZ  EXRC      ;NO, CONTINUE
       LXI  H,FCB+1   ;GET FILENAME START
FN     LDAX D         ;GET CHARACTER
       CPI  0DH       ;IS IT A 'CR'
       RZ             ;YES, DONE
       CPI  '!'       ;LEGAL CHARACTER?
       JC   QWHAT     ;NO, SEND ERROR
       CPI  '['       ;AGAIN
       JNC  QWHAT     ;DITTO
       MOV  M,A        ;SAVE IT IN FCB
       INX  H         ;NEXT
       INX  D
       MVI  A,FCB+9 & 255
       CMP  L         ;LAST?
       JNZ  FN        ;NO, CONTINUE
       RET            ;TRUNCATE AT 8 CHARACTERS
;* 
;************************************************************* 
;* 
;* *** LIST *** & PRINT ***
;* 
;* LIST HAS TWO FORMS: 
;* 'LIST(CR)' LISTS ALL SAVED LINES
;* 'LIST #(CR)' START LIST AT THIS LINE #
;* YOU CAN STOP THE LISTING BY CONTROL C KEY 
;* 
;* PRINT COMMAND IS 'PRINT ....;' OR 'PRINT ....(CR)'
;* WHERE '....' IS A LIST OF EXPRESIONS, FORMATS, BACK-
;* ARROWS, AND STRINGS.  THESE ITEMS ARE SEPERATED BY COMMAS.
;* 
;* A FORMAT IS A POUND SIGN FOLLOWED BY A NUMBER.  IT CONTROLSs 
;* THE NUMBER OF SPACES THE VALUE OF A EXPRESION IS GOING TO 
;* BE PRINTED.  IT STAYS EFFECTIVE FOR THE REST OF THE PRINT 
;* COMMAND UNLESS CHANGED BY ANOTHER FORMAT.  IFF NO FORMAT IS
;* SPECIFIED, 6 POSITIONS WILL BE USED.
;* 
;* A STRING IS QUOTED IN A PAIR OF SINGLE QUOTES OR A PAIR OF
;* DOUBLE QUOTES.
;* 
;* A BACK-ARROW MEANS GENERATE A (CR) WITHOUT (LF) 
;* 
;* A (CRLF) IS GENERATED AFTER THE ENTIRE LIST HAS BEEN
;* PRINTED OR IFF THE LIST IS A NULL LIST.  HOWEVER IFF THE LIST 
;* ENDED WITH A COMMA, NO (CRL) IS GENERATED. 
;* 
LIST   CALL TSTNUM    ;TEST IFF THERE IS A #
       CALL ENDCHK    ;IFF NO # WE GET A 0
       CALL FNDLN     ;FIND THIS OR NEXT LINE
LS1    JC   RSTART    ;C:PASSED TXTUNF 
       CALL PRTLN     ;PRINT THE LINE
       CALL CHKIO     ;STOP IFF HIT CONTROL-C 
       CALL FNDLNP    ;FIND NEXT LINE
       JMP  LS1       ;AND LOOP BACK 
;* 
PRINT  MVI  C,6       ;C = # OF SPACES 
       RST  1         ;IFF NULL LIST & ";"
       DB   73Q 
       DB   6Q 
       CALL CRLF      ;GIVE CR-LF AND
       JMP  RUNSML    ;CONTINUE SAME LINE
PR2    RST  1         ;IFF NULL LIST (CR) 
       DB   0DH
       DB   6Q
       CALL CRLF      ;ALSO GIVE CR-LF AND 
       JMP  RUNNXL    ;GO TO NEXT LINE 
PR0    RST  1         ;ELSE IS IT FORMAT?
       DB   '#' 
       DB   5Q
       RST  3         ;YES, EVALUATE EXPR. 
       MOV  C,L       ;AND SAVE IT IN C
       JMP  PR3       ;LOOK FOR MORE TO PRINT
PR1    CALL QTSTG     ;OR IS IT A STRING?
       JMP  PR8       ;IFF NOT, MUST BE EXPR. 
PR3    RST  1         ;IFF ",", GO FIND NEXT
       DB   ',' 
       DB   6Q
       CALL FIN       ;IN THE LIST.
       JMP  PR0       ;LIST CONTINUES
PR6    CALL CRLF      ;LIST ENDS 
       RST  6 
PR8    RST  3         ;EVALUATE THE EXPR 
       PUSH B 
       CALL PRTNUM    ;PRINT THE VALUE 
       POP  B 
       JMP  PR3       ;MORE TO PRINT?
;* 
;**************************************************************
;* 
;* *** GOSUB *** & RETURN ***
;* 
;* 'GOSUB EXPR;' OR 'GOSUB EXPR (CR)' IS LIKE THE 'GOTO' 
;* COMMAND, EXCEPT THAT THE CURRENT TEXT POINTER, STACK POINTER
;* ETC. ARE SAVE SO THAT EXECUTION CAN BE CONTINUED AFTER THE
;* SUBROUTINE 'RETURN'.  IN ORDER THAT 'GOSUB' CAN BE NESTED 
;* (AND EVEN RECURSIVE), THE SAVE AREA MUST BE STACKED.
;* THE STACK POINTER IS SAVED IN 'STKGOS'. THE OLD 'STKGOS' IS 
;* SAVED IN THE STACK.  IFF WE ARE IN THE MAIN ROUTINE, 'STKGOS'
;* IS ZERO (THIS WAS DONE BY THE "MAIN" SECTION OF THE CODE),
;* BUT WE STILL SAVE IT AS A FLAG FORr NO FURTHER 'RETURN'S.
;* 
;* 'RETURN(CR)' UNDOS EVERYHING THAT 'GOSUB' DID, AND THUS
;* RETURN THE EXCUTION TO THE COMMAND AFTER THE MOST RECENT
;* 'GOSUB'.  IFF 'STKGOS' IS ZERO, IT INDICATES THAT WE 
;* NEVER HAD A 'GOSUB' AND IS THUS AN ERROR. 
;* 
GOSUB  CALL PUSHA     ;SAVE THE CURRENT "FOR"
       RST  3         ;PARAMETERS
       PUSH D         ;AND TEXT POINTER
       CALL FNDLN     ;FIND THE TARGET LINE
       JNZ  AHOW      ;NOT THERE. SAY "HOW?" 
       LHLD CURRNT    ;FOUND IT, SAVE OLD
       PUSH H         ;'CURRNT' OLD 'STKGOS' 
       LHLD STKGOS
       PUSH H 
       LXI  H,0       ;AND LOAD NEW ONES 
       SHLD LOPVAR
       DAD  SP
       SHLD STKGOS
       JMP  RUNTSL    ;THEN RUN THAT LINE
RETURN CALL ENDCHK    ;THERE MUST BE A 0DH
       LHLD STKGOS    ;OLD STACK POINTER 
       MOV  A,H       ;0 MEANS NOT EXIST 
       ORA  L 
       JZ   QWHAT     ;SO, WE SAY: "WHAT?" 
       SPHL           ;ELSE, RESTORE IT
       POP  H 
       SHLD STKGOS    ;AND THE OLD 'STKGOS'
       POP  H 
       SHLD CURRNT    ;AND THE OLD 'CURRNT'
       POP  D         ;OLD TEXT POINTER
       CALL POPA      ;OLD "FOR" PARAMETERS
       RST  6         ;AND WE ARE BACK HOME
;* 
;**************************************************************
;* 
;* *** FOR *** & NEXT ***
;* 
;* 'FOR' HAS TWO FORMS:
;* 'FOR VAR=EXP1 TO EXP2 STEP EXP1' AND 'FOR VAR=EXP1 TO EXP2' 
;* THE SECOND FORM MEANS THE SAME THING AS THE FIRST FORM WITH 
;* EXP1=1.  (I.E., WITH A STEP OF +1.) 
;* TBI WILL FIND THE VARIABLE VAR. AND SET ITS VALUE TO THE
;* CURRENT VALUE OF EXP1.  IT ALSO EVALUATES EXPR2 AND EXP1
;* AND SAVE ALL THESE TOGETHER WITH THE TEXT POINTERr ETC. IN 
;* THE 'FOR' SAVE AREA, WHICH CONSISTS OF 'LOPVAR', 'LOPINC',
;* 'LOPLMT', 'LOPLN', AND 'LOPPT'.  IFF THERE IS ALREADY SOME-
;* THING IN THE SAVE AREA (THIS IS INDICATED BY A NON-ZERO 
;* 'LOPVAR'), THEN THE OLD SAVE AREA IS SAVED IN THE STACK 
;* BEFORE THE NEW ONE OVERWRITES IT. 
;* TBI WILL THEN DIG IN THE STACK AND FIND OUT IFF THIS SAME
;* VARIABLE WAS USED IN ANOTHER CURRENTLY ACTIVE 'FOR' LOOP. 
;* IFF THAT IS THE CASE THEN THE OLD 'FOR' LOOP IS DEACTIVATED.
;* (PURGED FROM THE STACK..) 
;* 
;* 'NEXT VAR' SERVES AS THE LOGICAL (NOT NECESSARILLY PHYSICAL)
;* END OF THE 'FOR' LOOP.  THE CONTROL VARIABLE VAR. IS CHECKED
;* WITH THE 'LOPVAR'.  IFF THEY ARE NOT THE SAME, TBI DIGS IN 
;* THE STACK TO FIND THE RIGHTt ONE AND PURGES ALL THOSE THAT 
;* DID NOT MATCH.  EITHER WAY, TBI THEN ADDS THE 'STEP' TO 
;* THAT VARIABLE AND CHECK THE RESULT WITH THE LIMIT.  IFF IT 
;* IS WITHIN THE LIMIT, CONTROL LOOPS BACK TO THE COMMAND
;* FOLLOWING THE 'FOR'.  IFF OUTSIDE THE LIMIT, THE SAVE ARER 
;* IS PURGED AND EXECUTION CONTINUES.
;* 
FOR    CALL PUSHA     ;SAVE THE OLD SAVE AREA
       CALL SETVAL    ;SET THE CONTROL VAR.
       DCX  H         ;HL IS ITS ADDRESS 
       SHLD LOPVAR    ;SAVE THAT 
       LXI  H,TAB5-1  ;USE 'EXEC' TO LOOK
       JMP  EXEC      ;FOR THE WORD 'TO' 
FR1    RST  3         ;EVALUATE THE LIMIT
       SHLD LOPLMT    ;SAVE THAT 
       LXI  H,TAB6-1  ;USE 'EXEC' TO LOOK
       JMP  EXEC      ;FOR THE WORD 'STEP'
FR2    RST  3         ;FOUND IT, GET STEP
       JMP  FR4 
FR3    LXI  H,1Q      ;NOT FOUND, SET TO 1 
FR4    SHLD LOPINC    ;SAVE THAT TOO 
FR5    LHLD CURRNT    ;SAVE CURRENT LINE # 
       SHLD LOPLN 
       XCHG           ;AND TEXT POINTER
       SHLD LOPPT 
       LXI  B,12Q     ;DIG INTO STACK TO 
       LHLD LOPVAR    ;FIND 'LOPVAR' 
       XCHG 
       MOV  H,B 
       MOV  L,B       ;HL=0 NOW
       DAD  SP        ;HERE IS THE STACK 
       DB   76Q 
FR7    DAD  B         ;EACH LEVEL IS 10 DEEP 
       MOV  A,M       ;GET THAT OLD 'LOPVAR' 
       INX  H 
       ORA  M 
       JZ   FR8       ;0 SAYS NO MORE IN IT
       MOV  A,M 
       DCX  H 
       CMP  D         ;SAME AS THIS ONE? 
       JNZ  FR7 
       MOV  A,M       ;THE OTHER HALF? 
       CMP  E 
       JNZ  FR7 
       XCHG           ;YES, FOUND ONE
       LXI  H,0Q
       DAD  SP        ;TRY TO MOVE SP
       MOV  B,H 
       MOV  C,L 
       LXI  H,12Q 
       DAD  D 
       CALL MVDOWN    ;AND PURGE 10 WORDS
       SPHL           ;IN THE STACK
FR8    LHLD LOPPT     ;JOB DONE, RESTORE DE
       XCHG 
       RST  6         ;AND CONTINUE
;* 
NEXT   RST  7         ;GET ADDRESS OF VAR. 
       JC   QWHAT     ;NO VARIABLE, "WHAT?"
       SHLD VARNXT    ;YES, SAVE IT
NX0    PUSH D         ;SAVE TEXT POINTER 
       XCHG 
       LHLD LOPVAR    ;GET VAR. IN 'FOR' 
       MOV  A,H 
       ORA  L         ;0 SAYS NEVER HAD ONE
       JZ   AWHAT     ;SO WE ASK: "WHAT?"
       RST  4         ;ELSE WE CHECK THEM
       JZ   NX3       ;OK, THEY AGREE
       POP  D         ;NO, LET'S SEE 
       CALL POPA      ;PURGE CURRENT LOOP
       LHLD VARNXT    ;AND POP ONE LEVEL 
       JMP  NX0       ;GO CHECK AGAIN
NX3    MOV  E,M       ;COME HERE WHEN AGREED 
       INX  H 
       MOV  D,M       ;DE=VALUE OF VAR.
       LHLD LOPINC
       PUSH H 
       DAD  D         ;ADD ONE STEP
       XCHG 
       LHLD LOPVAR    ;PUT IT BACK 
       MOV  M,E 
       INX  H 
       MOV  M,D 
       LHLD LOPLMT    ;HL->LIMIT 
       POP  PSW       ;OLD HL
       ORA  A 
       JP   NX1       ;STEP > 0
       XCHG 
NX1    CALL CKHLDE    ;COMPARE WITH LIMIT
       POP  D         ;RESTORE TEXT POINTER
       JC   NX2       ;OUTSIDE LIMIT 
       LHLD LOPLN     ;WITHIN LIMIT, GO
       SHLD CURRNT    ;BACK TO THE SAVED 
       LHLD LOPPT     ;'CURRNT' AND TEXT 
       XCHG           ;POINTER 
       RST  6 
NX2    CALL POPA      ;PURGE THIS LOOP 
       RST  6 
;* 
;**************************************************************
;* 
;* *** REM *** IFF *** INPUT *** & LET (& DEFLT) ***
;* 
;* 'REM' CAN BE FOLLOWED BY ANYTHING AND IS IGNORED BY TBI.
;* TBI TREATS IT LIKE AN 'IF' WITH A FALSE CONDITION.
;* 
;* 'IF' IS FOLLOWED BY AN EXPR. AS A CONDITION AND ONE OR MORE 
;* COMMANDS (INCLUDING OUTHER 'IF'S) SEPERATED BY SEMI-COLONS. 
;* NOTE THAT THE WORD 'THEN' IS NOT USED.  TBI EVALUATES THE 
;* EXPR. IFF IT IS NON-ZERO, EXECUTION CONTINUES.  IFF THE 
;* EXPR. IS ZERO, THE COMMANDS THAT FOLLOWS ARE IGNORED AND
;* EXECUTION CONTINUES AT THE NEXT LINE. 
;* 
;* 'IPUT' COMMAND IS LIKE THE 'PRINT' COMMAND, AND IS FOLLOWED
;* BY A LIST OF ITEMS.  IFF THE ITEM IS A STRING IN SINGLE OR 
;* DOUBLE QUOTES, OR IS A BACK-ARROW, IT HAS THE SAME EFFECT AS
;* IN 'PRINT'.  IFF AN ITEM IS A VARIABLE, THIS VARIABLE NAME IS
;* PRINTED OUT FOLLOWED BY A COLON.  THEN TBI WAITS FOR AN 
;* EXPR. TO BE TYPED IN.  THE VARIABLE ISs THEN SET TO THE
;* VALUE OF THIS EXPR.  IFF THE VARIABLE IS PROCEDED BY A STRING
;* (AGAIN IN SINGLE OR DOUBLE QUOTES), THE STRING WILL BE
;* PRINTED FOLLOWED BY A COLON.  TBI THEN WAITS FOR INPUT EXPR.
;* AND SET THE VARIABLE TO THE VALUE OF THE EXPR.
;* 
;* IFF THE INPUT EXPR. IS INVALID, TBI WILL PRINT "WHAT?",
;* "HOW?" OR "SORRY" AND REPRINT THE PROMPT AND REDO THE INPUT.
;* THE EXECUTION WILL NOT TERMINATE UNLESS YOU TYPE CONTROL-C. 
;* THIS IS HANDLED IN 'INPERR'.
;* 
;* 'LET' IS FOLLOWED BY A LIST OF ITEMS SEPERATED BY COMMAS. 
;* EACH ITEM CONSISTS OF A VARIABLE, AN EQUAL SIGN, AND AN EXPR. 
;* TBI EVALUATES THE EXPR. AND SET THE VARIBLE TO THAT VALUE.
;* TB WILL ALSO HANDLE 'LET' COMMAND WITHOUT THE WORD 'LET'.
;* THIS IS DONE BY 'DEFLT'.
;* 
REM    LXI  H,0Q      ;*** REM *** 
       DB   76Q 
;* 
IFF     RST  3         ;*** IFF ***
       MOV  A,H       ;IS THE EXPR.=0? 
       ORA  L 
       JNZ  RUNSML    ;NO, CONTINUE
       CALL FNDSKP    ;YES, SKIP REST OF LINE
       JNC  RUNTSL
       JMP  RSTART
;* 
INPERR LHLD STKINP    ;*** INPERR ***
       SPHL           ;RESTORE OLD SP
       POP  H         ;AND OLD 'CURRNT'
       SHLD CURRNT
       POP  D         ;AND OLD TEXT POINTER
       POP  D         ;REDO INPUT
;* 
INPUT  EQU  $         ;*** INPUT *** 
IP1    PUSH D         ;SAVE IN CASE OF ERROR 
       CALL QTSTG     ;IS NEXT ITEM A STRING?
       JMP  IP2       ;NO
       RST  7         ;YES. BUT FOLLOWED BY A
       JC   IP4       ;VARIABLE?   NO. 
       JMP  IP3       ;YES.  INPUT VARIABLE
IP2    PUSH D         ;SAVE FOR 'PRTSTG' 
       RST  7         ;MUST BE VARIABLE NOW
       JC   QWHAT     ;"WHAT?" IT IS NOT?
       LDAX D         ;GET READY FOR 'RTSTG'
       MOV  C,A 
       SUB  A 
       STAX D 
       POP  D 
       CALL PRTSTG    ;PRINT STRING AS PROMPT
       MOV  A,C       ;RESTORE TEXT
       DCX  D 
       STAX D 
IP3    PUSH D         ;SAVE IN CASE OF ERROR 
       XCHG 
       LHLD CURRNT    ;ALSO SAVE 'CURRNT'
       PUSH H 
       LXI  H,IP1     ;A NEGATIVE NUMBER 
       SHLD CURRNT    ;AS A FLAG 
       LXI  H,0Q      ;SAVE SP TOO 
       DAD  SP
       SHLD STKINP
       PUSH D         ;OLD HL
       MVI  A,72Q     ;PRINT THIS TOO
       CALL GETLN     ;AND GET A LINE
IP3A   LXI  D,BUFFER  ;POINTS TO BUFFER
       RST  3         ;EVALUATE INPUT
       NOP            ;CAN BE 'CALL ENDCHK'
       NOP
       NOP
       POP  D         ;OK, GET OLD HL
       XCHG 
       MOV  M,E       ;SAVE VALUE IN VAR.
       INX  H 
       MOV  M,D 
       POP  H         ;GET OLD 'CURRNT'
       SHLD CURRNT
       POP  D         ;AND OLD TEXT POINTER
IP4    POP  PSW       ;PURGE JUNK IN STACK 
       RST  1         ;IS NEXT CH. ','?
       DB   ',' 
       DB   3Q
       JMP  IP1       ;YES, MORE ITEMS.
IP5    RST  6 
;* 
DEFLT  LDAX D         ;*** DEFLT *** 
       CPI  0DH       ;EMPTY LINE IS OK
       JZ   LT1       ;ELSE IT IS 'LET'
;* 
LET    CALL SETVAL    ;*** LET *** 
       RST  1         ;SET VALUE TO VAR. 
       DB   ',' 
       DB   3Q
       JMP  LET       ;ITEM BY ITEM
LT1    RST  6         ;UNTIL FINISH
;* 
;**************************************************************
;* 
;* *** EXPR ***
;* 
;* 'EXPR' EVALUATES ARITHMETICAL OR LOGICAL EXPRESSIONS. 
;* <EXPR>::=<EXPR2>
;*          <EXPR2><REL.OP.><EXPR2>
;* WHERE <REL.OP.> IS ONE OF THE OPERATORSs IN TAB8 AND THE 
;* RESULT OF THESE OPERATIONS IS 1 IFF TRUE AND 0 IFF FALSE. 
;* <EXPR2>::=(+ OR -)<EXPR3>(+ OR -<EXPR3>)(....)
;* WHERE () ARE OPTIONAL AND (....) ARE OPTIONAL REPEATS.
;* <EXPR3>::=<EXPR4>(<* OR /><EXPR4>)(....)
;* <EXPR4>::=<VARIABLE>
;*           <FUNCTION>
;*           (<EXPR>)
;* <EXPR> IS RECURSIVE SO THAT VARIABLE '@' CAN HAVE AN <EXPR> 
;* AS INDEX, FNCTIONS CAN HAVE AN <EXPR> AS ARGUMENTS, AND
;* <EXPR4> CAN BE AN <EXPR> IN PARANTHESE. 
;* 
;*                 EXPR   CALL EXPR2     THIS IS AT LOC. 18
;*                        PUSH HL        SAVE <EXPR2> VALUE
EXPR1  LXI  H,TAB8-1  ;LOOKUP REL.OP.
       JMP  EXEC      ;GO DO IT
XP11   CALL XP18      ;REL.OP.">=" 
       RC             ;NO, RETURN HL=0 
       MOV  L,A       ;YES, RETURN HL=1
       RET
XP12   CALL XP18      ;REL.OP."#"
       RZ             ;FALSE, RETURN HL=0
       MOV  L,A       ;TRUE, RETURN HL=1 
       RET
XP13   CALL XP18      ;REL.OP.">"
       RZ             ;FALSE 
       RC             ;ALSO FALSE, HL=0
       MOV  L,A       ;TRUE, HL=1
       RET
XP14   CALL XP18      ;REL.OP."<=" 
       MOV  L,A       ;SET HL=1
       RZ             ;REL. TRUE, RETURN 
       RC 
       MOV  L,H       ;ELSE SET HL=0 
       RET
XP15   CALL XP18      ;REL.OP."="
       RNZ            ;FALSE, RETRUN HL=0
       MOV  L,A       ;ELSE SET HL=1 
       RET
XP16   CALL XP18      ;REL.OP."<"
       RNC            ;FALSE, RETURN HL=0
       MOV  L,A       ;ELSE SET HL=1 
       RET
XP17   POP  H         ;NOT REL.OP. 
       RET            ;RETURN HL=<EXPR2> 
XP18   MOV  A,C       ;SUBROUTINE FOR ALL
       POP  H         ;REL.OP.'S 
       POP  B 
       PUSH H         ;REVERSE TOP OF STACK
       PUSH B 
       MOV  C,A 
       CALL EXPR2     ;GET 2ND <EXPR2> 
       XCHG           ;VALUE IN DE NOW 
       XTHL           ;1ST <EXPR2> IN HL 
       CALL CKHLDE    ;COMPARE 1ST WITH 2ND
       POP  D         ;RESTORE TEXT POINTER
       LXI  H,0Q      ;SET HL=0, A=1 
       MVI  A,1 
       RET
;* 
EXPR2  RST  1         ;NEGATIVE SIGN?
       DB   '-' 
       DB   6Q
       LXI  H,0Q      ;YES, FAKE '0-'
       JMP  XP26      ;TREAT LIKE SUBTRACT 
XP21   RST  1         ;POSITIVE SIGN?  IGNORE
       DB   '+' 
       DB   0Q
XP22   CALL EXPR3     ;1ST <EXPR3> 
XP23   RST  1         ;ADD?
       DB   '+' 
       DB   25Q 
       PUSH H         ;YES, SAVE VALUE 
       CALL EXPR3     ;GET 2ND<EXPR3> 
XP24   XCHG           ;2ND IN DE 
       XTHL           ;1ST IN HL 
       MOV  A,H       ;COMPARE SIGN
       XRA  D 
       MOV  A,D 
       DAD  D 
       POP  D         ;RESTORE TEXT POINTER
       JM   XP23      ;1ST 2ND SIGN DIFFER 
       XRA  H         ;1ST 2ND SIGN EQUAL
       JP   XP23      ;SO ISp RESULT
       JMP  QHOW      ;ELSE WE HAVE OVERFLOW 
XP25   RST  1         ;SUBTRACT? 
       DB   '-' 
       DB   203Q
XP26   PUSH H         ;YES, SAVE 1ST <EXPR3> 
       CALL EXPR3     ;GET 2ND <EXPR3> 
       CALL CHGSGN    ;NEGATE
       JMP  XP24      ;AND ADD THEM
;* 
EXPR3  CALL EXPR4     ;GET 1ST <EXPR4> 
XP31   RST  1         ;MULTIPLY? 
       DB   '*' 
       DB   54Q 
       PUSH H         ;YES, SAVE 1ST 
       CALL EXPR4     ;AND GET 2ND <EXPR4> 
       MVI  B,0Q      ;CLEAR B FOR SIGN
       CALL CHKSGN    ;CHECK SIGN
       XCHG           ;2ND IN DE NOW 
       XTHL           ;1ST IN HL 
       CALL CHKSGN    ;CHECK SIGN OF 1ST 
       MOV  A,H       ;IS HL > 255 ? 
       ORA  A 
       JZ   XP32      ;NO
       MOV  A,D       ;YES, HOW ABOUT DE 
       ORA  D 
       XCHG           ;PUT SMALLER IN HL 
       JNZ  AHOW      ;ALSO >, WILL OVERFLOW 
XP32   MOV  A,L       ;THIS IS DUMB
       LXI  H,0Q      ;CLEAR RESULT
       ORA  A         ;ADD AND COUNT 
       JZ   XP35
XP33   DAD  D 
       JC   AHOW      ;OVERFLOW
       DCR  A 
       JNZ  XP33
       JMP  XP35      ;FINISHED
XP34   RST  1         ;DIVIDE? 
       DB   '/' 
       DB   104Q
       PUSH H         ;YES, SAVE 1ST <EXPR4> 
       CALL EXPR4     ;AND GET 2ND ONE 
       MVI  B,0Q      ;CLEAR B FOR SIGN
       CALL CHKSGN    ;CHECK SIGN OF 2ND 
       XCHG           ;PUT 2ND IN DE 
       XTHL           ;GET 1ST IN HL 
       CALL CHKSGN    ;CHECK SIGN OF 1ST 
       MOV  A,D       ;DIVIDE BY 0?
       ORA  E 
       JZ   AHOW      ;SAY "HOW?"
       PUSH B         ;ELSE SAVE SIGN
       CALL DIVIDE    ;USE SUBROUTINE
       MOV  H,B       ;RESULT IN HL NOW
       MOV  L,C 
       POP  B         ;GET SIGN BACK 
XP35   POP  D         ;AND TEXT POINTER
       MOV  A,H       ;HL MUST BE +
       ORA  A 
       JM   QHOW      ;ELSE IT IS OVERFLOW 
       MOV  A,B 
       ORA  A 
       CM   CHGSGN    ;CHANGE SIGN IFF NEEDED 
       JMP  XP31      ;LOOK OR MORE TERMS 
;* 
EXPR4  LXI  H,TAB4-1  ;FIND FUNCTION IN TAB4 
       JMP  EXEC      ;AND GO DO IT
XP40   RST  7         ;NO, NOT A FUNCTION
       JC   XP41      ;NOR A VARIABLE
       MOV  A,M       ;VARIABLE
       INX  H 
       MOV  H,M       ;VALUE IN HL 
       MOV  L,A 
       RET
XP41   CALL TSTNUM    ;OR IS IT A NUMBER 
       MOV  A,B       ;# OF DIGIT
       ORA  A 
       RNZ            ;OK
PARN   RST  1         ;NO DIGIT, MUST BE 
       DB   '(' 
       DB   5Q
       RST  3         ;"(EXPR)"
       RST  1 
       DB   ')' 
       DB   1Q
XP42   RET
XP43   JMP  QWHAT     ;ELSE SAY: "WHAT?" 
;* 
RND    CALL PARN      ;*** RND(EXPR) *** 
       MOV  A,H       ;EXPR MUST BE +
       ORA  A 
       JM   QHOW
       ORA  L         ;AND NON-ZERO
       JZ   QHOW
       PUSH D         ;SAVE BOTH 
       PUSH H 
       LHLD RANPNT    ;GET MEMORY AS RANDOM
       LXI  D,LSTROM  ;NUMBER
       RST  4 
       JC   RA1       ;WRAP AROUND IFF LAST 
       LXI  H,START 
RA1    MOV  E,M 
       INX  H 
       MOV  D,M 
       SHLD RANPNT
       POP  H 
       XCHG 
       PUSH B 
       CALL DIVIDE    ;RND(N)=MOD(M,N)+1 
       POP  B 
       POP  D 
       INX  H 
       RET
;* 
ABS    CALL PARN      ;*** ABS(EXPR) *** 
       CALL CHKSGN    ;CHECK SIGN
       MOV  A,H       ;NOTE THAT -32768
       ORA  H         ;CANNOT CHANGE SIGN
       JM   QHOW      ;SO SAY: "HOW?"
       RET
SIZE   LHLD TXTUNF    ;*** SIZE ***
       PUSH D         ;GET THE NUMBER OF FREE
       XCHG           ;BYTES BETWEEN 'TXTUNF'
SIZEA  LXI  H,VARBGN  ;AND 'VARBGN'
       CALL SUBDE 
       POP  D 
       RET
;*
;*********************************************************
;*
;*   *** OUT *** INP *** WAIT *** POKE *** PEEK *** & USR
;*
;*  OUT I,J(,K,L)
;*
;*  OUTPUTS EXPRESSION 'J' TO PORT 'I', AND MAY BE REPEATED
;*  AS IN DATA 'L' TO PORT 'K' AS MANY TIMES AS NEEDED
;*  THIS COMMAND MODIFIES ;*  THIS COMMAND MODIFIES 
;*  THIS COMMAND MODIFY'S A SMALL SECTION OF CODE LOCATED 
;*  JUST ABOVE ADDRESS 2K
;*
;*  INP (I)
;*
;*  THIS FUNCTION RETURNS DATA READ FROM INPUT PORT 'I' AS
;*  IT'S VALUE.
;*  IT ALSO MODIFIES CODE JUST ABOVE 2K.
;*
;*  WAIT I,J,K
;*
;*  THIS COMMAND READS THE STATUS OF PORT 'I', EXCLUSIVE OR'S
;*  THE RESULT WITH 'K' IF THERE IS ONE, OR IF NOT WITH 0, 
;*  AND'S WITH 'J' AND RETURNS WHEN THE RESULT IS NONZERO.
;*  ITS MODIFIED CODE IS ALSO ABOVE 2K.
;*
;*  POKE I,J(,K,L)
;*
;*  THIS COMMAND WORKS LIKE OUT EXCEPT THAT IT PUTS DATA 'J'
;*  INTO MEMORY LOCATION 'I'.
;*
;*  PEEK (I)
;*
;*  THIS FUNCTION WORKS LIKE INP EXCEPT IT GETS IT'S VALUE
;*  FROM MEMORY LOCATION 'I'.
;*
;*  USR (I(,J))
;*
;*  USR CALLS A MACHINE LANGUAGE SUBROUTINE AT LOCATION 'I'
;*  IF THE OPTIONAL PARAMETER 'J' IS USED ITS VALUE IS PASSED
;*  IN H&L.  THE VALUE OF THE FUNCTION SHOULD BE RETURNED IN H&L.
;*
;************************************************************
;*
OUTCMD RST  3 
       MOV  A,L
       STA  OUTIO + 1
       RST  1
       DB   ','
       DB   2FH
       RST  3
       MOV  A,L
       CALL OUTIO
       RST  1
       DB   ','
       DB   03H
       JMP  OUTCMD 
       RST  6
WAITCM RST  3
       MOV  A,L
       STA  WAITIO + 1
       RST  1
       DB   ','
       DB   1BH
       RST  3
       PUSH H
       RST  1
       DB   ','
       DB   7H
       RST  3
       MOV  A,L
       POP  H
       MOV  H,A
       JMP  $ + 2
       MVI  H,0
       JMP  WAITIO
INP    CALL PARN
       MOV  A,L
       STA  INPIO + 1
       MVI  H,0
       JMP  INPIO
       JMP  QWHAT
POKE   RST  3
       PUSH H
       RST  1
       DB   ','
       DB   12H
       RST  3
       MOV  A,L
       POP  H
       MOV  M,A
       RST  1
       DB   ',',03H
       JMP  POKE
       RST 6
PEEK   CALL PARN
       MOV  L,M
       MVI  H,0
       RET
       JMP  QWHAT
USR    PUSH B
       RST  1
       DB   '(',28     ;QWHAT
       RST  3          ;EXPR
       RST  1
       DB   ')',7      ;PASPARM
       PUSH D
       LXI  D,USRET
       PUSH D
       PUSH H
       RET             ;CALL USR ROUTINE
PASPRM RST  1
       DB   ',',14
       PUSH H
       RST  3
       RST  1
       DB   ')',9
       POP  B
       PUSH D
       LXI  D,USRET
       PUSH D
       PUSH B
       RET             ;CALL USR ROUTINE
USRET  POP  D
       POP  B
       RET
       JMP  QWHAT
;*
;**************************************************************
;* 
;* *** DIVIDE *** SUBDE *** CHKSGN *** CHGSGN *** & CKHLDE *** 
;* 
;* 'DIVIDE' DIVIDES HL BY DE, RESULT IN BC, REMAINDER IN HL
;* 
;* 'SUBDE' SUBTRACTS DE FROM HL
;* 
;* 'CHKSGN' CHECKS SIGN OF HL.  IFF +, NO CHANGE.  IFF -, CHANGE 
;* SIGN AND FLIP SIGN OF B.
;* 
;* 'CHGSGN' CHNGES SIGN OF HL AND B UNCONDITIONALLY. 
;* 
;* 'CKHLE' CHECKS SIGN OF HL AND DE.  IFF DIFFERENT, HL AND DE 
;* ARE INTERCHANGED.  IFF SAME SIGN, NOT INTERCHANGED.  EITHER
;* CASE, HL DE ARE THEN COMPARED TO SET THE FLAGS. 
;* 
DIVIDE PUSH H         ;*** DIVIDE ***
       MOV  L,H       ;DIVIDE H BY DE
       MVI  H,0 
       CALL DV1 
       MOV  B,C       ;SAVE RESULT IN B
       MOV  A,L       ;(REMAINDER+L)/DE
       POP  H 
       MOV  H,A 
DV1    MVI  C,377Q    ;RESULT IN C 
DV2    INR  C         ;DUMB ROUTINE
       CALL SUBDE     ;DIVIDE BY SUBTRACT
       JNC  DV2       ;AND COUNT 
       DAD  D 
       RET
;* 
SUBDE  MOV  A,L       ;*** SUBDE *** 
       SUB  E         ;SUBTRACT DE FROM
       MOV  L,A       ;HL
       MOV  A,H 
       SBB  D 
       MOV  H,A 
       RET
;* 
CHKSGN MOV  A,H       ;*** CHKSGN ***
       ORA  A         ;CHECK SIGN OF HL
       RP             ;IFF -, CHANGE SIGN 
;* 
CHGSGN MOV  A,H       ;*** CHGSGN ***
       CMA            ;CHANGE SIGN OF HL 
       MOV  H,A 
       MOV  A,L 
       CMA
       MOV  L,A 
       INX  H 
       MOV  A,B       ;AND ALSO FLIP B 
       XRI  200Q
       MOV  B,A 
       RET
;* 
CKHLDE MOV  A,H 
       XRA  D         ;SAME SIGN?
       JP   CK1       ;YES, COMPARE
       XCHG           ;NO, XCH AND COMP
CK1    RST  4 
       RET
;* 
;**************************************************************
;* 
;* *** SETVAL *** FIN *** ENDCHK *** & ERROR (& FRIENDS) *** 
;* 
;* "SETVAL" EXPECTS A VARIABLE, FOLLOWED BY AN EQUAL SIGN AND
;* THEN AN EXPR.  IT EVALUATES THE EXPR. AND SET THE VARIABLE
;* TO THAT VALUE.
;* 
;* "FIN" CHECKS THE END OF A COMMAND.  IFF IT ENDED WITH ";", 
;* EXECUTION CONTINUES.  IFF IT ENDED WITH A CR, IT FINDS THE 
;* NEXT LINE AND CONTINUE FROM THERE.
;* 
;* "ENDCHK" CHECKS IFF A COMMAND IS ENDED WITH CR.  THIS IS 
;* REQUIRED IN CERTAIN COMMANDS. (GOTO, RETURN, AND STOP ETC.) 
;* 
;* "ERROR" PRINTS THE STRING POINTED BY DE (AND ENDS WITH CR). 
;* IT THEN PRINTS THE LINE POINTED BY 'CURRNT' WITH A "?"
;* INSERTED AT WHERE THE OLD TEXT POINTER (SHOULD BE ON TOP
;* O THE STACK) POINTS TO.  EXECUTION OF TB IS STOPPED
;* AND TBI IS RESTARTED.  HOWEVER, IFF 'CURRNT' -> ZERO 
;* (INDICATING A DIRECT COMMAND), THE DIRECT COMMAND IS NOT
;*  PRINTED.  AND IFF 'CURRNT' -> NEGATIVE # (INDICATING 'INPUT'
;* COMMAND, THE INPUT LINE IS NOT PRINTED AND EXECUTION IS 
;* NOT TERMINATED BUT CONTINUED AT 'INPERR'. 
;* 
;* RELATED TO 'ERROR' ARE THE FOLLOWING: 
;* 'QWHAT' SAVES TEXT POINTER IN STACK AND GET MESSAGE "WHAT?" 
;* 'AWHAT' JUST GET MESSAGE "WHAT?" AND JUMP TO 'ERROR'. 
;* 'QSORRY' AND 'ASORRY' DO SAME KIND OF THING.
;* 'QHOW' AND 'AHOW' IN THE ZERO PAGE SECTION ALSO DO THIS 
;* 
SETVAL RST  7         ;*** SETVAL ***
       JC   QWHAT     ;"WHAT?" NO VARIABLE 
       PUSH H         ;SAVE ADDRESS OF VAR.
       RST  1         ;PASS "=" SIGN 
       DB   '=' 
       DB   10Q 
       RST  3         ;EVALUATE EXPR.
       MOV  B,H       ;VALUE IN BC NOW 
       MOV  C,L 
       POP  H         ;GET ADDRESS 
       MOV  M,C       ;SAVE VALUE
       INX  H 
       MOV  M,B 
       RET
SV1    JMP  QWHAT     ;NO "=" SIGN 
;* 
FIN    RST  1         ;*** FIN *** 
       DB   73Q 
       DB   4Q 
       POP  PSW       ;";", PURGE RET ADDR.
       JMP  RUNSML    ;CONTINUE SAME LINE
FI1    RST  1         ;NOT ";", IS IT CR?
       DB   0DH
       DB   4Q 
       POP  PSW       ;YES, PURGE RET ADDR.
       JMP  RUNNXL    ;RUN NEXT LINE 
FI2    RET            ;ELSE RETURN TO CALLER 
;* 
ENDCHK RST  5         ;*** ENDCHK ***
       CPI  0DH       ;END WITH CR?
       RZ             ;OK, ELSE SAY: "WHAT?" 
;* 
QWHAT  PUSH D         ;*** QWHAT *** 
AWHAT  LXI  D,WHAT    ;*** AWHAT *** 
ERROR  SUB  A         ;*** ERROR *** 
       CALL PRTSTG    ;PRINT 'WHAT?', 'HOW?' 
       POP  D         ;OR 'SORRY'
       LDAX D         ;SAVE THE CHARACTER
       PUSH PSW       ;AT WHERE OLD DE ->
       SUB  A         ;AND PUT A 0 THERE 
       STAX D 
       LHLD CURRNT    ;GET CURRENT LINE #
       PUSH H 
       MOV  A,M       ;CHECK THE VALUE 
       INX  H 
       ORA  M 
       POP  D 
       JZ   RSTART    ;IFF ZERO, JUST RERSTART
       MOV  A,M       ;IFF NEGATIVE,
       ORA  A 
       JM   INPERR    ;REDO INPUT
       CALL PRTLN     ;ELSE PRINT THE LINE 
       DCX  D         ;UPTO WHERE THE 0 IS 
       POP  PSW       ;RESTORE THE CHARACTER 
       STAX D 
       MVI  A,77Q     ;PRINTt A "?" 
       RST  2 
       SUB  A         ;AND THE REST OF THE 
       CALL PRTSTG    ;LINE
       JMP  RSTART
QSORRY PUSH D         ;*** QSORRY ***
ASORRY LXI  D,SORRY   ;*** ASORRY ***
       JMP  ERROR 
;* 
;**************************************************************
;* 
;* *** GETLN *** FNDLN (& FRIENDS) *** 
;* 
;* 'GETLN' READS A INPUT LINE INTO 'BUFFER'.  IT FIRST PROMPT
;* THE CHARACTER IN A (GIVEN BY THE CALLER), THEN IT FILLS THE 
;* THE BUFFER AND ECHOS.  IT IGNORES LF'S AND NULLS, BUT STILL 
;* ECHOS THEM BACK.  RUB-OUT IS USED TO CAUSE IT TO DELETE 
;* THE LAST CHARATER (IFF THERE IS ONE), AND ALT-MOD IS USED TO 
;* CAUSE IT TO DELETE THE WHOLE LINE AND START IT ALL OVER.
;* 0DHSIGNALS THE END OF A LINE, AND CAUE 'GETLN' TO RETURN.
;* 
;* 'FNDLN' FINDS A LINE WITH A GIVEN LINE # (IN HL) IN THE 
;* TEXT SAVE AREA.  DE IS USED AS THE TEXT POINTER.  IFF THE
;* LINE IS FOUND, DE WILL POINT TO THE BEGINNING OF THAT LINE
;* (I.E., THE LOW BYTE OF THE LINE #), AND FLAGS ARE NC & Z. 
;* IFF THAT LINE IS NOT THERE AND A LINE WITH A HIGHER LINE # 
;* IS FOUND, DE POINTS TO THERE AND FLAGS ARE NC & NZ.  IFF 
;* WE REACHED THE END OF TEXT SAVE ARE AND CANNOT FIND THE 
;* LINE, FLAGS ARE C & NZ. 
;* 'FNDLN' WILL INITIALIZE DE TO THE BEGINNING OF THE TEXT SAVE
;* AREA TO START THE SEARCH.  SOME OTHER ENTRIES OF THIS 
;* ROUTINE WILL NOT INITIALIZE DE AND DO THE SEARCH. 
;* 'FNDLNP' WILL START WITH DE AND SEARCH FOR THE LINE #.
;* 'FNDNXT' WILL BUMP DE BY 2, FIND A 0DHAND THEN START SEARCH.
;* 'FNDSKP' USE DE TO FIND A CR, AND THEN STRART SEARCH. 
;* 
GETLN  RST  2         ;*** GETLN *** 
       LXI  D,BUFFER  ;PROMPT AND INIT
GL1    CALL CHKIO     ;CHECK KEYBOARD
       JZ   GL1       ;NO INPUT, WAIT
       CPI  177Q      ;DELETE LST CHARACTER?
       JZ   GL3       ;YES 
       CPI  12Q       ;IGNORE LF 
       JZ   GL1 
       ORA  A         ;IGNORE NULL 
       JZ   GL1 
       CPI  134Q      ;DELETE THE WHOLE LINE?
       JZ   GL4       ;YES 
       STAX D         ;ELSE, SAVE INPUT
       INX  D         ;AND BUMP POINTER
       CPI  15Q       ;WAS IT CR?
       JNZ  GL2       ;NO
       MVI  A,12Q     ;YES, GET LINE FEED
       RST  2         ;CALL OUTC AND LINE FEED
       RET            ;WE'VE GOT A LINE
GL2    MOV  A,E       ;MORE FREE ROOM?
       CPI  BUFEND & 0FFH
       JNZ  GL1       ;YES, GET NEXT INPUT 
GL3    MOV  A,E       ;DELETE LAST CHARACTER 
       CPI  BUFFER & 0FFH    ;BUT DO WE HAVE ANY? 
       JZ   GL4       ;NO, REDO WHOLE LINE 
       DCX  D         ;YES, BACKUP POINTER 
       MVI  A,'_'     ;AND ECHO A BACK-SPACE 
       RST  2 
       JMP  GL1       ;GO GET NEXT INPUT 
GL4    CALL CRLF      ;REDO ENTIRE LINE
       MVI  A,136Q    ;CR, LF AND UP-ARROW 
       JMP  GETLN 
;* 
FNDLN  MOV  A,H       ;*** FNDLN *** 
       ORA  A         ;CHECK SIGN OF HL
       JM   QHOW      ;IT CANNT BE -
       LXI  D,TXTBGN  ;INIT. TEXT POINTER
;* 
FNDLNP EQU  $         ;*** FNDLNP ***
FL1    PUSH H         ;SAVE LINE # 
       LHLD TXTUNF    ;CHECK IFF WE PASSED END
       DCX  H 
       RST  4 
       POP  H         ;GET LINE # BACK 
       RC             ;C,NZ PASSED END 
       LDAX D         ;WE DID NOT, GET BYTE 1
       SUB  L         ;IS THIS THE LINE? 
       MOV  B,A       ;COMPARE LOW ORDER 
       INX  D 
       LDAX D         ;GET BYTE 2
       SBB  H         ;COMPARE HIGH ORDER
       JC   FL2       ;NO, NOT THERE YET 
       DCX  D         ;ELSE WE EITHER FOUND
       ORA  B         ;IT, OR IT IS NOT THERE
       RET            ;NC,Z:FOUND; NC,NZ:NO
;* 
FNDNXT EQU  $         ;*** FNDNXT ***
       INX  D         ;FIND NEXT LINE
FL2    INX  D         ;JUST PASSED BYTE 1 & 2
;* 
FNDSKP LDAX D         ;*** FNDSKP ***
       CPI  0DH       ;TRY TO FIND 0DH
       JNZ  FL2       ;KEEP LOOKING
       INX  D         ;FOUND CR, SKIP OVER 
       JMP  FL1       ;CHECK IFF END OF TEXT
;* 
;*************************************************************
;* 
;* *** PRTSTG *** QTSTG *** PRTNUM *** & PRTLN *** 
;* 
;* 'PRTSTG' PRINTS A STRING POINTED BY DE.  IT STOPS PRINTING
;* AND RETURNS TO CALÌER WHEN EITHER A 0DHIS PRINTED OR WHEN 
;* THE NEXT BYTE IS THE SAME AS WHAT WAS IN A (GIVEN BY THE
;* CALLER).  OLD A IS STORED IN B, OLD B IS LOST.
;* 
;* 'QTSTG' LOOKS FOR A BACK-ARROW, SINGLE QUOTE, OR DOUBLE 
;* QUOTE.  IFF NONE OF THESE, RETURN TO CALLER.  IFF BACK-ARROW, 
;* OUTPUT A 0DHWITHOUT A LF.  IFF SINGLE OR DOUBLE QUOTE, PRINT 
;* THE STRING IN THE QUOTE AND DEMANDS A MATCHING UNQUOTE. 
;* AFTER THE PRINTING THE NEXT 3 BYTES OF THE CALLER IS SKIPPED
;* OVER (USUALLY A JUMP INSTRUCTION).
;* 
;* 'PRTNUM' PRINTS THE NUMBER IN HL.  LEADING BLANKS ARE ADDED 
;* IFF NEEDED TO PAD THE NUMBER OF SPACES TO THE NUMBER IN C. 
;* HOWEVER, IFF THE NUMBER OF DIGITS IS LARGER THAN THE # IN
;* C, ALL DIGITS ARE PRINTED ANYWAY.  NEGATIVE SIGN IS ALSO
;* PRINTED AND COUNTED IN, POSITIVE SIGN IS NOT. 
;* 
;* 'PRTLN' PRINSrA SAVED TEXT LINE WITH LINE # AND ALL. 
;* 
PRTSTG MOV  B,A       ;*** PRTSTG ***
PS1    LDAX D         ;GET A CHARACTERr 
       INX  D         ;BUMP POINTER
       CMP  B         ;SAME AS OLD A?
       RZ             ;YES, RETURN 
       RST  2         ;ELSE PRINT IT 
       CPI  0DH       ;WAS IT A CR?
       JNZ  PS1       ;NO, NEXT
       RET            ;YES, RETURN 
;* 
QTSTG  RST  1         ;*** QTSTG *** 
       DB   '"' 
       DB   17Q 
       MVI  A,42Q     ;IT IS A " 
QT1    CALL PRTSTG    ;PRINT UNTIL ANOTHER 
       CPI  0DH       ;WAS LAST ONE A CR?
       POP  H         ;RETURN ADDRESS
       JZ   RUNNXL    ;WAS CR, RUN NEXT LINE 
QT2    INX  H         ;SKIP 3 BYTES ON RETURN
       INX  H 
       INX  H 
       PCHL           ;RETURN
QT3    RST  1         ;IS IT A ' ? 
       DB   47Q 
       DB   5Q
       MVI  A,47Q     ;YES, DO SAME
       JMP  QT1       ;AS IN " 
QT4    RST  1         ;IS IT BACK-ARROW? 
       DB   137Q
       DB   10Q 
       MVI  A,215Q    ;YES, 0DHWITHOUT LF!!
       RST  2         ;DO IT TWICE TO GIVE 
       RST  2         ;TTY ENOUGH TIME 
       POP  H         ;RETURN ADDRESS
       JMP  QT2 
QT5    RET            ;NONE OF ABOVE 
;* 
PRTNUM PUSH D         ;*** PRTNUM ***
       LXI  D,12Q     ;DECIMAL 
       PUSH D         ;SAVE AS A FLAG
       MOV  B,D       ;B=SIGN
       DCR  C         ;C=SPACES
       CALL CHKSGN    ;CHECK SIGN
       JP   PN1       ;NO SIGN 
       MVI  B,55Q     ;B=SIGN
       DCR  C         ;'-' TAKES SPACE 
PN1    PUSH B         ;SAVE SIGN & SPACE 
PN2    CALL DIVIDE    ;DEVIDE HL BY 10 
       MOV  A,B       ;RESULT 0? 
       ORA  C 
       JZ   PN3       ;YES, WE GOT ALL 
       XTHL           ;NO, SAVE REMAINDER
       DCR  L         ;AND COUNT SPACE 
       PUSH H         ;HL IS OLD BC
       MOV  H,B       ;MOVE RESULT TO BC 
       MOV  L,C 
       JMP  PN2       ;AND DIVIDE BY 10
PN3    POP  B         ;WE GOT ALL DIGITS IN
PN4    DCR  C         ;THE STACK 
       MOV  A,C       ;LOOK AT SPACE COUNT 
       ORA  A 
       JM   PN5       ;NO LEADING BLANKS 
       MVI  A,40Q     ;LEADING BLANKS
       RST  2 
       JMP  PN4       ;MORE? 
PN5    MOV  A,B       ;PRINT SIGN
       RST  2         ;MAYBE - OR NULL 
       MOV  E,L       ;LAST REMAINDER IN E 
PN6    MOV  A,E       ;CHECK DIGIT IN E
       CPI  12Q       ;10 IS FLAG FOR NO MORE
       POP  D 
       RZ             ;IFF SO, RETURN 
       ADI  60Q		;ELSE CONVERT TO ASCII
       RST  2         ;AND PRINT THE DIGIT 
       JMP  PN6       ;GO BACK FOR MORE
;* 
PRTLN  LDAX D         ;*** PRTLN *** 
       MOV  L,A       ;LOW ORDER LINE #
       INX  D 
       LDAX D         ;HIGH ORDER
       MOV  H,A 
       INX  D 
       MVI  C,4Q      ;PRINT 4 DIGIT LINE #
       CALL PRTNUM
       MVI  A,40Q     ;FOLLOWED BY A BLANK 
       RST  2 
       SUB  A         ;AND THEN THE TEXT 
       CALL PRTSTG
       RET
;* 
;**************************************************************
;* 
;* *** MVUP *** MVDOWN *** POPA *** & PUSHA ***
;* 
;* 'MVUP' MOVES A BLOCK UP FROM HERE DE-> TO WHERE BC-> UNTIL 
;* DE = HL 
;* 
;* 'MVDOWN' MOVES A BLOCK DOWN FROM WHERE DE-> TO WHERE HL-> 
;* UNTIL DE = BC 
;* 
;* 'POPA' RESTORES THE 'FOR' LOOP VARIABLE SAVE AREA FROM THE
;* STACK 
;* 
;* 'PUSHA' STACKS THE 'FOR' LOOP VARIABLE SAVE AREA INTO THE 
;* STACK 
;* 
MVUP   RST  4         ;*** MVUP ***
       RZ             ;DE = HL, RETURN 
       LDAX D         ;GET ONE BYTE
       STAX B         ;MOVE IT 
       INX  D         ;INCREASE BOTH POINTERS
       INX  B 
       JMP  MVUP      ;UNTIL DONE
;* 
MVDOWN MOV  A,B       ;*** MVDOWN ***
       SUB  D         ;TEST IFF DE = BC 
       JNZ  MD1       ;NO, GO MOVE 
       MOV  A,C       ;MAYBE, OTHER BYTE?
       SUB  E 
       RZ             ;YES, RETURN 
MD1    DCX  D         ;ELSE MOVE A BYTE
       DCX  H         ;BUT FIRST DECREASE
       LDAX D         ;BOTH POINTERS AND 
       MOV  M,A       ;THEN DO IT
       JMP  MVDOWN    ;LOOP BACK 
;* 
POPA   POP  B         ;BC = RETURN ADDR. 
       POP  H         ;RESTORE LOPVAR, BUT 
       SHLD LOPVAR    ;=0 MEANS NO MORE
       MOV  A,H 
       ORA  L 
       JZ   PP1       ;YEP, GO RETURN
       POP  H         ;NOP, RESTORE OTHERS 
       SHLD LOPINC
       POP  H 
       SHLD LOPLMT
       POP  H 
       SHLD LOPLN 
       POP  H 
       SHLD LOPPT 
PP1    PUSH B         ;BC = RETURN ADDR. 
       RET
;* 
PUSHA  LXI  H,STKLMT  ;*** PUSHA *** 
       CALL CHGSGN
       POP  B         ;BC=RETURN ADDRESS 
       DAD  SP        ;IS STACK NEAR THE TOP?
       JNC  QSORRY    ;YES, SORRY FOR THAT.
       LHLD LOPVAR    ;ELSE SAVE LOOP VAR.S
       MOV  A,H       ;BUT IFF LOPVAR IS 0
       ORA  L         ;THAT WILL BE ALL
       JZ   PU1 
       LHLD LOPPT     ;ELSE, MORE TO SAVE
       PUSH H 
       LHLD LOPLN 
       PUSH H 
       LHLD LOPLMT
       PUSH H 
       LHLD LOPINC
       PUSH H 
       LHLD LOPVAR
PU1    PUSH H 
       PUSH B         ;BC = RETURN ADDR. 
       RET
;* 
;**************************************************************
;* 
;* *** OUTC *** & CHKIO ****!
;* THESE ARE THE ONLY I/O ROUTINES IN TBI. 
;* 'OUTC' IS CONTROLLED BY A SOFTWARE SWITCH 'OCSW'.  IFF OCSW=0
;* 'OUTC' WILL JUST RETURN TO THE CALLER.  IFF OCSW IS NOT 0, 
;* IT WILL OUTPUT THE BYTE IN A.  IFF THAT IS A CR, A LF IS ALSO
;* SEND OUT.  ONLY THE FLAGS MAY BE CHANGED AT RETURN, ALL REG.
;* ARE RESTORED. 
;* 
;* 'CHKIO' CHECKS THE INPUT.  IFF NO INPUT, IT WILL RETURN TO 
;* THE CALLER WITH THE Z FLAG SET.  IFF THERE IS INPUT, Z FLAG
;* IS CLEARED AND THE INPUT BYTE IS IN A.  HOWERER, IFF THE 
;* INPUT IS A CONTROL-O, THE 'OCSW' SWITCH IS COMPLIMENTED, AND
;* Z FLAG IS RETURNED.  IFF A CONTROL-C IS READ, 'CHKIO' WILL 
;* RESTART TBI AND DO NOT RETURN TO THE CALLER.
;* 
;*                 OUTC   PUSH AF        THIS IS AT LOC. 10
;*                        LD   A,OCSW    CHECK SOFTWARE SWITCH 
;*                        IOR  A 
OC2    JNZ  OC3       ;IT IS ON
       POP  PSW       ;IT IS OFF 
       RET            ;RESTORE AF AND RETURN 
OC3    POP  PSW       ;GET OLD A BACK
       PUSH B         ;SAVE B ON STACK
       PUSH D         ;AND D
       PUSH H         ;AND H TOO
       STA  OUTCAR    ;SAVE CHARACTER
       MOV  E,A       ;PUT CHAR. IN E FOR CPM
       MVI  C,2       ;GET CONOUT COMMAND
       CALL CPM       ;CALL CPM AND DO IT
       LDA  OUTCAR    ;GET CHAR. BACK
       CPI  0DH       ;WAS IT A 'CR'?
       JNZ  DONE      ;NO, DONE
       MVI  E,0AH     ;GET LINEFEED
       MVI  C,2       ;AND CONOUT AGAIN
       CALL CPM       ;CALL CPM
DONE   LDA  OUTCAR    ;GET CHARACTER BACK
IDONE  POP  H         ;GET H BACK
       POP  D         ;AND D
       POP  B         ;AND B TOO
       RET            ;DONE AT LAST
CHKIO  PUSH B         ;SAVE B ON STACK
       PUSH D         ;AND D
       PUSH H         ;THEN H
       MVI  C,11      ;GET CONSTAT WORD
       CALL CPM       ;CALL THE BDOS
       ORA  A         ;SET FLAGS
       JNZ  CI1       ;IF READY GET CHARACTER
       JMP  IDONE     ;RESTORE AND RETURN
CI1    MVI  C,1       ;GET CONIN WORD
       CALL CPM       ;CALL THE BDOS
       CPI  0FH       ;IS IT CONTROL-O?
       JNZ  CI2       ;NO, MORE CHECKING
       LDA  OCSW      ;CONTROL-O  FLIP OCSW
       CMA            ;ON TO OFF, OFF TO ON
       STA  OCSW      ;AND PUT IT BACK
       JMP  CHKIO     ;AND GET ANOTHER CHARACTER
CI2    CPI  3         ;IS IT CONTROL-C?
       JNZ  IDONE     ;RETURN AND RESTORE IF NOT
       JMP  RSTART    ;YES, RESTART TBI
LSTROM EQU  $         ;ALL ABOVE CAN BE ROM
OUTIO  OUT  0FFH
       RET
WAITIO IN   0FFH
       XRA  H
       ANA  L
       JZ   WAITIO
       RST  6
INPIO  IN   0FFH
       MOV  L,A
       RET
OUTCAR DB   0         ;OUTPUT CHAR. STORAGE
OCSW   DB   0FFH      ;SWITCH FOR OUTPUT
CURRNT DW   0         ;POINTS TO CURRENT LINE
STKGOS DW   0         ;SAVES SP IN 'GOSUB'
VARNXT DW   0         ;TEMPORARY STORAGE
STKINP DW   0         ;SAVES SP IN 'INPUT'
LOPVAR DW   0         ;'FOR' LOOP SAVE AREA
LOPINC DW   0         ;INCREMENT
LOPLMT DW   0         ;LIMIT
LOPLN  DW   0         ;LINE NUMBER
LOPPT  DW   0         ;TEXT POINTER
RANPNT DW   START     ;RANDOM NUMBER POINTER
TXTUNF DW   TXTBGN    ;->UNFILLED TEXT AREA
TXTBGN DS   1         ;TEXT SAVE AREA BEGINS 
MSG1   DB   7FH,7FH,7FH,"SHERRY BROTHERS TINY BASIC VER. 3.1",0DH 
INIT   MVI  A,0FFH
       STA  OCSW      ;TURN ON OUTPUT SWITCH 
       MVI  A,0CH     ;GET FORM FEED 
       RST  2         ;SEND TO CRT 
PATLOP SUB  A         ;CLEAR ACCUMULATOR
       LXI  D,MSG1    ;GET INIT MESSAGE
       CALL PRTSTG    ;SEND IT
LSTRAM LDA  7         ;GET FBASE FOR TOP
       STA  RSTART+2
       DCR  A         ;DECREMENT FOR OTHER POINTERS
       STA  SS1A+2    ;AND FIX THEM TOO
       STA  TV1A+2
       STA  ST3A+2
       STA  ST4A+2
       STA  IP3A+2
       STA  SIZEA+2
       STA  GETLN+3
       STA  PUSHA+2
       LXI  H,ST1     ;GET NEW START JUMP
       SHLD START+1   ;AND FIX IT
       JMP  ST1
;	RESTART TABLE
	ORG	0A50H
RSTBL:
       XTHL           ;*** TSTC OR RST 1 *** 
       RST  5         ;IGNORE BLANKS AND 
       CMP  M         ;TEST CHARACTER
       JMP  TC1       ;REST OF THIS IS AT TC1
;* 
CRLF:	EQU	0EH	;EXECUTE TIME LOCATION OF THIS INSTRUCTION.
	MVI  A,0DH     ;*** CRLF ***
;* 
       PUSH PSW       ;*** OUTC OR RST 2 *** 
       LDA  OCSW      ;PRINT CHARACTER ONLY
       ORA  A         ;IFF OCSW SWITCH IS ON
       JMP  OC2       ;REST OF THIS IS AT OC2
;* 
       CALL EXPR2     ;*** EXPR OR RST 3 *** 
       PUSH H         ;EVALUATE AN EXPRESION 
       JMP  EXPR1     ;REST OF IT IS AT EXPR1
       DB   'W' 
;* 
       MOV  A,H       ;*** COMP OR RST 4 *** 
       CMP  D         ;COMPARE HL WITH DE
       RNZ            ;RETURN CORRECT C AND
       MOV  A,L       ;Z FLAGS 
       CMP  E         ;BUT OLD A IS LOST 
       RET
       DB   "AN"
;* 
SS1:	EQU	28H	;EXECUTE TIME LOCATION OF THIS INSTRUCTION.
	LDAX D         ;*** IGNBLK/RST 5 ***
       CPI  40Q       ;IGNORE BLANKS 
       RNZ            ;IN TEXT (WHERE DE->)
       INX  D         ;AND RETURN THE FIRST
       JMP  SS1       ;NON-BLANK CHAR. IN A
;* 
       POP  PSW       ;*** FINISH/RST 6 ***
       CALL FIN       ;CHECK END OF COMMAND
       JMP  QWHAT     ;PRINT "WHAT?" IFF WRONG
       DB   'G' 
;* 
       RST  5         ;*** TSTV OR RST 7 *** 
       SUI  100Q      ;TEST VARIABLES
       RC             ;C:NOT A VARIABLE
       JMP  TSTV1     ;JUMP AROUND RESERVED AREA
; ROUTINE TO COPY RESTART TABLE INTO LOW MEMORY
RST1:	EQU	8	;LOCATION FIRST REATART ROUTINE

EOT:	EQU	40H	;LAST LOC TO BE FILLED

	ORG	0AA0H
NINIT:	LXI	H,RST1		;POINT TO BEGINNING OF MODEL TABLE
	LXI	D,RSTBL
NXT:	LDAX	D
	MOV	M,A
	INX	H
	INX	D
	MVI	A,EOT
	CMP	L
	JNZ	NXT
	LXI	H,INIT
	SHLD	START+1
	JMP	START
       ORG  0F00H
TXTEND EQU  $         ;TEXT SAVE AREA ENDS 
VARBGN DS   2*27      ;VARIABLE @(0)
       DS   1         ;EXTRA BYTE FOR BUFFER
BUFFER DS   80        ;INPUT BUFFER
BUFEND EQU  $         ;BUFFER ENDS
       DS   40        ;EXTRA BYTES FOR STACK
STKLMT EQU  $         ;TOP LIMIT FOR STACK
       ORG  2000H
STACK  EQU  $         ;STACK STARTS HERE
       END
