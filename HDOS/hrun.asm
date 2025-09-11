; HRUN is a set of programs that allow you to run HDOS 2.0 commands and
; programs under CP/M. It was developed around 1983 and distributed by
; HUG. It will run on an H8 or H89 or even other CP/M computers. There
; is also a version for the H/Z-100 computer which ran CP/M but did not
; support HDOS natively.
; 
; Most HDOS programs seem to run okay (e.g. BH BASIC, MBASIC, PIE,
; MAPLE). There is even a program that can transfer files from HDOS to
; CP/M disks. Kind of cool to see Benton Harbor BASIC running under
; CP/M!
; 
; Documentation is here:
; https://heathkit.garlanger.com/software/library/HUG/docs/HRUN_doc.pdf
; 
; It was also described in REMark Issue 37 1983 and REMark Issue 43
; 1983.
; 
; The software can be found here
; https://sebhc.github.io/sebhc/software/Applications/SEBHCArchive_Vol1_20090625.zip
; in these disk images:
; 
; 885-1223a_HRUN_HDOS_Emulator_for_CPM.h8d
; 885-1223b_HRUN_HDOS_Emulator_for_CPM.h8d
; 885-1223c_HRUN_HDOS_Emulator_for_CPM.h8d
; 
; This version has additional Y2K fixes by me `to support dates after
; 1999 in the DATE command. These were taken from similar fixes for HDOS
; 2.0.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                       ;
;       HRUN -- RUN HDOS PROGRAMS UNDER CP/M            ;
;                                                       ;
;       THIS PROGRAM IS A "RUN TIME INTERPRETER"        ;
;       FOR HDOS, TO ALLOW HDOS PROGRAMS TO BE RUN      ;
;       ON A CP/M SYSTEM.  ANY HDOS PROGRAM WILL        ;
;       RUN THAT LIMITS ITSELF TO ACCESSING THE         ;
;       SYSTEM THROUGH SCALLS OR H17 ROM CALLS          ;
;       (EXCEPT FOR H17 DISK ROUTINES).                 ;
;                                                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;       BY P. SWAYNE, HUG  19-JAN-83

;       COPYRIGHT (C) 1983 BY HEATH USERS' GROUP

;       NOTE: THIS PROGRAM MUST BE COMBINED WITH H17ROM.HEX
;       USING DDT IN ORDER FOR IT TO WORK.

VER     EQU     20H             ;EMULATING HDOS VERSION 2.0

TRUE    EQU     0FFFFH
FALSE   EQU     NOT TRUE

Z100    EQU     FALSE           ;ASSEMBLE FOR H/Z100
TABX    EQU     FALSE           ;EXPAND TABS TO LP:

DBUG    EQU     FALSE           ;ASSEMBLE TEST VERSION

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               ;
;       CP/M DEFINITIONS        ;
;                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;       BDOS FUNCTIONS

BDOS    EQU     5               ;BDOS JUMP VECTOR
CONIN   EQU     1               ;CONSOLE INPUT
CONOUT  EQU     2               ;CONSOLE OUTPUT
LSTOUT  EQU     5               ;LST OUTPUT
DCIO    EQU     6               ;DIRECT CONSOLE I/O
PRINT   EQU     9               ;PRINT STRING
RDCON   EQU     10              ;READ CONSOLE BUFFER
CONST   EQU     11              ;GET CONSOLE STATUS
RESET   EQU     13              ;RESET DISK SYSTEM
SELDSK  EQU     14              ;SELECT DISK
OPENF   EQU     15              ;OPEN FILE
CLOSEF  EQU     16              ;CLOSE FILE
SFF     EQU     17              ;SEARCH FOR FIRST
SFN     EQU     18              ;SEARCH FOR NEXT
DELETE  EQU     19              ;DELETE FILE
READF   EQU     20              ;READ FILE (SEQUENTIAL)
WRITEF  EQU     21              ;WRITE FILE (SEQUENTIAL)
MAKEF   EQU     22              ;MAKE FILE
RENAMF  EQU     23              ;RENAME FILE
RETCD   EQU     25              ;RETURN CURRENT DISK
SETDMA  EQU     26              ;SET DMA ADDRESS
FLAGS   EQU     30              ;SET FILE ATTRIBUTES
GETDPB  EQU     31              ;GET DISK PARAMETERS
READR   EQU     33              ;READ RANDOM
WRITER  EQU     34              ;WRITE RANDOM
CFS     EQU     35              ;COMPUTE FILE SIZE
SRR     EQU     36              ;SET RANDOM RECORD
RESDRV  EQU     37              ;RESET DRIVE
WRITER0 EQU     40              ;WRITE RANDOM WITH ZERO FILL

;       FCB DEFINITIONS

DRIVE   EQU     0               ;DRIVE CODE
FNAME   EQU     1               ;FILE NAME
FTYPE   EQU     9               ;FILE TYPE
EXTENT  EQU     12              ;EXTENT NUMBER
RECORD  EQU     15              ;RECORD COUNT
CRECORD EQU     32              ;CURRENT RECORD
R0      EQU     33              ;RANDOM RECORD NUMBER LOW
R1      EQU     34              ;RANDOM RECORD NUMBER HI
R2      EQU     35              ;RANDOM RECORD NUMBER OVERFLOW

;       RAM LOCATIONS

IOBYTE  EQU     3               ;IOBYTE
CLOCK   EQU     8               ;CLOCK JUMP VECTOR
TICCNT  EQU     0BH             ;TIC COUNTER
H88CTL  EQU     0DH             ;H88 CONTROL BYTE
H8CTL   EQU     0EH             ;H8 CONTROL BYTE
CRTINT  EQU     18H             ;CRT INTERRUPT VECTOR

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                               ;
;       HDOS DEFINITIONS        ;
;                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;       HDOS SCALL DEFINITIONS

;                               ;RESIDENT FUNCTIONS
xEXIT   EQU     000Q            ;EXIT TO O-S
xSCIN   EQU     001Q            ;INPUT CHR FROM CONSOLE
xSCOUT  EQU     002Q            ;OUTPUT CHR TO CONSOLE
xPRINT  EQU     003Q            ;PRINT LINE ON CONSOLE
xREAD   EQU     004Q            ;READ FROM DISK
xWRITE  EQU     005Q            ;WRITE TO DISK
xCONSL  EQU     006Q            ;SET/CLEAR CONSOLE MODES
xCLRCO  EQU     007Q            ;CLEAR CONSLOE BUFFER
xLOADO  EQU     010Q            ;LOAD OVERLAY
xVERS   EQU     011Q            ;RETURN HDOS VERSION NUMBER
xSYSRES EQU     12Q             ;PRECEDING FUNCTIONS ARE RESIDENT
;                               ;HDOSOVL0xSYS FUNCTIONS
xLINK   EQU     40Q             ;LINK (MUST BE FIRST)
xCTLC   EQU     41Q             ;CNTL-C COMMAND
xOPENR  EQU     42Q             ;OPENR
xOPENW  EQU     43Q             ;OPENW
xOPENU  EQU     44Q             ;OPENU
xOPENC  EQU     45Q             ;OPENC (CONTIGUOUS)
xCLOSE  EQU     46Q             ;CLOSE
xPOSIT  EQU     47Q             ;POSITION
xDELET  EQU     50Q             ;DELETE
xRENAME EQU     51Q             ;RENAME
xSETTP  EQU     52Q             ;SETTOP
xDECODE EQU     53Q             ;NAME DECODE
xNAME   EQU     54Q             ;GET FILE NAME FROM CHANNEL
xCLEAR  EQU     55Q             ;CLEAR CHAN
xCLEARA EQU     56Q             ;CLEARS ALL CHANNELS
xERROR  EQU     57Q             ;LOOKUP ERROR
xCHFLG  EQU     60Q             ;CHANGE FLAGS
xEQUMT  EQU     61Q             ;FLAG SYSTEM DISK DISMOUNTED
xLOADD  EQU     62Q             ;LOAD DD
;                               ;HDOSOVL1xSYS FUNCTIONS
xMOUNT  EQU     200Q            ;MOUNT (MUST BE FIRST)
xDMOUN  EQU     201Q            ;DISMOUNT
xMONMS  EQU     202Q            ;MOUNT/NO MESG
xDMNMS  EQU     203Q            ;DISMOUNT/NO MESG
xRESET  EQU     204Q            ;RESET = DISMOUNT/MOUNT
;                               ;OVERLAY INDICES
OVL0    EQU     0               ;HDOSOVL0xSYS
OVL1    EQU     1               ;HDOSOVL1xSYS

;       HDOS EQUATES

SxSOVR  EQU     2166H           ;STACK OVERFLOW WARNING
STACK   EQU     2280H           ;SYSTEM STACK ADDRESS
STACKL  EQU     STACK-(SxSOVR+2)        ;STACK SIZE
USERFWA EQU     2280H           ;FIRST WORKING ADDRESS FOR USER
;                               ;RAM CELL DEFINITIONS
SxDATE  EQU     20BFH           ;SYSTEM DATE(IN ASCII)
SxDATC  EQU     20C8H           ;CODED DATE
SxTIME  EQU     20CAH           ;(POSS TIME)
SxHIMEM EQU     20CEH           ;HARDWARE HIGH MEMORY ADDRESS+1
SxSYSM  EQU     20D0H           ;FWA RESIDENT SYSTEM
SxUSRM  EQU     20D2H           ;LWA USER MEMORY
SxOMAX  EQU     20D4H           ;MAX OVERLAY SIZE FOR SYSTEM
;                               ;SYMBOLS FOR xCONSL SCALL
CSLxECH EQU     10000000B       ;SUPPRESS ECHO
CSLxWRP EQU     00000010B       ;WRAP LINES AT WIDTH
CSLxCHR EQU     00000001B       ;OPERATE IN CHR MODE
IxCSLMD EQU     0               ;S.CSLMD IS FIRST BYTE
SxCSLMD EQU     20D6H           ;CONSOLE MODE
CTPxBKS EQU     10000000B       ;TERMINAL BACKSPACES
CTPxMLI EQU     00100000B       ;MAP LOWER CASE TO UPPER ON INPUT
CTPxMLO EQU     00010000B       ;MAP LOWER CASE TO UPPER ON OUTPUT
CTPx2SB EQU     00001000B       ;TERMINAL NEEDS 2 STOP BITS
CTPxBKM EQU     00000010B       ;MAP BKSP UPON INPUT TO RUBOUT
CTPxTAB EQU     00000001B       ;TERMINAL SUPPORTS TABS
IxCONTY EQU     1               ;S.CONTY IS 2ND BYTE
SxCONTY EQU     20D7H           ;CONSOLE TYPE FLAGS
IxCUSOR EQU     2               ;S.CUSOR IS 3RD BYTE
SxCUSOR EQU     20D8H           ;CURRENT CURSOR POSITION
IxCONWI EQU     3               ;S.CONWI IS 4TH BYTE
SxCONWI EQU     20D9H           ;CONSOLE WIDTH
COxFLG  EQU     00000001B       ;CTL-O FLAG
CSxFLG  EQU     10000000B       ;CTL-S FLAG
CPxFLG  EQU     00000010B       ;CTL-P FLAG
IxCONFL EQU     4               ;S.CONFL IS 5TH BYTE
SxCONFL EQU     20DAH           ;CONSOLE FLAGS

;*      S.INT - SYSTEM INTERNAL WORKAREA DEFINITIONS.
;
;       THESE CELLS ARE REFERENCED BY OVERLAYS AND MAIN CODE, AND
;       MUST THEREFORE RESIDE IN FIXED LOW MEMORY.

;*      CONSOLE STATUS FLAGS

SxCDB   EQU     20E3H           ;CONSOLE DESCRIPTOR BYTE
CDBxH85 EQU     00000000B
CDBxH84 EQU     00000001B       ;=0 IF H8-5, =1 IF H8-4
SxBAUD  EQU     20E4H           ;[0-14]  H8-4 BAUD RATE, =0 IF H8-5
;               ;               [15]    =1 IF BAUD RATE => 2 STOP BITS

;*      TABLE ADDRESS WORDS

SxDLINK EQU     20E6H           ;ADDRESS OF DATA IN HDOS CODE
SxOFWA  EQU     20E8H           ;FWA  OVERLAY  TABLE
SxCFWA  EQU     20EAH           ;FWA  CHANNEL  TABLE
SxDFWA  EQU     20ECH           ;FWA  DEVICE   TABLE
SxRFWA  EQU     20EEH           ;FWA  RESIDENT HDOS CODE

;*      DEVICE DRIVER DELAYED LOAD FLAGS

SxDDLDA EQU     20F0H           ;DRIVER LOAD ADDRESS (HIGH BYTE=0 IF NO LOAD PENDING)
SxDDLEN EQU     20F2H           ;CODE LENGTH IN BYTES
SxDDGRP EQU     20F4H           ;GROUP NUMBER FOR DRIVER
SxDDDTA EQU     20F6H           ;DEVICE'S ADDRESS IN DEVLST +DEV.RES
SxDDOPC EQU     20F8H           ;OPEN OPCODE PENDEDING

;*      OVERLAY MANAGEMENT FLAGS

OVLxIN  EQU     00000001B       ;IN MEMORY
OVLxRES EQU     00000010B       ;PERMINANTLY RESIDENT
OVLxNUM EQU     00001100B       ;OVERLAY NUMBER MASK
OVLxUCS EQU     10000000B       ;USER CODE SWAPPED FOR OVERLAY

SxOVLFL EQU     20F9H           ;OVERLAY FLAG

;       SYSCALL PROCESSING WORK AREAS

SxCACC  EQU     2106H           ;(ACC) UPON SYSCALL
SxCODE  EQU     2107H           ;SYSCALL INDEX IN PROGRESS

SxMOUNT EQU     211AH           ;<>0 IF THE SYSTEM DISK IS MOUNTED

;*      ACTIVE I/O AREA.
;
;       THE AIO.XXX AREA CONTAINS INFORMATION ABOUT THE I/O OPERATION
;       CURRENTLY BEING PERFORMED. THE INFORMATION IS OBTAINED FROM
;       THE CHANNEL TABLE, AND WILL BE RESTORED THERE WHEN DONE.
;
;       NORMALLY, THE AIO.XXX INFORMATION WOULD BE OBTAINED DIRECTLY
;       FROM VARIOUS SYSTEM TABLES VIA POINTER REGISTERS. SINCE THE
;       8080 HAS NO GOOD INDEXED ADDRESSING, THE DATA IS MANUALLY
;       COPIED INTO THE AIO.XXX CELLS BEFORE PROCESSING, AND
;       BACKDATED AFTER PROCESSING.

AIOxVEC EQU     2120H           ;JUMP INSTRUCTION
AIOxDDA EQU     2121H           ;DEVICE DRIVER ADDRESS
AIOxFLG EQU     2123H           ;FLAG BYTE
AIOxGRT EQU     2124H           ;ADDRESS OF GROUP RESERV TABLE
AIOxSPG EQU     2126H           ;SECTORS PER GROUP
AIOxCGN EQU     2127H           ;CURRENT GROUP NUMBER
AIOxCSI EQU     2128H           ;CURRENT SECTOR INDEX
AIOxLGN EQU     2129H           ;LAST GROUP NUMBER
AIOxLSI EQU     212AH           ;LAST SECTOR INDEX
AIOxDTA EQU     212BH           ;DEVICE TABLE ADDRESS
AIOxDES EQU     212DH           ;DIRECTORY SECTOR
AIOxDEV EQU     212FH           ;DEVICE CODE
AIOxUNI EQU     2131H           ;UNIT NUMBER (0-9)

AIOxDIR EQU     2132H           ;DIRECTORY ENTRY

AIOxCNT EQU     2149H           ;SECTOR COUNT
AIOxEOM EQU     214AH           ;END OF MEDIA FLAG
AIOxEOF EQU     214BH           ;END OF FILE FLAG
AIOxTFP EQU     214CH           ;TEMP FILE POINTERS
AIOxCHA EQU     214EH           ;ADDRESS OF CHANNEL BLOCK (IOC.DDA)

SxBDA   EQU     2150H           ;BOOT DEVICE ADDRESS (SETUP BY ROM) /80.09.GC/
SxSCR   EQU     2151H           ;SYSTEM SCRATCH AREA ADDRESS

;       ERROR CODE DEFINITIONS

ECxHIN  EQU     0               ;HDOS ISSUE NUMBER
ECxEOF  EQU     1               ;END OF FILE
ECxEOM  EQU     2               ;END OF MEDIA
ECxILC  EQU     3               ;ILLEGAL SYSCALL CODE
ECxCNA  EQU     4               ;CHANNEL NOT AVAILABLE
ECxDNS  EQU     5               ;DEVICE NOT SUITABLE
ECxIDN  EQU     6               ;ILLEGAL DEVICE NAME
ECxIFN  EQU     7               ;ILLEGAL FILE NAME
ECxNRD  EQU     8               ;NO ROOM FOR DD
ECxFNO  EQU     9               ;CHANNEL NOT OPEN
ECxILR  EQU     10              ;ILLEGAL REQUEST
ECxFUC  EQU     11              ;FILE USAGE CONFLICT
ECxFNF  EQU     12              ;FILE NAME NOT FOUND
ECxUND  EQU     13              ;UNKNOWN DEVICE
ECxICN  EQU     14              ;ILLEGAL CHANNEL NUMBER
ECxDIF  EQU     15              ;DIRECTORY FULL
ECxIFC  EQU     16              ;ILLEGAL FILE CONTENTS
ECxNEM  EQU     17              ;NOT ENOUGH MEMORY
ECxRF   EQU     18              ;READ FAILURE
ECxWF   EQU     19              ;WRITE FAILURE
ECxWPV  EQU     20              ;WRITE PROTECTION VIOLATION
ECxWP   EQU     21              ;DISK WRITE PROTECTED
ECxFAP  EQU     22              ;FILE ALREADY PRESENT
ECxDDA  EQU     23              ;DEVICE DRIVER ABORT
ECxFL   EQU     24              ;FILE LOCKED
ECxFAO  EQU     25              ;FILE ALREADY OPEN
ECxIS   EQU     26              ;ILLEGAL SWITCH
ECxUUN  EQU     27              ;UNKNOWN UNIT NUMBER
ECxFNR  EQU     28              ;FILE NAME REQUIRED
ECxDIW  EQU     29              ;DEVICE IS NOT WRITABLE (POS LOCK)

;       ASCII EQUIVALENCES

CR      EQU     15Q             ;CARRIAGE RETURN
LF      EQU     12Q             ;LINE FEED
NULL    EQU     200Q            ;PAD CHARACTER
BELL    EQU     7Q              ;BELL
RUBOUT  EQU     177Q            ;
BKSP    EQU     10Q             ;CTL-H
CxSYN   EQU     26Q             ;SYNC
CxSTX   EQU     2Q              ;STX
QUOTE   EQU     47Q
TAB     EQU     11Q
ESC     EQU     33Q
NL      EQU     12Q             ;NEW LINE (HDOS)
ENL     EQU     NL+200Q         ;NL+END OF LINE FLAG
FF      EQU     14Q             ;FORM FEED
CTLA    EQU     01Q             ;CTL-A
CTLB    EQU     02Q             ;CTL-B
CTLC    EQU     03Q             ;CTL-C
CTLD    EQU     04Q             ;CTL-D
CTLO    EQU     'O'-40H
CTLP    EQU     'P'-40H
CTLS    EQU     'S'-40H
CTLQ    EQU     'Q'-40H
CTLU    EQU     'U'-40H

;  USEFUL SUBROUTINES IN H17 ROM.

COMP    EQU     1830H           ;COMPARE (DE) TO (HL) FOR (C) BYTES
DADA    EQU     183AH           ;(HL) = (HL)+(A) REG A IS NOT DESTROYED
DADAx   EQU     1841H           ;(HL) = (HL)+(A) REG A IS DESTROYED
DU66    EQU     1846H           ;(HL) = (BC) / (DE)
HLIHL   EQU     1889H           ;INDIRECT LOAD HL THROUGH HL
CDEHL   EQU     188EH           ;COMPARE (DE) TO (HL) FOR EQUALITY
CHL     EQU     1894H           ;TWO'S COMPLEMENT FOR (HL)
INDL    EQU     189CH           ;LOAD REG DE WITH 2 BYTES AT (HL)+DISPLACEMENT
MOVE    EQU     18AAH           ;FROM (DE) TO (HL) FOR (BC)
MU10    EQU     18D4H           ;MULTIPLY (DE) BY 10 GIVING (HL)
MU66    EQU     18DFH           ;HL = BC * DE
MU86    EQU     1907H           ;HL = DE * A
RSTALL  EQU     1927H           ;RESTORE ALL REGISTERS
SAVALL  EQU     192CH           ;SAVE ALL REGISTERS ON STACK
TJMP    EQU     1931H           ;TABLE JUMP WITH (A) = INDEX
TJMPx   EQU     1932H           ;TABLE JUMP WITH (A) = INDEX*2
TBRA    EQU     193EH           ;BRANCH RELATIVE THROUGH TABLE
TBLS    EQU     1949H           ;SEQUENTIAL TABLE SEARCH
TYPTX   EQU     195EH           ;TYPE TEXT WITH ADDR OF TEXT ON STACK
TYPTXx  EQU     1964H           ;TYPE TEXT WITH ADDR IN REG HL
UDD     EQU     196FH           ;BC = ADDR, A = COUNT, HL = PLACEMENT
ZERO    EQU     198AH           ;HL = ADDR, B = COUNT
FILL    EQU     ZERO+1          ;HL=ADDR, B = COUNT, A = VAL
RxDLY   EQU     1DC3H           ;WAIT (A) * 2 MILLISECONDS

;*      MTR - PAM/8 EQUIVALENCES.
;
;       THIS DECK CONTAINS SYMBOLIC DEFINITIONS USED TO
;       MAKE USE OF THE PAM/8 CODE AND CONTROL BYTESx

;*      IO PORTS

IPxPAD  EQU     360Q            ;PAD INPUT PORT
OPxCTL  EQU     360Q            ;CONTROL OUTPUT PORT
OPxDIG  EQU     360Q            ;DIGIT SELECT OUTPUT PORT
OPxSEG  EQU     361Q            ;SEGMENT SELECT OUTPUT PORT
IPxCON  EQU     362Q            ;H-88/H-89/HA-8-8 Configuration /80.07.gc/
OP2xCTL EQU     362Q            ;H-88/H-89/HA-8-8 Control Port  /80.07.gc/

;*      MONITOR IDENTIFICATION FLAGS
;
;       THESE BYTES IDENTIFY THE ROM MONITOR.
;       THEY ARE THE VARIOUS VALUES OF LOCATION .IDENT

MxPAM8  EQU     021Q            ;'LXI' INSTRUCTION AT 000.000 IN PAM-8
MxFOX   EQU     303Q            ;'JMP' INSTRUCTION AT 000.000 IN FOX ROM

;*      ROUTINE ENTRY POINTS.
;

xIDENT  EQU     0000H           ;IDENTIFICATION LOCATION
xDLY    EQU     002BH           ;DELAY

;*      RAM CELLS USED BY H8MTR.
;

xMFLAG  EQU     2008H           ;USER OPTION BYTE
xCTLFLG EQU     2009H           ;PANEL CONTROL BYTE
xTICCNT EQU     201BH           ;CLOCK TICK COUNTER
xUIVEC  EQU     201FH           ;USER INTERRUPT VECTORS
xNMIRET EQU     2034H           ;H88/H89 NMI Return Address     /80.07.gc/
xCTL2FL EQU     2036H           ;OP2.CTL Control Byte           /80.07.gc/
UIVEC1  EQU     xUIVEC          ;CLOCK VECTOR
UIVEC2  EQU     UIVEC1+3        ;SINGLE STEP VECTOR
UIVEC3  EQU     UIVEC2+3        ;CONSOLE VECTOR
UIVEC4  EQU     UIVEC3+3        ;H37 VECTOR
UIVEC5  EQU     UIVEC4+3        ;MODEM VECTOR
UIVEC6  EQU     UIVEC5+3        ;USER VECTOR
UIVEC7  EQU     UIVEC6+3        ;SCALL VECTOR

        IF      DBUG
SCALL   EQU     0F7H            ;USER RST 6 DURING DEBUGGING
        ENDIF
        IF      NOT DBUG
SCALL   EQU     0FFH            ;HDOS SYSTEM CALL (RST 7)
        ENDIF


        ORG     100H

;;;;;;;;;;;;;;;;;;;;;;;;;
;                       ;
;       MAIN CODE       ;
;                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;

;       INITIALIZATION

START   JMP     START1          ;JUMP OVER BUFFER
INCNT   DB      0               ;INPUT COUNTER
INBUF   DS      100             ;INPUT BUFFER
START1  LXI     SP,STACK        ;SET HDOS STACK
        LXI     H,2000H
        LXI     B,SxSOVR-2000H
        MVI     D,0             ;GET A ZERO
ZRAM    MOV     M,D             ;ZERO HDOS TABLES
        INX     H
        DCX     B
        MOV     A,B
        ORA     C
        JNZ     ZRAM
        LXI     H,IOCM1         ;POINT TO CHANNEL TABLE
        SHLD    SxCFWA          ;SET UP CFWA
        MVI     C,7             ;SEVEN IO CHANNELS
ZIOC    INX     H               ;SKIP OVER ADDRESS
        INX     H
        MVI     B,40
        CALL    ZERO            ;CLEAR AN IOC
        DCR     C               ;TEST IF DONE
        JNZ     ZIOC            ;IF NOT, CONTINUE
        LXI     D,DATE
        LXI     H,SxDATE
        LXI     B,DATLEN
        CALL    MOVE            ;MOVE IN DUMMY DATE
        LXI     H,DEVTBL        ;GET DEVICE TABLE ADDRESS
        SHLD    SxDFWA          ;PUT IT IN MEMORY
        LXI     H,SxCONTY       ;POINT TO CONSOLE TYPE BYTE
        MVI     M,CTPxBKS+CTPxBKM+CTPxTAB       ;SET IT UP
        INX     H               ;SKIP OVER CURSOR POSITION
        INX     H               ;ON TO CONSOLE WIDTH
        MVI     M,255           ;SET IT TO MAX
        MVI     A,1
        STA     SxCDB           ;SAY CONSOLE IS H8-4
        STA     SxMOUNT         ;FLAG SYSTEM MOUNTED
        MVI     A,1111B
        STA     SxOVLFL         ;FLAG OVERLAYS LOADED
        MVI     A,320Q          ;SET H8 HARDWARE BITS
        STA     xCTLFLG         ;IN .CTLFLG
        MVI     A,0C3H          ;JUMP INSTRUCTION
        STA     AIOxVEC         ;SET UP AIO VECTOR
        STA     xDLY            ;SET UP JUMP TO DLY
        IF      NOT Z100
        STA     20Q
        ENDIF
        STA     50Q
        STA     60Q
        IF      NOT DBUG
        STA     70Q             ;PUT JMP INSTRUCTIONS AT RST VECTORS
        ENDIF
        STA     66H             ;NMI INTERRUPT
        MVI     B,19
        LXI     H,xUIVEC
        CALL    FILL            ;ALSO .UIVEC AREA
        STA     2040H           ;ALLOW HDOS RETURN VIA 2040H
        LXI     H,RxDLY
        SHLD    xDLY+1          ;SET UP JUMP TO RxDLY
        LXI     H,EXIT
        SHLD    2041H           ;ALLOW RETURN VIA JMP 2040H
        LXI     H,INTRET        ;INTERRUPT RETURN
        SHLD    UIVEC6+1        ;SET UP USER VECTOR RETURN
        SHLD    UIVEC5+1        ;AND MODEM VECTOR RETURN
        LXI     H,MYCLOCK
        SHLD    UIVEC1+1        ;SET JUMP TO LOCAL CLOCK
        LXI     H,SCPROC
        IF      DBUG
        SHLD    UIVEC6+1
        ENDIF
        IF      NOT DBUG
        SHLD    UIVEC7+1        ;SET JUMP TO SCALL PROCESSOR
        LXI     H,UIVEC7
        SHLD    71Q             ;SET SCALL VECTOR JUMP
        ENDIF
        IF      Z100
        LXI     H,SSTEP
        SHLD    61Q             ;Z100'S USE RST 6 FOR SSTEP
        ENDIF
        IF      NOT Z100
        LXI     H,UIVEC6
        SHLD    61Q             ;SET IN USER VECTOR JUMP
        LXI     H,SSTEP
        SHLD    21Q             ;AND SINGLE STEP VECTOR JUMP
        LXI     H,NMI
        SHLD    67H             ;AND NON-MASK INT VECTOR
        DI                      ;KILL INTERRUPTS FOR NEXT
        LHLD    CLOCK+1         ;GET CP/M CLOCK VECTOR
        SHLD    CLKRET          ;SAVE AS CLOCK RETURN
        LXI     H,UIVEC1        ;GET CLOCK VECTOR ADDRESS
        SHLD    CLOCK+1         ;PUT IT IN CP/M VECTOR
        EI                      ;RESTORE INTERRUPTS
        ENDIF
        LXI     H,UIVEC5
        SHLD    51Q             ;SET IN MODEM VECTOR JUMP
        MVI     A,NL
        DB      SCALL,xSCOUT
        MVI     H,' '
        XRA     A
        DB      SCALL,xERROR    ;SIGN ON
        CALL    TYPTX
        DB      ' by PS:',NL,NL,'System has',' '+80H
        LXI     H,0FFH
FMEMTP  DCR     H               ;FIND TOP OF MEMORY
        MOV     A,M
        INR     M
        CMP     M
        MOV     M,A
        JZ      FMEMTP
        SHLD    SxHIMEM         ;SAVE LWA
        INX     H               ;HL = LWA + 1
        MOV     A,H
        SUI     40Q             ;CONVERT TO NO. OF K
        RAR
        RAR
        ANI     77Q
        LXI     H,RAMNO         ;PUT NO. OF K HERE
        MOV     C,A
        MVI     B,0             ;BC = NO. OF K
        MVI     A,2             ;TWO DIGITS
        CALL    UDD             ;CONVERT TO DECIMAL
        CALL    TYPTX
RAMNO   DB      '00k of RAM.',NL,ENL
        LHLD    BDOS+1          ;GET BDOS ENTRY POINT
        LXI     D,-512
        DAD     D               ;SUBTRACT 512 BYTES
        SHLD    SxSCR           ;SAVE AS SCRATCH RAM START
        SHLD    SxDLINK         ;AND AS DLINK ADDRESS
        SHLD    SxRFWA          ;AND AS HDOS CODE START
        SHLD    SxSYSM          ;AND AS SYSTEM FWA
        LXI     H,80H           ;POINT TO COMMAND LINE
        MOV     A,M             ;GET COUNT
        ORA     A               ;ANY ARGUMENT?
        JZ      SYSCMD          ;NO, START HRUN
        INX     H               ;ELSE, POINT TO ARGUMENT
        JMP     RUN             ;AND RUN IT

;       SYSCMD - PROCESS SYSTEM COMMANDS
;       THIS PART OF THE CODE PROCESSES THE COMMANDS
;       RUN, CAT, DIR, RESET, LOAD, TYPE, COPY, DELETE, RENAME 
;       AND BYE (RETURN TO CP/M)

SYSCMD  LXI     SP,STACK        ;SET HDOS STACK
        LXI     H,0             ;GET A ZERO
        PUSH    H               ;SAVE IT
        MVI     A,1
        DB      SCALL,xCTLC     ;CLEAR CONTROL-A
        POP     H
        MVI     A,2
        DB      SCALL,xCTLC     ;CLEAR CONTROL-B
        LXI     H,SYCTLC        ;GET OUR CONTROL-C PROCESSOR
        MVI     A,3
        DB      SCALL,xCTLC     ;SET CONTROL-C EXIT
        XRA     A
        STA     PFLG            ;CLEAR PIP FLAG
        LHLD    SxRFWA
        SHLD    SxSYSM          ;RESET SYSTEM FWA
        LXI     H,2280H
        SHLD    SxUSRM          ;SET USER MEMORY LIMIT
        XRA     A
        STA     SxCSLMD         ;SET LINE INPUT MODE
        LDA     INCNT           ;GET INPUT COUNT
        ORA     A               ;CHARACTERS IN BUFFER?
        JNZ     SYS1            ;YES, EXECUTE THEM
        LDA     INTFLG          ;GET INTERNAL FLAG
        ORA     A               ;IN INTERNAL MODE?
        JNZ     SYS1            ;YES, SKIP SUBMIT CHECK
        LXI     H,SUBSUB        ;POINT TO "SUB.SUB"
        LXI     D,DEFALT        ;AND DEFAULTS
        MVI     A,-1 AND 0FFH   ;USE CHANNEL -1
        DB      SCALL,xOPENR    ;TRY TO OPEN "SUB.SUB"
        JC      SYS0            ;IT ISN'T THERE
        LXI     H,SUBMIT        ;POINT TO "SUBMIT"
        JMP     RUN             ;TRY TO RUN IT
SYS0    MVI     A,1
        STA     INTFLG          ;FLAG INTERNAL MODE
SYS1    LDA     SxCUSOR         ;GET CURSOR POSITION
        DCR     A               ;AT COLUMN ONE?
        JZ      PROMPT          ;IF SO, PROMPT ENTRY
        MVI     A,NL            ;ELSE, PRINT NEW LINE
        DB      SCALL,xSCOUT
PROMPT  MVI     A,'>'
        DB      SCALL,xSCOUT    ;PRINT PROMPT
        LXI     H,CMDCNT        ;POINT TO INPUT COUNT
        MVI     M,0             ;CLEAR IT
        INX     H               ;MOVE TO BUFFER
        MVI     B,100           ;CLEAR 100 CHARACTERS
        CALL    ZERO            ;CLEAR BUFFER
        LXI     H,CMDBUF        ;POINT TO INPUT BUFFER
SYIN    DB      SCALL,xSCIN     ;GET A CHARACTER
        JC      SYIN
        CALL    MCU             ;MAP TO UPPER CASE
        MOV     M,A             ;STORE IT
        INX     H               ;INCREMENT POINTER
        PUSH    H               ;SAVE HL
        LXI     H,CMDCNT        ;GET COMMAND INPUT COUNTER
        INR     M               ;INCREMENT IT
        POP     H               ;RESTORE HL
        CPI     CTLD            ;IS IT CONTROL-D?
        JZ      SYCTLC1         ;IF SO, START OVER
        CPI     NL              ;END OF ENTRY?
        JNZ     SYIN            ;NO, KEEP GOING
        LXI     H,CMDBUF        ;POINT TO INPUT BUFFER
        MOV     A,M             ;GET FIRST CHARACTER
        CPI     NL              ;NULL ENTRY?
        JZ      SYSCMD          ;IF SO, START OVER
        LDA     CMDCNT          ;GET COUNT OF CHARACTERS
        DCR     A               ;DO NOT COUNT NEW-LINE
        PUSH    H               ;SAVE IT
        CALL    DADA            ;FIND END OF ENTRY
        MVI     M,0             ;TERMINATE ENTRY
        POP     H               ;RESTORE BUFFER ADDRESS
        MVI     C,0             ;CLEAR A COUNTER
FNALP   MOV     A,M             ;GET A CHARACTER
        CPI     'A'             ;LESS THAN A?
        JC      NOTA            ;NOT ALPHA
        CPI     'Z'+1           ;MORE THAN z?
        JNC     NOTA            ;NOT ALPHA
        MOV     M,A             ;REPLACE CHARACTER
        INR     C               ;INCREMENT COUNTER
        INX     H               ;AND POINTER
        JMP     FNALP           ;TRY NEXT CHARACTER
NOTA    PUSH    H               ;SAVE ADDRESS OF END OF COMMAND
        MVI     B,CMDEND-CMDS   ;GET LENGTH TO SEARCH
        LXI     D,CMDBUF        ;POINT TO COMMAND
        LXI     H,CMDS          ;POINT TO COMMAND STRINGS
        PUSH    B               ;SAVE COUNT
        CALL    INSTR           ;LOOK FOR A VALID COMMAND
        POP     B
        POP     D               ;GET ADDRESS OF END OF COMMAND
        JNZ     NOMATCH         ;COULDN'T MATCH COMMAND
        MOV     A,M             ;GET COMMAND SIZE
        CMP     C               ;RIGHT COMMAND LENGTH?
        JNZ     NOMATCH         ;NO
        INX     H               ;MOVE TO TABLE KEY
        MOV     A,M             ;GET IT
        CPI     12              ;INDEX TOO BIG?
        JNC     NOMATCH         ;YES, NO MATCH
        XCHG                    ;HL = END OF COMMAND
        CALL    TJMP            ;JUMP TO COMMAND
        DW      RUN
        DW      CAT
        DW      TYPE
        DW      RESETx
        DW      COPY
        DW      LOAD
        DW      BYE
        DW      DEL
        DW      REN
        DW      DATEx
        DW      VERCMD
NOMATCH LXI     H,CMDBUF        ;NO MATCH, GET BUFFER

;       COULDN'T MATCH ENTRY, MUST BE RUN

RUN     CALL    SOB             ;SKIP BLANKS
        SHLD    NAMADR          ;SAVE FILE NAME ADDRESS
        MVI     B,17            ;MAX FILE NAME LENGTH
RUN0    MOV     A,M             ;LOOK FOR FOLLOWING SPACE
        ORA     A               ;END OF ENTRY?
        JZ      RUN2            ;IF SO, NO ARGUMENT
        CALL    CFD+2           ;CHECK FOR DELIMITER
        JZ      RUN1            ;FOUND ONE
        INX     H
        DCR     B
        JNZ     RUN0
        JMP     RUN2            ;DIDN'T FIND ONE
RUN1    SHLD    ARGADR          ;SAVE ARGUMENT ADDRESS
        MVI     C,0             ;CLEAR A COUNTER
RUN1A   MOV     A,M             ;LOOK FOR END
        ORA     A
        JZ      RUN1B
        INX     H
        INR     C
        JMP     RUN1A
RUN1B   INR     C               ;INCLUDE TRAILING ZERO
        MVI     B,0             ;BC = COUNT
        MOV     L,C
        MOV     H,B             ;HL = COUNT ALSO
        CALL    CHL             ;COMPLEMENT IT
        XCHG                    ;DE = COUNT COMPLEMENT
        LXI     H,STACK         ;HL = NORMAL STACK
        DAD     D               ;SUBTRACT ARGUMENT COUNT
        SPHL                    ;PUT STACK THERE
        XCHG                    ;DE = STACK ADDRESS
        LHLD    ARGADR          ;GET ADDRESS OF ARGUMENT
        XCHG                    ;HL = STACK ADDRESS
        CALL    MOVE            ;MOVE IT TO STACK SPACE
RUN2    LHLD    NAMADR          ;GET FILE NAME
        LXI     D,DEFALT        ;AND DEFAULTS
        XRA     A
        STA     INTFLG          ;CLEAR INTERNAL FLAG
        DB      SCALL,xLINK     ;TRY TO LINK TO FILE
        MVI     A,1
        STA     INTFLG          ;SET INTERNAL FLAG
        LDA     PFLG
        ORA     A               ;IS THIS A PIP COMMAND?
        JNZ     RUNP            ;YES, DIFFERENT ERROR MESSAGE
        CALL    TYPTX
        DB      BELL,NL,'Illegal command.',ENL
        JMP     SYSCMD          ;TRY AGAIN
RUNP    CALL    TYPTX
        DB      BELL,NL,'PIP.ABS is required for this command.',ENL
        JMP     SYSCMD          ;TRY AGAIN
NAMADR  DW      0               ;FILE NAME ADDRESS
ARGADR  DW      0               ;ARGUMENT ADDRESS
DEFALT  DB      'SY0ABS'
SUBSUB  DB      'SUB.SUB',0
SUBMIT  DB      'SUBMIT',0
PFLG    DB      0               ;PIP FLAG
INTFLG  DB      0               ;INTERNAL FLAG

CMDS    DB      'RUN',3,0
        DB      'CAT',3,1
        DB      'TYPE',4,2
        DB      'RESET',5,3
        DB      'COPY',4,4
        DB      'LOAD',4,5
        DB      'BYE',3,6
        DB      'DEL',3,7
        DB      'DELETE',6,7
        DB      'REN',3,8
        DB      'RENAME',6,8
        DB      'DATE',4,9
        DB      'VER',3,10
CMDEND  EQU     $-1

;       BYE - RETURN TO CP/M

BYE     CALL    TYPTX
        DB      NL,'Replace CP/M system disk, hit RETURN','.'+80H
        DB      SCALL,xSCIN
        JC      $-2             ;WAIT FOR RETURN
        IF      NOT Z100
        DI                      ;KILL INTERRUPTS
        LHLD    CLKRET
        SHLD    CLOCK+1         ;REPLACE OLD CLOCK
        EI
        ENDIF
        LXI     H,0C9FBH        ;EI, RET
        SHLD    50Q             ;REPLACE ORIGINAL INT. RETURNS
        SHLD    60Q
        IF      NOT DBUG
        SHLD    70Q
        ENDIF
        IF      NOT Z100
        SHLD    20Q
        ENDIF
        MVI     C,0             ;RETURN TO CP/M
        CALL    BDOS

;       LOAD - LOAD DEVICE DRIVERS

LOAD    JMP     SYSCMD          ;NOT IMPLEMENTED

;       COPY - COPY FILES

COPY    MVI     A,1
        STA     PFLG            ;MARK AS PIP COMMAND
        LXI     H,' P'          ;PUT "PIP"
        SHLD    CMDBUF          ; IN INPUT BUFFER
        LXI     H,'IP'
        SHLD    CMDBUF+2
        LXI     H,CMDBUF                ;POINT TO BUFFER
        JMP     RUN             ;AND RUN PIP TO COPY

;       TYPE - TYPE FILE ON CONSOLE

TYPE    MVI     A,1
        STA     PFLG            ;MARK AS PIP COMMAND
        PUSH    H               ;SAVE POINTER
        LXI     H,CMDBUF
        LDA     CMDCNT
        DCR     A
        CALL    DADA            ;FIND END OF ARGUMENT
        LXI     D,SUPCMD        ;GET "/SUP"
        LXI     B,4
        CALL    MOVE            ;MOVE IT IN
        POP     H
        CALL    SOB             ;SKIP BLANKS
        LXI     D,CMDBUF+8      ;PUT ARGUMENT HERE
        XCHG
        LXI     B,50            ;MOVE 50 CHARACTERS
        CALL    MOVE
        LXI     D,TYPCMD        ;GET TYPE COMMAND
        LXI     H,CMDBUF        ;PUT IT HERE
        PUSH    H               ;SAVE ADDRESS
        LXI     B,8             ;8 CHARACTERS
        CALL    MOVE            ;MOVE IT IN
        POP     H               ;GET CMDBUF ADDRESS
        JMP     RUN             ;AND RUN IT
TYPCMD  DB      'PIP TT:='
SUPCMD  DB      '/SUP'

;       DEL - DELETE FILE

DEL     MVI     A,1
        STA     PFLG            ;MARK AS PIP COMMAND
        CALL    SOB             ;SKIP BLANKS
        PUSH    H               ;SAVE ARGUMENT ADDRESS
        LXI     H,CMDBUF        ;POINT TO INPUT BUFFER
        LDA     CMDCNT          ;GET COUNT OF CHARACTERS
        DCR     A               ;BACK UP TO TRAILING ZERO
        CALL    DADA            ;MOVE POINTER THERE
        LXI     D,DELCMD1       ;GET "/DEL"
DEL1    LXI     B,4             ;MOVE 4 CHARACTERS
        CALL    MOVE
        POP     D               ;DE = ORIGINAL ARGUMENT ADDRESS
        LXI     H,CMDBUF+20     ;PUT IT HERE
        LXI     B,50            ;MOVE 50 CHARACTERS
        CALL    MOVE
        LXI     H,CMDBUF+16     ;PUT "PIP" HERE
        PUSH    H               ;SAVE THIS ADDRESS
        LXI     D,DELCMD2       ;GET "PIP"
        LXI     B,4             ;4 CHARACTERS
        CALL    MOVE            ;MOVE IT IN
        POP     H               ;GET POINTER
        JMP     RUN             ;RUN THE COMMAND
DELCMD1 DB      '/DEL'
DELCMD2 DB      'PIP '


;       REN - RENAME FILE

REN     MVI     A,1
        STA     PFLG            ;MARK AS PIP COMMAND
        CALL    SOB             ;SKIP BLANKS
        PUSH    H               ;SAVE ARGUMENT ADDRESS
        LXI     H,CMDBUF        ;POINT TO INPUT BUFFER
        LDA     CMDCNT          ;GET INPUT COUNT
        DCR     A               ;IGNORE TRAILING ZERO
        CALL    DADA            ;MOVE POINTER THERE
        LXI     D,RENCMD        ;GET "/REN"
        JMP     DEL1            ;USE DEL TO PROCESS
RENCMD  DB      '/REN'

;       CAT - DIRECTORY WITH PIP.ABS

CAT     MVI     A,1
        STA     PFLG            ;MARK AS PIP COMMAND
        CALL    SOB             ;SKIP BLANKS
        PUSH    H               ;SAVE ARGUMENT ADDRESS
        LXI     H,CMDBUF        ;POINT OT INPUT BUFFER
        LDA     CMDCNT          ;GET INPUT COUNT
        DCR     A               ;IGNORE TRAILING ZERO
        CALL    DADA            ;MOVE POINTER THERE
        LXI     D,CATCMD        ;GET "/L"
        JMP     DEL1            ;USE DEL TO PROCESS
CATCMD  DB      '/L',0,0

;       RESETx - RESET DRIVES
;
;       ENTRY:
;       HL = DEVICE (DRIVE) DESCRIPTOR)
;
;       EXIT -- NONE

RESETx  CALL    SOB             ;SKIP BLANKS
        DB      SCALL,xRESET
        JMP     SYSCMD

;       RESET SYSTEM CALL

RESET1  
        LDA     SxCSLMD         ;GET CURRENT CONSOLE MODE
        MOV     B,A             ;SAVE IN B
        XRA     A
        STA     SxCSLMD         ;SET LINE MODE
        CALL    TYPTX
        DB      NL,'Replace disk in',' '+80H
        CALL    PDEV            ;PRINT DEVICE
        CALL    TYPTX
        DB      ' and hit RETURN','.'+80H
        DB      SCALL,xSCIN
        JC      $-2
        MVI     C,RESET
        CALL    CBDOS           ;RESET DISK SYSTEM
        MOV     A,B
        STA     SxCSLMD         ;RESTORE OLD CONSOLE MODE
        RET

PDEV    PUSH    H
        PUSH    B
        MVI     B,3
PDEV0   MOV     A,M
        DB      SCALL,xSCOUT
        INX     H
        DCR     B
        JNZ     PDEV0
        MVI     A,':'
        DB      SCALL,xSCOUT    ;PRINT COLON
        POP     B
        POP     H
        RET

;       VER - REPORT HDOS VERSION

VERCMD  MVI     M,0             ;KILL ANY ARGUMENT
;       JMP     DATE            ;USE DATE TO REPORT VERSION

;**     DATE - PROCESS DATE COMMAND.
;
;       DATE                    ;PRINT DATE
;       DATE MM-DDD-YY          ;SET DATE

DATEx   CALL    SOB
        MOV     A,M
        ANA     A
        JZ      DATE2A          ;HE JUST WANTS TO KNOW THE DATE

;       SET NEW DATE

        CALL    CAD             ;CODE AUGUSTAN DATE
        JNC     DATE2           ;OK
        CALL    TYPTX
        DB      BELL,'Illegal Date Format',ENL
        JMP     DATE2A

DATE2   XCHG
        SHLD    SxDATC
        XCHG
        LXI     H,SxDATE
        CALL    DAD0            ;DECODE INTO ASCII
DATE2A  MVI     A,NL
        DB      SCALL,xSCOUT
        CALL    DATE3
        JMP     SYSCMD

;       DISPLAY THE CURRENT DATE

DATE3   CALL    TYPTX
        DB      'HRUN  HDOS '
        DB      (VER SHR 4) + '0'
        DB      '.'
        DB      (VER AND 0FH) + '0'
        DB      ' Emulator, Version 1.0',' '+80H
        LXI     H,SxDATE
        MVI     B,9
DATE4   MOV     A,M             ;PRINT CURRENT DATE
        DB      SCALL,xSCOUT
        INX     H
        DCR     B
        JNZ     DATE4
        RET
;*      CAD - CODE AUGUSTAN DATE.
;
;       CAD IS CALLED TO CODE AN AUGUSTAN DATE INTO THE FORM:
;
;
;       ----------------------------------------
;       I        7 BITS  I  4 BITS  I  5 BITS  I
;       ----------------------------------------
;               YEAR          MON       DAY
;                00-99       1-12       1-31
;
;       FROM THE FORM:
;
;       DD-MMM-YY
;
;       ENTRY   (HL) = ADDRESS OF STRING
;       EXIT    'C' CLEAR IF OK
;                (DE) = 15 BIT VALUE
;                (HL) ADVANCED PAST '-YY'
;               'C' SET IF ERROR
;       USES    ALL

CAD     PUSH    H                                               ;/80.08.GC/
        MVI     C,CADBL                                         ;/80.08.GC/
        LXI     D,CADB                                          ;/80.08.GC/
        CALL    COMP                                            ;/80.08.GC/
        JNZ     CAD0            ;Is not 'No-Date'               ;/80.08.GC/
        POP     D                                               ;/80.08.GC/
        LXI     D,0             ;0  =>  No Date                 ;/80.08.gc/
        ANA     A               ;Clear 'C'                      ;/80.08.gc/
        RET                                                     ;/80.08.GC/

CAD0    POP     H                                               ;/80.08.GC/
        CALL    DDD             ;DECODE DECIMAL DIGITS
        RC                      ;ERROR
        MOV     A,D
        ANA     A
        STC                     ;ASSUME TOO LARGE
        RNZ                     ;TOO LARGE
        MOV     A,E
        ANA     A
        STC
        RZ                      ;TOO SMALL FOR DD
        CPI     32
        CMC     
        RC                      ;TOO LARGE
        XCHG                    ;(HL) = DAY
        MVI     A,100000B
        ADD     L
        MOV     L,A             ;COUNT 1ST MONTH
        XCHG                    ;(DE) = DD*16+1, (HL) = ADDRESS

;       DECODE MONTH

        PUSH    D               ;SAVE DD*16+1
        MOV     A,M
        INX     H
        CPI     '-'
        JNZ     CAD2            ;FORMAT ERROR
        LXI     D,CADA          ;(DE) = MONTH TABLE ADDRESS
        MVI     B,3
        PUSH    H
CAD0A   MOV     A,M             ;CAPITALIZE ENTRY
        MOV     M,A
        INX     H
        DCR     B
        JNZ     CAD0A
        POP     H
CAD1    LXI     B,3
        PUSH    H               ;SAVE TEXT ADDRESS, CADA ADDRESS
        PUSH    D
        CALL    COMP            ;COMPARE
        POP     D               ;(DE) = *CADA* ADDRESS
        JZ      CAD3            ;GOT MONTH
        POP     H               ;(HL) = BUFFER ADDRESS OF MMM-YY
        INX     D
        INX     D
        INX     D               ;TRY NEXT MONTH
        XTHL
        MVI     A,100000B
        CALL    DADAx           ;COUNT MONTH
        XTHL
        LDAX    D               ;(A) = ENTRY IN CADA
        ANA     A
        JNZ     CAD1            ;MORE MONTHS TO GO

;       ERROR

CAD2    POP     H               ;CLEAR STACK
        STC
        RET                     ;FLAG ERROR

;       CRACK -YY

CAD3    POP     B               ;DISCARD ADDRESS IF MMM-YY
        MOV     A,M
        CPI     '-'
        JNZ     CAD2            ;NOT -
        INX     H
        CALL    DDD             ;DECODE DECIMAL DIGITS
        JC      CAD2            ;IF ERROR
        MOV     A,D
        ANA     A
        JNZ     CAD2            ;ERROR
        MOV     A,E             ;(A) = YEAR
        JC      CAD2            ;ERROR
        CPI     100             ;SKW- Y2K FIX
        JNC     CAD2            ;TOO LARGE
        POP     D               ;(DE) = MONTH AND DAY
        ADD     A               ;(A) = YEAR*2
        ADD     D
        MOV     D,A             ;MERGE WITH REST OF IT
        RET

CADA    DS      0               ;TABLE OF MONTHS
        DB      'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC',0

CADB    DB      ' No-Date '                                     ;/80.08.GC/
CADBL   EQU     $-CADB                                          ;/80.08.GC/

;*      DAD - DECODE AUGUSTAN DATE.
;
;       DAD DECODES A 15 BIT DATE CODE OF THE FORMAT:
;
;       ----------------------------------------
;       I        7 BITS  I  4 BITS  I  5 BITS  I
;       ----------------------------------------
;               YEAR          MON       DAY
;                00-99       1-12       1-31
;
;       TO THE FORM:
;
;       DD-MMM-YY
;
;       ENTRY   (DE) = 16 BIT VALUE
;               (HL) = ADDRESS FOR DECODE
;       EXIT    'C' CLEAR IF OK
;                (DE) = (DE)+9
;               'C' SET IF ERROR
;       USES    ALL

DAD0    MOV     A,D                                             ;/80.08.gc/
        ORA     E                                               ;/80.08.gc/
        JZ      DAD2            ;No-Date                        ;/80.08.gc/
        MOV     B,D
        MOV     C,E
        LXI     D,32
        PUSH    H               ;SAVE ADDRESS
        CALL    DU66            ;(DE) = DAY, (HL) = YEAR & MONTH
        XTHL                    ;(HL) = ADDRESS
        MOV     B,D
        MOV     C,E
        MOV     A,E
        ANA     A
        JZ      DAD1            ;BAD VALUE
        MVI     A,2
        CALL    UDD             ;UNPACK DAY
        MVI     M,'-'
        INX     H
        POP     B               ;(BC) = YEAR & MONTH
        LXI     D,16
        PUSH    H               ;SAVE ADDRESS
        CALL    DU66
        XTHL                    ;(HL) = ADDRESS, ((SP)) = YEAR
        MOV     A,E
        ADD     A
        ADD     E               ;(A) = 3*MONTH
        JZ      DAD1            ;BAD VALUE
        CPI     13*3
        JNC     DAD1            ;TOO LARGE
        XCHG                    ;(DE) = ADDRESS
        LXI     H,DADB-3
        CALL    DADAx           ;(HL) = ADDRESS OF MONTH
        LXI     B,3
        XCHG                    ;(HL) = BUFFER ADDR, (DE) = ADDR IN 'DADB'
        CALL    MOVE            ;MOVE MONTH IN
        MVI     M,'-'
        INX     H
        POP     B               ;(BC) = YEAR
        MOV     A,C
        ADI     0
        CPI     100
        CMC
        RC                      ;TOO LARGE
        MOV     C,A             ;(BC) = YEAR
        MVI     A,2
        CALL    UDD             ;UNPACK YEAR
        ANA     A
        RET

;       ILLEGAL FORMAT

DAD1    POP     H               ;RESTORE STACK
        STC                     ;FLAG ERROR
        RET

;       No-Date                                                 ;/80.08.gc/

DAD2    LXI     B,DADCL                                         ;/80.08.gc/
        LXI     D,DADC                                          ;/80.08.gc/
        JMP     MOVE                                            ;/80.08.gc/

DADB    DB      'JanFebMarAprMayJunJulAugSepOctNovDec'

DADC    DB      ' No-Date '                                     ;/80.08.gc/
DADCL   EQU     $-DADC                                          ;/80.08.gc/
;*      DDD - DECODE DECIMAL DIGITS.
;
;       DDD DECODES A STRING OF DECIMAL DIGITS INTO A DECIMAL INTEGER.
;
;       THE CHARACTERS ARE TAKEN OUT OF MEMORY. CONVERSION STOPS WITH THE
;       FIRST NON-DIGIT CHARACTER FOUND.
;
;       ENTRY   (HL) = ADDRESS OF CHARACTERS
;       EXIT    'C' CLEAR IF OK
;                (DE) = NUMBER
;                (HL) = INDEX OF FIRST NON-DIGIT ENCOUNTERED
;               'C' SET IF ERROR
;       USES    A,F,D,E,H,L

DDD     LXI     D,0             ;(DE) = ACCUM

DDD1    MOV     A,M
        SUI     '0'
        CMC
        RNC                     ;TOO SMALL
        CPI     10
        RNC                     ;TOO LARGE
        INX     H               ;ADVANCE ADDRESS
        PUSH    H               ;SAVE (HL)
        CALL    MU10            ;(HL) = ACCUM*10
        XCHG                    ;(DE) = ACCUM
        POP     H               ;(HL) = ADDRESS OF STRING
        RC                      ;OVERFLOW
        ADD     E
        MOV     E,A
        MVI     A,0
        ADC     D
        MOV     D,A
        JNC     DDD1            ;NOT OVERFLOW
        RET

;       LOCAL CLOCK PROCESSOR

MYCLOCK PUSH    H               ;SAVE HL
        LHLD    xTICCNT         ;GET OUR TIC COUNTER
        INX     H               ;INCREMENT IT
        SHLD    xTICCNT         ;UPDATE IT
        POP     H               ;RESTORE HL
        JMP     0               ;JUMP TO OLD CLOCK
CLKRET  EQU     $-2

;       SINGLE STEP INTERRUPT PROCESSOR

SSTEP   PUSH    H               ;SAVE REGISTERS
        PUSH    D
        PUSH    B
        PUSH    PSW
;       LXI     H,xCTLFLG       ;POINT TO CONTROL BYTE
;       MOV     A,M             ;GET IT
;       ORI     20Q             ;SET SINGLE STEP INHIBIT
;       OUT     OPxCTL          ;CLEAR SSTEP
;       MOV     M,A             ;REPLACE BYTE
        LXI     H,10
        DAD     SP              ;ADD 10 TO CURRENT SP
        PUSH    H               ;SAVE ON STACK
        EI
        IF      Z100
        JMP     UIVEC6
        ENDIF
        IF      NOT Z100
        JMP     UIVEC2          ;EXIT TO USER PROGRAM
        ENDIF

;       INTERRUPT RETURN

INTRET  EI
        RET

;       NON-MASKABLE INTERRUPT HANDLER

NMI     XTHL                    ;GET RETURN ADDR
        SHLD    xNMIRET         ;SAVE IT
        XTHL                    ;REPLACE IT
        PUSH    H               ;SAVE REGISTERS
        PUSH    B
        PUSH    PSW
        MOV     B,A             ;SAVE A
        LHLD    xNMIRET         ;GET RETURN ADDR
        DCX     H               ;BACK UP TO PORT NO.
        MOV     A,M             ;GET IT
        CPI     OPxCTL          ;PORT 360Q?
        JNZ     NMIX            ;NO, EXIT
        DCX     H               ;BACK UP TO INSTRUCTION
        MOV     A,M             ;GET IT
        CPI     333Q            ;IN?
        JNZ     NMI1            ;NO, TRY OUT
        POP     PSW             ;ELSE, GET PSW
        MVI     A,10101010B     ;SET UNIQUE BIT PATTERN
        JMP     NMIX1           ;AND EXIT
NMI1    CPI     323Q            ;OUT?
        JNZ     NMIX            ;NO, EXIT
;       MOV     A,B             ;ELSE, GET DATA AGAIN
;       ANI     120Q            ;MOVE CLOCK TO BIT 1
;       RRC
;       RRC
;       RRC
;       RRC
;       RRC
;       DB      70Q,1           ;JRC $+3
;       INR     A               ;SET SSTEP BIT
;       LXI     H,H88CTL        ;GET CONTROL DATA
;       ORA     M               ;OR IT IN
;       OUT     OP2xCTL         ;SEND TO H88 PORT
;       ANI     11111100B       ;REMOVE CLOCK AND SSTEP
;       MOV     M,A             ;REPLACE CONTROL BYTE
NMIX    POP     PSW             ;RESTORE REGISTERS
NMIX1   POP     B
        POP     H
        DB      355Q,105Q       ;Z80 RETN

;       COMMAND BUFFER

CMDCNT  EQU     SxSOVR+2        ;COMMAND INPUT COUNT
CMDBUF  EQU     CMDCNT+1        ;COMMAND BUFFER


;       SCPROC - SCALL PROCESSOR
;       THIS CODE PROCESSES HDOS SCALLS

SCPROC  STA     SxCACC          ;SAVE A REGISTER
        IF      Z100
OLDTIC  EQU     210CH           ;USE FATAL ERROR VECTOR
        PUSH    H               ;SAVE HL
        PUSH    D               ;SAVE DE
        LHLD    TICCNT          ;GET TIC COUNTER
        XCHG                    ;IN DE
        LHLD    OLDTIC          ;GET OLD TIC VALUE
        CALL    CDEHL           ;COMPARE THEM
        JZ      SCPROC1         ;NO CHANGE
        XCHG                    ;ELSE PUT TICCNT IN HL
        SHLD    OLDTIC          ;UPDATE OLD TIC COUNTER
        LHLD    xTICCNT         ;GET HDOS TIC COUNTER
        INX     H               ;INCREMENT IT
        SHLD    xTICCNT
SCPROC1 POP     D               ;RESTORE REGISTERS
        POP     H
        ENDIF
        XTHL                    ;GET RETURN ADDRESS
        MOV     A,M             ;GET SCALL NUMBER
        STA     SxCODE          ;STORE SCALL CODE
        INX     H               ;SKIP OVER IT
        XTHL                    ;REPLACE RETURN ADDRESS
        CPI     xSYSRES         ;RESIDENT SCALL?
        JC      SCRES           ;YES
        CPI     xLOADD+1        ;OVERLAY 0 SCALL?
        JC      SC0             ;YES
        CPI     xRESET+1        ;OVERLAY 2 SCALL?
        JC      SC1             ;YES
        JMP     SCILL           ;ELSE ILLEGAL SCALL
SCRES   CALL    SJMP            ;TABLE JUMP TO RESIDENT SCALLS
        DW      EXIT
        DW      SCIN
        DW      SCOUT
        DW      PRINT1
        DW      READ
        DW      WRITE
        DW      CONSL
        DW      CLRCO
        DW      IGNORE          ;IGNORE LOADO
        DW      VERS
SC0     SUI     xLINK           ;LINK OR MORE?
        JC      SCILL           ;ILLEGAL SCALL IF NOT
        CALL    SJMP            ;TABLE JUMP TO OVERLAY 0
        DW      LINK
        DW      CTLC1
        DW      OPENR
        DW      OPENW
        DW      OPENU
        DW      SCILL           ;OPENC IS ILLEGAL
        DW      CLOSE
        DW      POSIT
        DW      DELET
        DW      RENAME
        DW      SETTP
        DW      DECODE
        DW      NAME
        DW      CLEAR
        DW      CLEARA
        DW      ERROR
        DW      CHFLG
        DW      IGNORE          ;IGNORE EQUMT
        DW      LOADD
SC1     SUI     xMOUNT          ;MOUNT OR MORE?
        JC      SCILL           ;ILLEGAL SCALL
        CALL    SJMP            ;TABLE JUMP TO OVERLAY 1
        DW      MOUNT
        DW      DMOUN
        DW      MONMS
        DW      DMNMS
        DW      RESET1
SCILL   MVI     A,ECxILC        ;ILLEGAL SCALL
        STC                     ;MARK ERROR
        RET

;       IGNORE - IGNORE THIS SYSTEM CALL

IGNORE  XRA     A               ;CLEAR ERROR
        RET

;       LOCAL CONTROL-C EXIT

SYCTLC  CALL    TYPTX
        DB      '^','C'+80H
SYCTLC1 DB      SCALL,xCLRCO    ;CLEAR CONSOLE
        MVI     A,1
        STA     INTFLG          ;SET INTERNAL MODE FLAG
;       JMP     EXIT

;       RESIDENT SYSTEM CALLS

;       EXIT - RETURN TO "HDOS"

EXIT    DB      SCALL,xCLEARA   ;CLEAR ALL I/O CHANNELS
        JMP     SYSCMD          ;JUMP TO SYSTEM COMMAND PROCESSOR

;       SCIN - SINGLE CHARACTER INPUT
;       IF IN LINE MODE, INPUT CHARACTERS INTO A LOCAL BUFFER
;       UNTIL NL, THEN TRANSMIT FROM BUFFER UNTIL EMPTY

SCIN    LDA     SxCSLMD         ;GET CONSOLE MODE
        RAR                     ;TEST FOR CHARACTER MODE
        JC      SCCHR           ;IN CHARACTER MODE
        LDA     INCNT           ;GET INPUT COUNT
        ORA     A               ;IS IT ZERO?
        JNZ     SCIN1           ;NO, GET CHARACTERS FROM BUFFER
SCLOOP  CALL    DCIN            ;GET A CHARACTER
        JZ      SCLOOP          ;NONE THERE
        CPI     CTLP            ;CONTROL-P?
        CZ      TOGLP           ;IF SO, TOGGLE IT
        JZ      SCLOOP          ;PRETEND NO CHARACTER
        CPI     CTLO            ;CONTROL-O?
        CZ      TOGLO           ;IF SO, TOGGLE IT
        JZ      SCLOOP          ;PRETEND NO CHARACTER
        CPI     BKSP            ;BACKSPACE?
        JZ      SCBACK          ;YES, PROCESS IT
        CPI     RUBOUT          ;DELETE?
        JZ      SCBACK          ;YES, PROCESS IT
        CPI     CTLU            ;CONTROL-U?
        JNZ     SCCTL           ;NO
        LDA     SxCSLMD         ;YES, GET CONSOLE MODE
        RAR                     ;SEE IF ECHOING
        JC      SCINOV          ;IF NOT, START OVER
        CALL    TYPTX
        DB      '^U',ENL        ;ELSE PRINT "^U"
SCINOV  XRA     A
        STA     INCNT           ;CLEAR CHR COUNT
        JMP     SCIN            ;START OVER
SCCTL   CPI     CTLD            ;CONTROL-D?
        CC      CTLCP           ;CHECK FOR CONTROL-C
        JC      SCLOOP          ;WAS ONE, TRY AGAIN
        CPI     CR              ;RETURN?
        JNZ     SCSAV           ;NO, NORMAL CHARACTER
        MVI     A,NL            ;ELSE, REPLACE WITH NL
SCSAV   ORA     A               ;CLEAR CARRY
        CALL    SCSAVE          ;SAVE CHARACTER
        JC      SCLOOP          ;BUFFER IS FULL
        CALL    COUT            ;CONDITIONALLY PRINT
        CPI     CTLD            ;CONTROL-D?
        JZ      SCINX           ;YES, EXIT
        CPI     NL              ;NEW-LINE?
        JNZ     SCLOOP          ;NO, GET MORE CHARACTERS
SCINX   STC                     ;SET CARRY
        RET
SCBACK  LDA     INCNT           ;GET INPUT COUNTER
        ORA     A               ;AT POSITION ZERO?
        JZ      SCLOOP          ;IF SO, TRY ANOTHER CHARACTER
        DCR     A               ;ELSE, DECREMENT COUNTER
        STA     INCNT           ;AND REPLACE IT
        LDA     SxCSLMD         ;GET CONSOLE MODE
        RAL                     ;TEST FOR ECHO SUPRESS MODE
        JC      SCLOOP          ;YES, DON'T BACK UP ON SCREEN
        PUSH    H
        LXI     H,INCNT         ;POINT TO INPUT COUNTER
        MOV     A,M             ;GET IT
        INX     H               ;MOVE TO BUFFER
        CALL    DADA            ;MOVE TO LAST CHARACTER
        MOV     A,M             ;GET IT
        POP     H
        CPI     TAB             ;IS IT A TAB?
        JZ      SCBAK1          ;YES
        CPI     ' '             ;OTHER CONTROL CHARACTER
        JC      SCLOOP          ;IF SO, DON'T BACK UP
SCBAK1  PUSH    PSW             ;SAVE CHARACTER
        LDA     SxCUSOR         ;GET CURSOR POSITION
        DCR     A               ;BACK UP
        STA     SxCUSOR
        LDA     SxCONTY         ;GET CONSOLE TYPE BYTE
        RAL                     ;TEST FOR BACKSPACE CAPABILITY
        JC      SCBAK2          ;IT CAN DO IT
        POP     PSW             ;RESTORE CHARACTER
        CALL    CHOUT           ;PRINT IT
        JMP     SCLOOP          ;CONTINUE
SCBAK2  POP     PSW             ;RESTORE STACK
        MVI     A,BKSP          ;ELSE, BACK UP
        CALL    CHOUT
        MVI     A,' '           ;SPACE
        CALL    CHOUT
        MVI     A,BKSP          ;AND BACKSPACE AGAIN
        CALL    CHOUT
        JMP     SCLOOP          ;TRY ANOTHER CHARACTER
SCIN1   LDA     INBUF           ;GET A CHARACTER
        CALL    MOVDN           ;MOVE CHARACTERS DOWN
SCIN1A  PUSH    PSW             ;SAVE CHARACTER
        LDA     SxCONTY         ;GET CONSOLE TYPE BYTE
        ANI     CTPxMLI         ;MAP TO UPPER CASE?
        JZ      SCIN2           ;NO, EXIT
        POP     PSW             ;ELSE, RESTORE CHARACTER
        CALL    MCU             ;MAP TO UPPER CASE
        ORA     A               ;CLEAR CARRY
        RET
SCIN2   POP     PSW             ;RESTORE CHARACTER
        ORA     A
        RET
SCCHR   CALL    DCIN            ;GET THE CHARACTER
        JZ      SCCHR2          ;NO CHARACTER READY
        CPI     CR              ;RETURN?
        JNZ     SCCHR1          ;NO
        MVI     A,NL            ;REPLACE WITH NL
SCCHR1  CPI     CTLD            ;LESS THAN CONTROL-D?
        CC      CTLCP           ;IF SO, PROCESS IT
        JC      SCCHR2          ;THERE WAS A CONTROL CHAR
        CALL    SCSAVE          ;ELSE, SAVE CHARACTER
SCCHR2  PUSH    H               ;SAVE HL
        LXI     H,INCNT         ;POINT TO INPUT COUNTER
        MOV     A,M             ;GET COUNT
        ORA     A               ;ANY INPUT?
        JZ      SCEXIT0         ;NO
        INX     H               ;ELSE, MOVE TO BUFFER
        MOV     A,M             ;GET CHARACTER
        CALL    MOVDN           ;MOVE CHARACTERS DOWN
        POP     H               ;RESTORE HL
SCIN4   CPI     CTLP            ;CONTROL-P?
        CZ      TOGLP           ;IF SO, TOGGLE IT
        JZ      SCEXIT          ;PRETEND NO CHARACTER
        CPI     CTLO            ;CONTROL-O?
        CZ      TOGLO           ;IF SO, TOGGLE IT
        JZ      SCEXIT          ;PRETEND NO CHARACTER
        ORA     A               ;CLEAR CARRY
        CALL    COUT            ;CONDITIONALLY PRINT
        JMP     SCIN1A          ;CHECK FOR MLI
SCEXIT0 POP     H
SCEXIT  STC                     ;MARK NO CHARACTER
        RET

;       SCSAVE - SAVE INPUT CHARACTER
;       ENTER WITH CHARACTER TO SAVE

SCSAVE  PUSH    H               ;SAVE HL
        PUSH    PSW             ;SAVE CHARACTER
        LXI     H,INCNT         ;POINT TO COUNT
        MOV     A,M             ;GET IT
        CPI     100             ;AT END?
        JNZ     SCNTM           ;NO
        MVI     A,BELL          ;ELSE, RING BELL
        CALL    CHOUT
        POP     PSW             ;RESTORE REGISTERS
        POP     H
        STC                     ;MARK BUFFER FULL
        RET
SCNTM   INR     M               ;INCREMENT COUNT
        INX     H               ;MOVE TO BUFFER
        CALL    DADA            ;MOVE TO EMPTY SPOT
        POP     PSW             ;RESTORE CHARACTER
        MOV     M,A             ;PUT IT IN
        POP     H               ;RESTORE HL
        RET

;       MOVDN - MOVE CHARACTERS DOWN IN INPUT BUFFER

MOVDN   PUSH    H               ;SAVE REGISTERS
        PUSH    D
        PUSH    B
        PUSH    PSW
        LXI     H,INCNT         ;POINT TO INPUT COUNTER
        MOV     B,M             ;PUT COUNT IN B
        INX     H               ;MOVE TO BUFFER
        LXI     D,INBUF+1       ;AND BUFFER + 1
MOVDN1  LDAX    D               ;GET A CHARACTER
        MOV     M,A             ;MOVE IT
        INX     H               ;INCREMENT POINTERS
        INX     D
        DCR     B               ;DECREMENT COUNTER
        JNZ     MOVDN1          ;LOOP UNTIL FINISHED
        LXI     H,INCNT         ;POINT TO COUNTER
        DCR     M               ;DECREMENT IT
        POP     PSW             ;RESTORE REGISTERS
        POP     B
        POP     D
        POP     H
        RET

;       TOGLP - TOGGLE CONTROL-P

TOGLP   PUSH    PSW             ;SAVE PSW
        LDA     SxCONFL         ;GET CONSOLE FLAG BYTE
        XRI     CPxFLG          ;TOGGLE FLAG
        STA     SxCONFL         ;REPLACE BYTE
        POP     PSW
        RET

;       TOGLO - TOGGLE CONTROL-O

TOGLO   PUSH    PSW
        LDA     SxCONFL
        XRI     COxFLG
        STA     SxCONFL
        POP     PSW
        RET

;       PROCESS CONTROL-C HITS
;       JUMP TO CONTROL PROCESSOR, IF ANY
;       WITH RETURN FROM .SCIN OR .SCOUT ON THE STACK

CTLCP   SHLD    HSAVE           ;SAVE REGISTERS
        PUSH    PSW
        ORA     A               ;ZERO IN A?
        JZ      NOCTLC          ;THEN DO NOT PROCESS
        LXI     H,CTLCA+1       ;POINT TO CTLA ADDR HIGH
        DCR     A               ;MAKE RANGE 0-2
        ADD     A               ;MAKE IT 0,2,4
        CALL    DADA            ;MOVE TO PROPER VECTOR
        MOV     A,M             ;GET THE HIGH BYTE
        ORA     A               ;IS IT ZERO?
        JZ      NOCTLC          ;YES, NO PROCESS
CTLCP1  DCX     H               ;POINT TO LOW BYTE
        MOV     L,M
        MOV     H,A             ;HL = PROCESS ADDRESS
        SHLD    CTLJMP          ;SET UP CONTROL JUMP
        POP     PSW             ;RESTORE PSW
        PUSH    PSW             ;SAVE IT AGAIN
        LXI     H,CTLCX         ;GET CONTROL-C EXIT ADDRESS
        PUSH    H               ;PUT IT ON THE STACK
        LHLD    HSAVE           ;GET HL
        JMP     0               ;GO TO PROCESS
CTLJMP  EQU     $-2
CTLCA   DW      0               ;CONTROL-A ADDRESS
CTLCB   DW      0               ;CONTROL-B ADDRESS
CTLCC   DW      0               ;CONTROL-C ADDRESS
NOCTLC  POP     PSW             ;RESTORE REGISTERS
        LHLD    HSAVE
        RET                     ;RETURN WITH FLAGS INTACT
CTLCX   POP     PSW             ;RESTORE PSW
        RET                     ;RETURN TO INTERRUPTED ROUTINE
HSAVE   DW      0               ;SAVED HL

;       SCOUT - SINGLE CHARACTER OUTPUT
;       OUTPUTS CHARACTER IN A

SCOUT   PUSH    PSW
        CALL    DCIN            ;TEST FOR INPUT
        JZ      SCOUT0          ;NONE THERE
        CPI     CTLO            ;CONTROL-O?
        CZ      TOGLO           ;IF SO, TOGGLE IT
        JZ      SCOUT+1
        CPI     CTLP            ;CONTROL-P?
        CZ      TOGLP           ;IF SO, TOGGLE IT
        JZ      SCOUT+1
        CPI     CTLS            ;CONTROL-S?
        CZ      SCWAIT          ;IF SO, WAIT
        CPI     CTLD            ;LESS THAN CONTROL-D?
        CC      CTLCP           ;IF SO, PROCESS IT
        JC      SCOUT+1         ;CONTROL CHAR, TRY AGAIN
        CALL    SCSAVE          ;GOOD CHARACTER, SAVE IT
        JMP     SCOUT+1         ;SEE IF MORE CHARACTERS
SCOUT0  LDA     SxCONTY         ;GET CONSOLE TYPE BYTE
        ANI     CTPxMLO         ;MAPPING TO UPPER CASE?
        JZ      SCOUT0A         ;NO
        POP     PSW             ;ELSE, GET CHARACTER
        PUSH    PSW             ;SAVE AGAIN
        CALL    MCU             ;MAP TO UPPER CASE
        JMP     SCOUT0A+2
SCOUT0A POP     PSW             ;RESTORE CHARACTER
        PUSH    PSW             ;SAVE AGAIN
        CPI     NL              ;NEW LINE CHARACTER
        JNZ     SCOUT1          ;NO, ORDINARY CHARACTER
        MVI     A,CR            ;ELSE SEND CR
        CALL    CHOUT
        MVI     A,1
        STA     SxCUSOR         ;CLEAR COLUMN NUMBER
        MVI     A,NL            ;REPLACE NL IN A
        CALL    CHOUT           ;PRINT IT
        POP     PSW             ;RESTORE CHARACTER
        RET
SCOUT1  PUSH    B               ;SAVE BC
        CPI     TAB             ;TAB CHARACTER?
        JNZ     SCOUT2          ;NO
        LDA     SxCUSOR         ;ELSE, GET CURSOR POSITION
        DCR     A               ;MAKE IT START AT ZERO
        MOV     B,A             ;SAVE IN B
        ADI     8               ;ADD 8
        ANI     0F8H            ;REDUCE TO NEXT TAB STOP
        MOV     C,A             ;SAVE IN C
        INR     A               ;START AT 1
        STA     SxCUSOR         ;SAVE AGAIN
        LDA     SxCONTY         ;GET CONSOLE TYPE BYTE
        RAR                     ;TEST FOR TAB SUPPORT
        MVI     A,TAB           ;ASSUME IT CAN
        JC      SCOUT2          ;TERMINAL SUPPORTS TABS
SCOUT1L MVI     A,' '
        CALL    CHOUT           ;PRINT A SPACE
        MOV     A,B             ;GET OLD CURSOR POSITION
        INR     A               ;ADD ONE
        MOV     B,A             ;SAVE IT
        CMP     C               ;UP TO NEW ONE?
        JNZ     SCOUT1L         ;NO, CONTINUE
        JMP     SCOUT3          ;ELSE, EXIT
SCOUT2  CALL    CHOUT           ;PRINT CHARACTER
        CPI     ' '             ;LESS THAN SPACE?
        JC      SCOUT3          ;IF SO, NO CURSOR COUNT
        LDA     SxCUSOR
        INR     A               ;UPDATE CUSOR POSITION
        MOV     B,A             ;SAVE IN B
        STA     SxCUSOR
        LDA     SxCONWI         ;GET CONSOLE WIDTH
        CPI     255             ;SET TO MAX?
        JZ      SCOUT3          ;YES, NO AUTO CR
        CMP     B               ;HAVE WE REACHED WIDTH?
        JNC     SCOUT3          ;NO, EXIT
        MVI     A,NL            ;ELSE, PRINT NL
        DB      SCALL,xSCOUT
SCOUT3  POP     B               ;RESTORE BC
        POP     PSW             ;RESTORE CHARACTER
        RET

;       CONDITIONAL OUT -- PRINTS IF IN ECHO MODE

COUT    PUSH    PSW             ;SAVE CHARACTER
        LDA     SxCSLMD         ;GET CONSOLE MODE
        RAL                     ;TEST FOR NO ECHO
        JC      COUT1           ;NO ECHO SET
        JMP     SCOUT0          ;USE SCOUT TO PROCESS
COUT1   POP     PSW
        RET

;       SCWAIT - WAIT FOR CONTROL-Q

SCWAIT  CALL    DCIN            ;LOOK FOR ENTRY
        JZ      SCWAIT          ;NONE THERE
        CPI     CTLD            ;CHECK FOR CONTROL CHARS
        CC      CTLCP
        CPI     CTLQ            ;CONTROL-Q?
        JNZ     SCWAIT          ;WAIT FOR IT
        XRA     A               ;CLEAR A
        RET

;       PRINT1 - PRINT TEXT AT ((HL))

PRINT1  JMP     TYPTXx

;       READ - READ FROM FILE
;
;       ENTRY:
;       DE = ADDRESS TO READ INTO
;       BC = COUNT OF BYTES TO READ (MOD 256)
;       A  = CHANNEL NUMBER
;
;       EXIT:
;       A  = ERROR CODE (IF CY = 1)
;       BC = UNUSED COUNT
;       DE = NEXT UNUSED ADDRESS

READ    PUSH    PSW             ;SAVE CHANNEL NO.
        CALL    GETCFLG         ;GET CHANNEL FLAG
        ANI     -1-1000B AND 0FFH ;MASK WRITE BIT
        ORA     A               ;CHANNEL OPEN?
        JZ      NOTOPEN         ;NO
        DCR     A               ;DISK DEVICE?
        JZ      READ1           ;YES
        DCR     A               ;PRINT DEVICE?
        JZ      READ0           ;YES, ERROR
        DCR     A               ;TT: DEVICE?
        JZ      READ2           ;YES
        DCR     A               ;AT: DEVICE?
        JZ      READ2           ;YES
        POP     PSW             ;ELSE, ASSUME ND: DEVICE
NDREAD  MVI     A,ECxEOF        ;SAY END OF FILE
        STC
        RET
READ0   POP     PSW             ;RESTORE PSW
        MVI     A,ECxDNS        ;WE CAN'T DO IT
        STC
        RET
READ1   POP     PSW             ;RESTORE CHANNEL NO.
        XCHG                    ;HL = ADDRESS
        CALL    CHN2FCB         ;CONVERT CHANNEL TO FCB
        CALL    SEC2REC         ;CONVERT SECTORS TO RECORDS
        MVI     A,READR         ;USE READ FUNCTION
        CALL    DBLKIO          ;READ THE BLOCK
        XCHG                    ;DE = NEXT ADDRESS
        CALL    REC2SEC         ;CONVERT RECORDS TO SECTORS
        RNC                     ;RETURN IF NO ERROR
        MVI     A,ECxEOF        ;MARK EOF
        RET
NOTOPEN POP     PSW             ;RESTORE A,F
        MVI     A,ECxFNO        ;CHANNEL NOT OPEN
        STC
        RET
READ2   POP     PSW             ;RESTORE CHANNEL NUMBER
READ3   DB      SCALL,xSCIN     ;USE SCIN FOR TT:
        JC      READ3
        STAX    D               ;STORE THE CHARACTER
        INX     D               ;INCREMENT POINTER
        DCX     B               ;DECREMENT COUNTER
        CPI     CTLD            ;CONTROL-D?
        JZ      READ4           ;EXIT ON CONTROL-D
        MOV     A,B
        ORA     C               ;TEST IF DONE
        JNZ     READ3           ;LOOP IF NOT
        RET
READ4   MVI     H,0             ;H = ZERO
        PUSH    B               ;SAVE BC
READ5   XRA     A               ;GET A ZERO
        STAX    D               ;STORE IT
        DCR     H               ;DONE?
        JZ      READ6           ;YES
        INX     D               ;INCREMENT POINTER
        DCX     B               ;DECREMENT COUNTER
        MOV     A,B
        ORA     C               ;DONE?
        JNZ     READ5
READ6   POP     B               ;RESTORE COUNT
        MVI     A,ECxEOF        ;SAY END OF FILE
        STC
        RET

;       WRITE - WRITE TO FILE
;
;       ENTRY:
;       DE = ADDRESS TO WRITE FROM
;       BC = COUNT OF BYTES TO WRITE
;       A  = CHANNEL NUMBER
;
;       EXIT:
;       A  = ERROR CODE (IF CY = 1)
;       BC = UNUSED COUNT
;       DE = NEXT UNUSED ADDRESS

WRITE   PUSH    PSW             ;SAVE CHANNEL NO.
        MOV     A,B
        ORA     C               ;ANYTHING TO WRITE?
        JZ      WRITEX          ;NO, EXIT
        POP     PSW             ;ELSE, RESTORE CHANNEL NO.
        PUSH    PSW             ;SAVE AGAIN
        CALL    GETCFLG         ;GET CHANNEL FLAG
        ANI     -1-1000B AND 0FFH ;MASK WRITE BIT
        ORA     A               ;OPENED?
        JZ      NOTOPEN         ;NO
        DCR     A               ;DISK DEVICE?
        JZ      WRITE1          ;YES
        DCR     A               ;PRINT DEVICE?
        JZ      WRITE00         ;YES
        DCR     A               ;TT: DEVICE?
        JZ      WRITE2          ;YES
        DCR     A               ;AT: DEVICE?
        JZ      WRITE2          ;YES
WRITEX  POP     PSW             ;ELSE, ASSUME ND: DEVICE
        ORA     A               ;CLEAR CARRY (DO NOTHING)
        RET
WRITE00 POP     PSW             ;RESTORE PSW
        XCHG                    ;HL = ADDRESS OF DATA
WRITE0  CALL    DCIN            ;TEST FOR INPUT
        JZ      WRITE01         ;NONE THERE
        CPI     CTLS            ;CONTROL-S?
        CZ      SCWAIT          ;IF SO, WAIT
        CPI     CTLD            ;LESS THAN CONTROL-D?
        CC      CTLCP           ;IF SO, PROCESS IT
        JC      WRITE0          ;CONTROL CHAR, TRY AGAIN
        CALL    SCSAVE          ;GOOD CHARACTER, SAVE IT
        JMP     WRITE0          ;SEE IF MORE CHARACTERS
WRITE01 MOV     A,M             ;GET A BYTE
        CPI     NL              ;NEW LINE?
        JNZ     WRITE0A         ;NO
        MVI     E,CR            ;ELSE, WRITE CR
        PUSH    B
        MVI     C,LSTOUT
        CALL    CBDOS
        POP     B
        IF      TABX
        XRA     A
        STA     CPOS            ;CLEAR CURSOR POSITION
        ENDIF
        MVI     A,NL            ;REPLACE NL
WRITE0A MOV     E,A             ;PUT CHARACTER IN E
        IF      TABX
        MVI     A,1
        STA     WCNT            ;SET CHARACTER COUNTER
        MOV     A,E             ;GET CHARACTER AGAIN
        CPI     TAB             ;TAB?
        JNZ     WNOTAB          ;NO
        LDA     CPOS            ;ELSE, GET CURSOR POSITION
        MOV     E,A             ;SAVE IN E
        ADI     8               ;ADD 8
        ANI     0F8H            ;REDUCE TO NEXT TAB STOP
        SUB     E               ;GET COUNT TO MOVE
        STA     WCNT            ;SAVE IT
        MVI     E,' '           ;USE SPACES INSTEAD OF TAB
        ENDIF
WNOTAB  PUSH    B               ;SAVE COUNT
WRLOOP  MVI     C,LSTOUT
        CALL    CBDOS           ;PRINT CHARACTER
        IF      TABX
        MOV     A,E             ;GET CHARACTER
        CPI     ' '             ;SPACE?
        JC      WNPOS           ;IF SO, NO CURSOR CHANGE
        LDA     CPOS            ;GET CURSOR POSITION
        INR     A               ;INCREMENT IT
        STA     CPOS
WNPOS   LDA     WCNT            ;GET WRITE COUNT
        DCR     A               ;DECREMENT IT
        STA     WCNT
        JNZ     WRLOOP          ;LOOP UNTIL DONE
        ENDIF
        POP     B               ;RESTORE COUNT
        INX     H               ;INCREMENT ADDRESS
        DCX     B               ;DECREMENT COUNT
        MOV     A,B
        ORA     C               ;DONE?
        JNZ     WRITE0          ;IF NOT, LOOP
        XCHG                    ;DE = NEXT ADDRESS
        RET
WRITE1  POP     PSW             ;GET CHANNEL NO.
        XCHG                    ;HL = ADDRESS
        CALL    CHN2FCB         ;CONVERT CHANNEL TO FCB
        CALL    SEC2REC         ;CONVERT SECTORS TO RECORDS
        MVI     A,WRITER        ;USE WRITE FUNCTION
        CALL    DBLKIO          ;WRITE THE BLOCK
        XCHG                    ;DE = NEXT ADDRESS
        CALL    REC2SEC         ;CONVERT RECORDS TO SECTORS
        RNC                     ;RETURN IF NO ERROR
        MVI     A,ECxEOM        ;ELSE, NO FREE SPACE
        RET
WRITE2  POP     PSW             ;RESTORE PSW
WRITE3  LDAX    D               ;GET A CHARACTER
        DB      SCALL,xSCOUT    ;PRINT IT
        INX     D               ;INCREMENT POINTER
        DCX     B               ;DECREMENT COUNTER
        MOV     A,B
        ORA     C               ;TEST IF DONE
        JNZ     WRITE3          ;LOOP IF NOT
        RET
        IF      TABX
CPOS    DB      0               ;CURSOR POSITION
WCNT    DB      0               ;WRITE COUNT
        ENDIF

;       CONSL - SET CONSOLE MODES
;
;       ENTRY:
;       A  = BYTE TO SET
;       B  = BITS TO SET
;       C  = MASK OF BITS TO EFFECT

CONSL   LXI     H,SxCSLMD       ;S.CSLMD IS FIRST BYTE
        CALL    DADA            ;MOVE TO BYTE TO CHANGE
        MOV     A,B             ;GET BITS TO CHANGE
        ANA     C               ;MASK OUT UNWANTED ONES
        MOV     B,A             ;REPLACE BITS IN B
        MOV     A,C             ;GET MASK
        CMA                     ;COMPLEMENT IT
        ANA     M               ;KILL EFFECTED BITS
        ORA     B               ;SET NEW ONES
        MOV     M,A             ;SET IN NEW BITS
        RET

;       CLRCO - CLEAR CONSOLE BUFFER

CLRCO   XRA     A
        STA     INCNT           ;CEAR INPUT COUNTER
CLRCO1  CALL    DCIN            ;CHECK FOR CHARACTER INPUT
        RZ                      ;RETURN IF NONE
        JMP     CLRCO1          ;TRY AGAIN

;       VERS - RETURN HDOS VERSION NUMBER

VERS    MVI     A,VER           ;GET HDOS VERSION NO.
        ORA     A               ;CLEAR CARRY
        RET

;       OVERLAY 0 SYSTEM CALLS

;       LINK - CHAIN TO ANOTHER PROGRAM
;
;       ENTRY:
;       HL = FILE NAME
;       DE = DEFAULTS
;
;       EXIT:
;       A  = UNCHANGED OR ERROR CODE IF ERROR (CY = 1)
;       SP = USER'S SP IF LINK OK

LINK    STA     SxCACC          ;SAVE A REGISTER
        PUSH    H               ;SAVE HL
        PUSH    D               ;AND DE
        LDA     FLGM1           ;GET CHANNEL -1 FLAG
        ORA     A               ;IS IT OPEN?
        JZ      LINK0           ;IF NOT, GO ON
        MVI     A,-1 AND 0FFH
        DB      SCALL,xCLOSE    ;ELSE, CLOSE IT
LINK0   MVI     A,-1 AND 0FFH   ;USE THAT CHANNEL
        POP     D               ;RESTORE REGISTERS
        POP     H
        DB      SCALL,xOPENR    ;OPEN FOR READ
        RC                      ;RETURN IF CAN'T OPEN
        LXI     D,80H
        MVI     C,SETDMA
        CALL    BDOS            ;SET DMA TO 80H (DEFAULT)
        LXI     D,FCBM1         ;POINT TO CHANNEL -1 FCB
        MVI     C,READF
        CALL    BDOS            ;READ IN 1 RECORD
        LDA     80H             ;GET FIRST FILE BYTE
        CPI     0FFH            ;PROGRAM FILE?
        JNZ     NOTABS          ;NO
        LDA     81H             ;GET FILE TYPE
        ORA     A               ;ABS FILE?
        JNZ     NOTABS          ;NO
        LHLD    84H             ;GET FILE SIZE
        XCHG                    ;IN DE
        LHLD    82H             ;GET FILE LOAD ADDRESS
        DAD     D               ;ADD SIZE
        SHLD    SxUSRM          ;SET USER MEMORY LIMIT
        LHLD    82H             ;GET FILE LOAD ADDRESS
        XCHG                    ;IN DE
        PUSH    D               ;SAVE IT
        LXI     B,100H          ;READ 1 SECTOR
        MVI     A,-1 AND 0FFH
        DB      SCALL,xREAD     ;READ THE SECTOR
        POP     H               ;GET THE LOAD ADDRESS AGAIN
        PUSH    H               ;SAVE AGAIN
        LXI     D,8
        DAD     D               ;ADD 8
        XCHG                    ;DE = LOAD + 8
        POP     H               ;HL = LOAD
        LXI     B,256-8         ;MOVE SECTOR - HEADER
        CALL    MOVE            ;MOVE DATA DOWN
        LHLD    84H             ;GET SIZE
        LXI     D,8
        DAD     D               ;ADD 8 (SIZE OF HEADER)
        MOV     A,H             ;GET HIGH BYTE
        ORA     A               ;ONE SECTOR FILE?
        JZ      LINK2           ;IF SO, JUMP TO IT
        MOV     B,H             ;ELSE MAKE COUNT OF SIZE
        MVI     C,0
        LHLD    82H             ;GET LOAD ADDRESS
        INR     H               ;ADD 256
        CALL    CHL             ;COMPLEMENT IT
        XCHG                    ;PUT IT IN DE
        LHLD    SxSYSM          ;GET SYSTEM FWA
        DAD     D               ;SUBTRACT LOAD ADDRESS
        MOV     A,L
        SUB     C               ;SUBTRACT COUNT FROM IT
        MOV     A,H
        SBB     B               ;TO SEE IF ENOUGH ROOM
        JNC     LINK1           ;THERE'S ENOUGH
        MVI     A,ECxNEM        ;ELSE, NOT ENOUGH MEMORY
        RET
LINK1   LHLD    82H             ;GET LOAD ADDRESS
        LXI     D,256-8         ;ADD FIRST SECTOR - HEADER
        DAD     D
        XCHG                    ;RESULT IN DE
        MVI     A,-1 AND 0FFH   ;USE CHANNEL -1
        DB      SCALL,xREAD     ;READ USER'S FILE
LINK2   LHLD    86H             ;GET ENTRY ADDRESS
        LDA     SxCACC          ;GET A REGISTER
        XTHL                    ;SET UP EXIT TO FILE
        RET                     ;JUMP TO FILE
NOTABS  MVI     A,ECxFUC        ;BAD FILE TYPE
        STC
        RET

;       CLTC1 - PROCESS CONTROL-C VECTORS
;
;       ENTRY:
;       HL = CONTROL-C VECTOR
;
;       EXIT: A = ERROR CODE (IF CY = 1)

CTLC1   ORA     A               ;CONTROL-@?
        JZ      CTLC2           ;NOT ALLOWED
        CPI     4               ;MORE THAN 3?
        CMC
        JC      CTLC2           ;IF SO, LEAVE
        XCHG                    ;SAVE ADDRESS
        LXI     H,CTLCA         ;POINT TO VECTOR AREA
        DCR     A
        ADD     A               ;CONVERT CHAR TO INDEX
        CALL    DADA            ;FIND OUR VECTOR
        MOV     M,E             ;PUT ADDRESS THERE
        INX     H
        MOV     M,D
        RET
CTLC2   MVI     A,ECxILR        ;ILLEGAL REQUEST
        RET

;       OPENR, OPENW AND OPENU - OPEN FILES
;
;       ENTRY:
;       HL = FILE NAME IN ASCII
;       DE = FILE DEFAULTS
;       A  = CHANNEL NUMBER
;
;       EXIT:
;       A  = ERROR (IF CY = 1)

OPENW   MVI     B,FTxOW         ;MARK OPENW
        JMP     OPENU+2
OPENR   MVI     B,FTxOR
        JMP     OPENU+2
OPENU   MVI     B,FTxOU+FTxOW+FTxOR
        PUSH    PSW             ;SAVE CHANNEL NO.
        CALL    GETCFLG         ;GET CHANNEL FLAG
        ORA     A               ;IS CHANNEL NOW OPEN?
        JZ      OPEN1           ;NO, GO AHEAD
        POP     PSW             ;ELSE, RESTORE A,F
        MVI     A,ECxFAO        ;AND SAY FILE ALREADY OPEN
        STC
        RET
OPEN1   POP     PSW             ;GET CHANNEL NO. AGAIN
        PUSH    PSW             ;SAVE AGAIN
        CALL    CFN             ;CRACK FILE NAME
        JNC     OPEN1A          ;GOOD FILE NAME
        POP     D               ;FIX STACK
        RET
OPEN1A  MOV     A,H             ;GET DEVICE TYPE
        CPI     1               ;DISK OPEN?
        JNZ     OPEN2           ;NO, EXIT
        MOV     A,B             ;GET FILE TYPE FLAG
        ORI     FTxDD           ;FLAG AS DIRECTORY DEVICE
        MOV     B,A
        MVI     C,OPENF
        CALL    CBDOS           ;TRY TO OPEN FILE
        PUSH    PSW             ;SAVE RESULT OF OPEN
        MOV     A,B             ;ELSE, GET FILE TYPE
        CPI     FTxOW+FTxDD     ;IS IT WRITE?
        JNZ     OPEN1B          ;NO, OPEN EXISTING FILE
        POP     PSW             ;GET RESULT OF OPEN
        INR     A               ;EXISTING FILE?
        JZ      OPEN1A0         ;NO
        PUSH    H               ;ELSE, SAVE HL
        MOV     H,D
        MOV     L,E             ;HL = FCB
        MVI     A,FTYPE
        CALL    DADA            ;MOVE TO FILE TYPE
        MOV     A,M             ;GET FIRST BYTE
        POP     H               ;RESTORE HL
        ANI     80H             ;ISOLATE WRITE FLAG
        JZ      OPEN1A1         ;FILE NOT WRITE PROTECTED
        POP     PSW             ;ELSE, GET CHANNEL NO.
        DB      SCALL,xCLOSE    ;CLOSE THE FILE
        MVI     A,ECxWPV        ;WRITE PROTECT VIOLATION
        STC
        RET
OPEN1A1 MVI     C,DELETE        ;DELETE OLD FILE
        CALL    CBDOS
OPEN1A0 MVI     C,MAKEF         ;AND MAKE NEW ONE
        CALL    CBDOS
        INR     A               ;GOOD MAKE?
        JNZ     OPEN1C          ;YES
        POP     PSW             ;ELSE, RESTORE A,F
        DB      SCALL,xCLOSE
        MVI     A,ECxDIF        ;SAY DIRECTORY FULL
        STC
        RET
OPEN1B  POP     PSW             ;GET RESULT OF OPEN
        INR     A               ;GOOD OPEN?
        JZ      NOFILE          ;NO
OPEN1C  XCHG                    ;HL = FCB, DE = FLAGS
        PUSH    H               ;SAVE FCB
        MVI     A,R0            ;LOCATE RANDOM RECORD COUNTER
        CALL    DADA
        MVI     M,0             ;ZERO IT
        INX     H
        MVI     M,0
        INX     H
        MVI     M,0
        POP     H               ;RESTORE FCB
        XCHG                    ;HL = FLAGS, DE = FCB
OPEN2   MOV     A,L             ;GET CAPABILITY FLAG
        ANA     B               ;AND WITH REQUEST
        CMP     B               ;STILL EQUALS REQUEST?
        JZ      OPEN2A          ;YES, GO AHEAD
        POP     PSW             ;ELSE, RESTORE PSW
        DB      SCALL,xCLOSE
        MVI     A,ECxDNS        ;SAY DEVICE NOT SUITABLE
        STC
        RET
OPEN2A
        IF      TABX
        MOV     A,H
        CPI     2
        JNZ     OPEN2AX         ;NOT LP:
        XRA     A
        STA     CPOS            ;CLEAR CURSOR POSITION
OPEN2AX
        ENDIF
        MOV     A,H             ;GET DEVICE TYPE
        CPI     4               ;AT: DEVICE?
        JNZ     OPEN2B          ;NO
        LDA     IOBYTE          ;ELSE, SWITCH IOBYTE
        XRI     1
        STA     IOBYTE          ;TO ALTERNATE DEVICE
        DB      SCALL,xCLRCO    ;CLEAR OUT UNWANTED CHARACTERS
OPEN2B  POP     PSW             ;GET CHANNEL NO.
        PUSH    PSW             ;SAVE AGAIN
        INR     A               ;MAKE IT 0-6
        MOV     C,H             ;SAVE FLAG
        LXI     H,CHNFLGS       ;GET CHANNEL FLAGS
        CALL    DADA            ;MOVE TO OUR CHANNEL
        MOV     A,B             ;GET FILE FLAG
        ANI     FTxOW           ;ISOLATE WRITE FLAG
        RLC                     ;MOVE IT OVER ONE
        ORA     C               ;ADD OPEN FLAG
        MOV     M,A             ;MARK AS OPEN
        MOV     A,B             ;GET FILE TYPE FLAG
        STA     IOCxFLG         ;PUT IT IN IOC
        ANI     FTxDD           ;DIRECTORY DEVICE?
        JZ      OPEN4           ;IF NOT, RETURN
        XCHG                    ;HL = FCB
        MVI     A,FTYPE         ;OFFSET TO FILE TYPE
        CALL    DADA            ;GO THERE
        MOV     A,M             ;GET IT
        ANI     80H             ;ISOLATE WRITE FLAG
        RRC                     ;MOVE IT TO HDOS POSITION
        RRC
        MOV     C,A             ;SAVE IT IN C
        CPI     40Q             ;IS FILE WRITE PROTECTED?
        JNZ     OPEN3           ;NO, GO ON
        MOV     A,B             ;ELSE, GET FILE TYPE
        ANI     FTxOW           ;OPEN FOR WRITE?
        JZ      OPEN3           ;NO, GO ON
        POP     PSW             ;ELSE, GET CHANNEL NUMBER
        DB      SCALL,xCLOSE    ;AND CLOSE FILE
        MVI     A,ECxWPV        ;WRITE PROTECT VIOLATION
        STC
        RET
OPEN3   INX     H               ;MOVE TO SYSTEM FLAG
        MOV     A,M             ;GET IT
        ANI     80H             ;ISOLATE IT
        ORA     C               ;ADD WRITE FLAG
        STA     DIRxFLG         ;SAVE IT
OPEN4   POP     PSW             ;RESTORE A,F
        CALL    CHN2IOC         ;LOCATE IOC FOR THIS CHANNEL
        INX     H               ;SKIP OVER ADDRESS OF NEXT IOC
        INX     H
        LXI     D,IOCxDDA       ;GET DUMMY CHANNEL TABLE
        LXI     B,40
        CALL    MOVE            ;FILL IN REAL ONE
        LXI     H,AIOxDDA       ;POINT TO AIO AREA
        LXI     D,IOCxDDA
        LXI     B,IMOVLEN
        CALL    MOVE            ;FILL IN AIO
        ORA     A               ;CLEAR CARRY
        RET
NOFILE  POP     PSW             ;RESTORE A,F
        DB      SCALL,xCLOSE
        MVI     A,ECxFNF        ;SAY FILE NOT FOUND
        STC
        RET

;       CLOSE - CLOSE FILE
;
;       ENTRY:
;       A  = CHANNEL NUMBER
;
;       EXIT:
;       A  = ERROR CODE (IF CY = 1)

CLOSE   PUSH    PSW             ;SAVE CHANNEL NUMBER
        CALL    GETCFLG         ;GET CHANNEL FLAG
        ANI     -1-1000B AND 0FFH ;MASK WRITE BIT
        ORA     A               ;ALREADY CLOSED?
        JZ      CLOSED          ;YES
        DCR     A               ;DISK DEVICE?
        JNZ     CLOSE2          ;NO, NON-DIR DEVICE
CLOSE1  POP     PSW             ;GET CHANNEL NO. AGAIN
        PUSH    PSW             ;SAVE AGAIN
        CALL    CHN2FCB         ;FIND FCB FOR THIS CHANNEL
        MVI     C,CLOSEF
        CALL    CBDOS           ;CLOSE THE FILE
        JMP     CLOSE2A
CLOSE2  CPI     3               ;AT: DEVICE?
        JNZ     CLOSE2A         ;NO
        LDA     IOBYTE          ;ELSE, SWITCH IOBYTE
        XRI     1
        STA     IOBYTE          ;TO ALTERNATE DEVICE
        DB      SCALL,xCLRCO
CLOSE2A POP     PSW             ;RESTORE CHANNEL
        PUSH    PSW             ;SAVE AGAIN
        CALL    CHN2IOC         ;FIND IOC FOR THIS CHANNEL
        INX     H               ;MOVE TO FLAG
        INX     H
        INX     H
        INX     H
        MVI     M,0             ;CLEAR IOC FLAG
        POP     PSW             ;RESTORE CHANNEL
        INR     A               ;MAKE NUMBER 0-6
        LXI     H,CHNFLGS       ;POINT TO CHANNEL FLAGS
        CALL    DADA            ;FIND THIS FLAG
        MVI     M,0             ;CLEAR THE FLAG
        ORA     A               ;CLEAR CARRY
        RET
CLOSED  POP     PSW             ;RESTORE A,F
        ORA     A               ;CLEAR CARRY
        RET

;       POSIT - POSITION FILE CURSOR
;
;       ENTRY:
;       A  = CHANNEL NUMBER
;       BC = SECTOR TO POSITION BEFORE
;
;       EXIT:
;       A  = ERROR CODE (IF CY = 1)
;       BC = UNUSED COUNT

POSIT   PUSH    PSW             ;SAVE CHANNEL NUMBER
        CALL    GETCFLG         ;GET CHANNEL FLAG
        ANI     -1-1000B AND 0FFH ;MASK WRITE BIT
        ORA     A               ;IS FILE OPEN?
        JZ      NOTOPEN         ;NO, ERROR
        DCR     A               ;IS THIS A DISK FILE?
        JZ      POSIT1          ;YES, GO ON
        POP     PSW             ;ELSE, RESTORE PSW
        MVI     A,ECxDNS        ;SAY DEVICE NOT SUITABLE
        STC
        RET
POSIT1  XRA     A
        STA     POSFLG          ;CLEAR POSIT FLAG
        POP     PSW             ;RESTORE CHANNEL NUMBER
        PUSH    PSW             ;SAVE AGAIN
        CALL    CHN2FCB         ;GET FCB FOR THIS FILE
        MOV     H,D
        MOV     L,E             ;HL = FCB
        MVI     A,R0
        CALL    DADA            ;GET RANDOM COUNT ADDRESS
        PUSH    H               ;SAVE IT
        MOV     A,M             ;GET CURRENT POSITION
        INX     H
        MOV     H,M
        MOV     L,A             ;IN HL
        SHLD    CURPOS          ;SAVE IT
        MOV     H,B
        MOV     L,C             ;HL = POSITION
        MOV     A,H
        CPI     80H             ;IS HE ASKING TOO MUCH?
        JNC     POSIT2          ;YES
        DAD     H               ;ELSE, DOUBLE IT TO GET CP/M RECORDS
        MVI     A,1
        STA     POSFLG          ;MARK REQUEST DOUBLED
POSIT2  SHLD    POSITN          ;SAVE RESULT
        MVI     C,CFS
        CALL    CBDOS           ;COMPUTE FILE SIZE
        POP     H               ;REMOVE HL FROM STACK
        POP     PSW             ;GET CHANNEL NUMBER
        PUSH    H               ;RESTORE HL
        CALL    GETCFLG         ;GET CHANNEL FLAG
        ANI     1000B           ;OPEN FOR WRITE?
        JZ      POSIT2A         ;NO
        LHLD    CURPOS          ;GET CURRENT POSITION
        XCHG                    ;IN DE
        POP     H               ;RESTORE R0 ADDRESS
        PUSH    H               ;SAVE AGAIN
        MOV     A,M             ;GET RESULT OF SIZE COMPUTE
        INX     H
        MOV     H,M
        MOV     L,A             ;IN HL
        MOV     A,L
        SUB     E               ;SUBTRACT CURRENT POSITION
        MOV     A,H             ; FROM CFS RESULT
        SBB     D               ; TO SEE WHICH IS LARGER
        JNC     POSIT2A         ;CFS IS LARGER, USE IT
        POP     H               ;ELSE, GET R0 ADDRESS
        PUSH    H               ;SAVE AGAIN
        MOV     M,E             ;AND PUT CURRENT POSITION BACK
        INX     H
        MOV     M,D
POSIT2A POP     H               ;GET R0 ADDRESS
        PUSH    H               ;SAVE AGAIN
        MOV     E,M             ;PUT SIZE IN DE
        INX     H
        MOV     D,M
        LHLD    POSITN          ;GET POSITION WANTED
        MOV     A,E             ;SUBTRACT IT FROM SIZE
        SUB     L
        MOV     A,D
        SBB     H               ;TO SEE IF IT'S TOO FAR OUT
        JC      POSIT3          ;IT IS, USE SIZE FOR POSIT
        POP     D               ;ELSE, GET R0 ADDRESS
        PUSH    D               ;SAVE IT AGAIN
        MOV     A,L             ;AND PUT POSITION WANTED IN IT
        STAX    D
        INX     D
        MOV     A,H
        STAX    D
POSIT3  POP     H               ;HL = R0 ADDRESS
        MOV     E,M             ;PUT CURRENT RECORD IN DE
        INX     H
        MOV     D,M
        LDA     POSFLG          ;GET POSIT FLAG
        ORA     A               ;WAS REQUEST DOUBLED?
        JNZ     POSIT4          ;YES
        MOV     A,D             ;ELSE, DIVIDE ACTUAL BY 2
        RAR
        MOV     D,A
        MOV     A,E
        RAR
        MOV     E,A
POSIT4  LHLD    POSITN          ;GET POSITION WANTED
        MOV     A,L             ;SUBTRACT CURRENT RECORD FROM IT
        SUB     E
        MOV     C,A             ;WITH RESULT IN BC
        MOV     A,H
        SBB     D
        MOV     B,A
        MOV     A,B             ;IS RESULT ZERO?
        ORA     C
        RZ                      ;IF SO, IT'S A GOOD POSIT
        LDA     POSFLG          ;ELSE, GET POSIT FLAG
        ORA     A               ;WAS REQUEST DOUBLED?
        JNZ     POSIT5          ;YES
        MOV     H,B             ;ELSE, DOUBLE RESULT
        MOV     L,C
        DAD     H
        MOV     B,H
        MOV     C,L
POSIT5  MVI     A,ECxEOF        ;SIGNAL END OF FILE
        STC
        RET
CURPOS  DW      0               ;CURRENT POSITION
POSITN  DW      0               ;POSITION WANTED
POSFLG  DB      0               ;POSIT FLAG

;       DELET - DELETE FILE
;
;       ENTRY:
;       HL = FILE NAME
;       DE = DEFAULTS
;
;       EXIT:
;       A  = ERROR CODE (IF CY = 1)

CWP     CALL    FNDFILE         ;TRY TO FIND FILE
        RC                      ;COULDN'T FIND IT
        MVI     A,FTYPE
        CALL    DADA            ;MOVE TO TYPE
        MOV     A,M             ;GET T1
        RAL                     ;TEST FOR WRITE PROTECT
        RNC                     ;FILE NOT WRITE PROTECTED
        MVI     A,ECxWPV        ;SAY WRITE PROTECTED
        RET
DELET   CALL    CWP             ;CHECK IF FILE WRITE PROTECTED
        RC                      ;IT IS
DELET2  MVI     C,DELETE
        CALL    BDOS            ;DELETE FILE
        INR     A               ;GOOD DELETE?
        ORA     A               ;CLEAR CARRY
        RNZ                     ;YES, RETURN
        MVI     A,ECxFNF        ;SAY FILE NOT FOUND
        STC
        RET

;       RENAME - RENAME A FILE
;
;       ENTRY:
;       HL = OLD FILE NAME
;       DE = OLD FILE DEFAULTS
;       BC = NEW FILE NAME (INCLUDING DEFAULTS)
;
;       EXIT:
;       A  = ERROR CODE (IF CY = 1)

RENAME  PUSH    H               ;SAVE OLD NAME, DEFAULTS
        PUSH    D
        PUSH    B               ;SAVE NEW NAME
        MOV     H,B
        MOV     L,C             ;HL = NEW NAME
        CALL    FNDFILE         ;DOES IT EXIST?
        JC      RENAME1         ;NO, GO AHEAD
        MVI     A,ECxFAP        ;SAY FILE ALREADY PRESENT
        POP     B               ;ELSE, FIX STACK
RENERR  POP     D
        POP     H
        STC
        RET
RENAME1 POP     B               ;GET NEW NAME
        MOV     H,B
        MOV     L,C             ;HL = NEW NAME
        LXI     D,DEFALT        ;ANY ANY OLD DEFAULTS
        MVI     A,6             ;USE CHANNEL 6 (DIR CHAN)
        CALL    CFN             ;CRACK NEW NAME
        JC      RENERR          ;BAD NAME
        INX     D               ;SKIP DRIVE CODE
        LXI     B,11
        LXI     H,RENTMP        ;PUT NAME HERE
        CALL    MOVE            ;MOVE IT
        POP     D               ;GET OLD NAME PARAMETERS
        POP     H
        CALL    CWP             ;CHECK FILE WP STATUS
        RC                      ;ERROR
        LXI     D,RENTMP        ;POINT TO NEW NAME
        LXI     H,FCB6+17       ;PUT IT HERE
        LXI     B,11
        CALL    MOVE            ;MOVE IT IN
        LXI     D,FCB6          ;POINT TO DIR FCB
        MVI     C,RENAMF
        CALL    BDOS            ;RENAME THE FILE
        INR     A               ;OK?
        ORA     A               ;CLEAR CARRY
        RNZ
        MVI     A,ECxFNF        ;SAY FILE NOT FOUND
        STC
        RET
RENTMP  DS      11              ;TEMPORARY NAME STORAGE

;       SETTP - SET USER MEMORY TOP
;
;       ENTRY:
;       HL = REQUESTED MEMORY TOP
;
;       EXIT:
;       IF CY = 1
;        HL = ACTUAL MEMORY TOP
;        A  = ERROR CODE (NOT ENOUGH MEMORY)

SETTP   XCHG                    ;DE = REQUESTED TOP
        LHLD    SxSYSM          ;GET ACTUAL TOP
        MOV     A,L             ;SUBTRACT REQUESTED TOP
        SUB     E               ; TO SEE IF IT'S ALLOWED
        MOV     A,H
        SBB     D
        MVI     A,ECxNEM        ;ASSUME NOT ENOUGH MEMORY
        RC                      ;AND RETURN IF NOT ENOUGH
        XCHG                    ;ELSE, PUT REQUEST IN HL
        SHLD    SxUSRM          ;AND SET USER MEMORY LIMIT
        RET

;       DECODE - DECODE FILE NAME
;
;       ENTRY:
;       BC = TABLE AREA
;       HL = FILE NAME
;       DE = DEFAULTS
;
;       EXIT:
;       A  = ERROR CODE IF CY = 1

DECODE  PUSH    B               ;SAVE TABLE ADDRESS
        PUSH    H               ;SAVE NAME ADDRESS
        LXI     H,DCDEV         ;POINT TO DEVICE AREA
        LXI     B,3
        CALL    MOVE            ;MOVE IN DEFAULT DEVICE
        LXI     B,3
        LXI     H,DCEXT         ;POINT TO EXTENSION AREA
        CALL    MOVE            ;MOVE IN DEFAULT EXTENSION
        POP     H               ;GET NAME AGAIN
        CALL    SOB             ;SKIP BLANKS
        CALL    MCU             ;MAP CHARACTER TO UPPER CASE
        MVI     B,0             ;SET NULL NAME FLAG
        CPI     '.'             ;NAMELESS EXTENSION?
        JZ      DECODE1         ;YES
        CPI     'A'             ;TEST FOR NON-ALPHA
        JC      DECODE4
        CPI     'Z'+1
        JNC     DECODE4         ;NON-ALPHA FOUND
DECODE1 CALL    GETA            ;GET ALPHA STRING
        JC      DECODEE         ;ERROR
        CPI     ':'             ;EXPLICIT DEVICE?
        JNZ     DECODE2         ;NO
        INX     H               ;ELSE, SKIP ':'
        MVI     A,3             ;3 CHARACTERS ALLOWED IN DEVICE
        CMP     C               ;CHECK
        JNC     DECOD1A         ;IT'S OK
        MVI     A,ECxIDN        ;SAY ILLEGAL DEVICE
        POP     B               ;FIX STACK
        RET
DECOD1A LXI     B,3             ;MOVE 3 CHARACTERS
        PUSH    H               ;SAVE HL
        LXI     H,DCDEV         ;POINT TO DEVICE AREA
        CALL    MOVE            ;MOVE IN NEW DEVICE
        POP     H               ;RESTORE HL
        CALL    GETA            ;GET NEXT STRING
        JC      DECODEE         ;TOO LONG
DECODE2 LXI     B,8             ;8 CHARACTERS IN NAME
        PUSH    H               ;SAVE HL
        LXI     H,DCNAME        ;GET NAME AREA
        CALL    MOVE            ;MOVE IN NAME
        POP     H               ;RESTORE HL
        MOV     A,M             ;GET NEXT CHARACTER
        CPI     '.'             ;EXTENSION NEXT?
        JNZ     DECODE3         ;NO
        INX     H               ;ELSE, SKIP PERIOD
        CALL    GETA            ;GET EXTENSION
        JC      DECODEE
        MVI     A,3             ;3 CHARACTERS ALLOWED
        CMP     C               ;CHECK
        JC      DECODEE         ;TOO MANY
        PUSH    H               ;SAVE HL
        LXI     H,DCEXT         ;POINT TO EXTENSION AREA
        LXI     B,3
        CALL    MOVE            ;MOVE IN EXTENSION
        POP     H               ;RESTORE HL
DECODE3 MVI     B,1             ;SET NON NULL NAME FLAG
DECODE4 MOV     C,L             ;GET ADDRESS LOW
        CALL    SOB             ;CHECK FOR BLANKS
        MOV     A,C             ;GET ADDRESS LOW
        SUB     L               ;SEE IF ANY BLANKS
        ANA     A               ;SET FLAGS
        MOV     A,M             ;GET DELIMITER CHARACTER
        CZ      CFD             ;CHECK IT
        JC      DECODEE         ;BAD DELIMITER
        MOV     A,B             ;GET NAME FLAG
        ANA     A               ;SET FLAGS
        JZ      DECODE5         ;Z = NULL NAME
        LDA     DCNAME          ;GET FIRST NAME CHARACTER
        ORA     A               ;NULL NAME?
        JZ      DECODE5         ;YES
        CPI     'A'             ;TEST FOR ALPHA
        JC      DECODEE         ;BAD NAME
        CPI     'Z'+1
        JNC     DECODEE         ;BAD NAME
        MVI     A,1             ;ELSE, CLEAR FLAGS
        ORA     A
DECODE5 DB      SCALL,xVERS     ;GET VERSION NUMBER
        STA     DCVER           ;PUT IT IN
        LDA     DCDEV+2         ;GET UNIT
        ORA     A               ;ANY SPECIFIED?
        JZ      DECODE6         ;NO
        SUI     '0'             ;ELSE, REMOVE ASCII BIAS
DECODE6 STA     DCDEV+2         ;DEVICE MUST BE BINARY
        LXI     D,DCDEV         ;POINT TO DECODED DEVICE
        LXI     H,DEVTBL        ;AND DEVICE TABLE
        LXI     B,102H          ;B = OPEN FLAG, C = # CHARS
DECODE7 PUSH    B               ;SAVE REGISTERS
        PUSH    D
        PUSH    H
        CALL    COMP            ;IS THIS OUR DEVICE?
        POP     H               ;RESTORE REGISTERS
        POP     D
        POP     B
        JZ      DECODE8         ;YES
        INR     B               ;INCREMENT FLAG
        MVI     A,14            ;ELSE, MOVE TO NEXT DEVICE
        CALL    DADA
        MOV     A,M             ;GET FIRST DEVICE CHAR
        ORA     A               ;END OF TABLE?
        JNZ     DECODE7         ;NO, KEEP LOOKING
        MVI     A,ECxUND        ;ELSE, SAY UNKNOWN DEVICE
        POP     B               ;RESTORE BC
        STC
        RET
DECODE8 MOV     A,B             ;GET OPEN FLAG
        STA     OPFLG           ;SAVE IT
        SHLD    DCDVT           ;STORE DEVICE TABLE ADDRESS
        SHLD    IOCxDTA         ;ALSO IN IOC
        MVI     A,4
        CALL    DADA            ;MOVE TO DEVICE ADDRESS
        PUSH    H               ;SAVE THIS ADDRESS
        CALL    HLIHL           ;GET DEVICE ADDRESS
        SHLD    IOCxDDA         ;SAVE IN IOC
        POP     H
        INX     H               ;MOVE TO CAPABILITY FLAG
        INX     H
        MOV     A,M             ;GET IT
        STA     DCFLG           ;STORE IT
        POP     B               ;GET DECODE TABLE ADDRESS
        MOV     H,B
        MOV     L,C             ;IN HL
        LXI     D,DCFLG         ;POINT TO OUR TABLE
        LXI     B,19            ;MOVE 19 CHARACTERS
        CALL    MOVE            ;MOVE DECODE INFO
        LXI     H,IOCxDEV       ;POINT TO IOC DIRECTORY AREA
        LXI     D,DCDEV         ;AND DECODED INFO
        LXI     B,16            ;16 CHARACTERS
        CALL    MOVE            ;UPDATE IOC
        ORA     A               ;CLEAR CARRY
        RET
DECODEE POP     B               ;RESTORE BC
        MVI     A,ECxIFN        ;ILLEGAL FILE NAME
        STC
        RET
DCFLG   DS      1
DCDEV   DS      3
DCNAME  DS      8
DCEXT   DS      3
        DB      0               ;PROJECT
DCVER   DB      20H             ;VERSION
DCDVT   DW      0               ;DEVICE TABLE ADDRESS
OPFLG   DB      0               ;OPEN FLAG (USED LATER)

GETA    LXI     D,GETAA         ;POINT TO TEMPORARY AREA
        MVI     C,9             ;SIZE OF AREA
        MOV     B,C             ;MAX ALLOWED + 1
        XRA     A
GETA1   STAX    D               ;ZERO BUFFER
        INX     D
        DCR     C
        JNZ     GETA1
        LXI     D,GETAA
GETA2   MOV     A,M             ;GET A CHARACTER
        CALL    MCU             ;MAP TO UPPER CASE
        CPI     '0'             ;TEST FOR ALPHA-NUMERIC
        JC      GETA4           ;END OF STRING
        CPI     '9'+1
        JC      GETA3           ;NUMERIC
        CPI     'A'
        JC      GETA4           ;END OF STRING
        CPI     'Z'+1
        JNC     GETA4           ;END
GETA3   STAX    D               ;STORE CHARACTER
        INX     D
        INX     H               ;INCREMENT POINTERS
        INR     C               ;INCREMENT COUNT
        DCR     B               ;DECREMENT LIMIT
        JNZ     GETA2           ;LOOP UNTIL DONE
        STC                     ;OVERFLOW
        RET
GETA4   ANA     A               ;CLEAR CARRY
        LXI     D,GETAA         ;DE = POINTER
        RET
GETAA   DS      9               ;9 CHARACTER AREA

CFD     ANA     A               ;TEST DELIMITER CHARACTER
        RZ                      ;IT'S A ZERO
        CPI     ','
        RZ
        CPI     '='
        RZ
        CPI     '/'
        RZ
        CPI     ' '
        RZ
        STC
        RET

MCU     CPI     'a'             ;LESS THAN "a"?
        RC
        CPI     'z'+1           ;MORE THAN "z"?
        RNC
        ANI     5FH             ;CAPITALIZE
        RET

;       NAME - RETURN FILE NAME
;
;       ENTRY:
;       A  = CHANNEL NUMBER
;       HL = ADDRESS FOR NAME
;       DE = ADDRESS FOR DEFAULTS
;
;       EXIT:
;       A  = ERROR CODE (IF CY = 1)

NAME    PUSH    PSW             ;SAVE CHANNEL NO.
        CALL    GETCFLG         ;GET CHANNEL FLAG
        ORA     A               ;IS CHANNEL OPEN?
        JZ      NOTOPEN         ;NO
        POP     PSW             ;ELSE, GET CHANNEL NO. AGAIN
        PUSH    H               ;SAVE NAME ADDRESS
        CALL    CHN2IOC         ;GET I/O CHANNEL ADDRESS
        MVI     A,IOCxDEV-IOCxLNK ;OFFSET TO DEVICE NAME
        CALL    DADA            ;GO THERE
        MOV     A,M             ;GET A CHARACTER
        STAX    D               ;PUT IT IN DEFAULT AREA
        INX     H
        INX     D
        MOV     A,M             ;GET SECOND CHARACTER
        STAX    D               ;PUT IT AWAY
        INX     H
        INX     D
        MOV     A,M             ;GET UNIT NO.
        ADI     '0'             ;ADD ASCII BIAS
        STAX    D               ;PUT IT AWAY
        INX     D               ;DE = EXTENSION AREA
        INX     H               ;HL = NAME IN IOC
        POP     B               ;BC = PLACE TO STORE NAME
        PUSH    D               ;SAVE EXTENSION AREA
        XCHG                    ;DE = NAME IN IOC
        MOV     H,B
        MOV     L,C             ;BC = PLACE FOR NAME
        LXI     B,8             ;8 CHARACTERS
        CALL    MOVE            ;MOVE IN NAME
        POP     H               ;GET PLACE FOR EXTENSION
        LXI     B,3             ;3 CHARACTERS
        CALL    MOVE            ;MOVE IT IN
        RET

;       CLEAR - CLEAR I/O CHANNEL
;
;       ENTRY:
;       A  = CHANNEL NUMBER
;
;       EXIT:
;       A  = ERROR CODE (IF CY = 1)

CLEAR   PUSH    PSW             ;SAVE CHANNEL NO.
        CALL    CHN2IOC         ;FIND IOC FOR THIS CHANNEL
        MVI     A,4             ;OFFSET TO FLAG
        CALL    DADA            ;GO THERE
        MVI     M,0             ;FLAG AS CLOSED
        POP     PSW             ;RESTORE CHANNEL NO.
        INR     A               ;MAKE CHANNEL 0-6
        LXI     H,CHNFLGS       ;POINT TO CHANNEL FLAGS
        CALL    DADA            ;FIND OUR CHANNEL
        MVI     M,0             ;CLEAR IT
        ORA     A               ;CLEAR CARRY
        RET

;       CLEARA - CLEAR ALL I/O CHANNELS
;
;       ENTRY: NONE

CLEARA  MVI     A,-1 AND 0FFH   ;SET CHANNEL COUNTER
CLEARA1 PUSH    PSW             ;SAVE CHANNEL NUMBER
        DB      SCALL,xCLEAR    ;CLEAR A CHANNEL
        POP     PSW             ;RESTORE NUMBER
        INR     A               ;INCREMENT IT
        CPI     6               ;DONE?
        JNZ     CLEARA1         ;LOOP UNTIL DONE
        ORA     A               ;CLEAR CARRY
        RET

;       ERROR - PROCESS ERROR MESSAGE REQUEST
;
;       ENTRY:
;       A  = ERROR CODE
;       H  = TRAILING CHARACTER

ERROR   PUSH    H               ;SAVE TRAILING CHARACTER
        ORA     A               ;ERROR #0?
        JNZ     ERROR1          ;NO
        CALL    DATE3           ;ELSE, PRINT SIGN-ON
        JMP     ERROR2          ;AND MOVE ON
ERROR1  PUSH    PSW             ;SAVE ERROR CODE
        CALL    TYPTX
        DB      '?02 SYSTEM ERROR ','#'+80H
        POP     PSW             ;GET ERROR CODE
        MOV     C,A
        MVI     B,0             ;IN BC
        MVI     A,3             ;3 DIGIT NUMBER
        LXI     H,ERRNO         ;PRINT IT HERE
        CALL    UDD             ;CONVERT TO DECIMAL
        CALL    TYPTX
ERRNO   DB      '000',80H
ERROR2  POP     H               ;GET TRAILING CHARACTER
        MOV     A,H
        DB      SCALL,xSCOUT    ;PRINT IT
        ORA     A               ;CLEAR CARRY
        RET

;       CHFLG - CHANGE FILE FLAGS
;
;       ENTRY:
;       B  = NEW VALUES
;       C  = MASK
;       HL = FILE NAME
;       DE = DEFAULTS
;
;       EXIT:
;       A  = ERROR CODE (IF CY = 1)

CHFLG   PUSH    B               ;SAVE FLAG DATA
        CALL    FNDFILE         ;TRY TO FIND THE FILE
        POP     B               ;RESTORE DATA
        RC                      ;COULDN'T FIND IT
        PUSH    H               ;SAVE FILE NAME ADDRESS
        MVI     A,FTYPE         ;MOVE TO FILE TYPE
        CALL    DADA
        MOV     A,B
        ANA     C               ;MASK OUT UNWANTED BITS
        MOV     C,A             ;SAVE FLAGS
        ANI     40Q             ;WANT WRITE PROTECT FLAG?
        JZ      FLAG1           ;NO
        MOV     A,M             ;GET T1 BYTE
        ORI     80H             ;SET WRITE PROTECT FLAG
        MOV     M,A             ;REPLACE IT
        JMP     FLAG2
FLAG1   MOV     A,M             ;GET T1 BYTE
        ANI     7FH             ;KILL WRITE PROTECT FLAG
        MOV     M,A             ;REPLACE IT
FLAG2   INX     H               ;MOVE TO T2 BYTE
        MOV     A,C             ;GET FLAGS WANTED
        ANI     200Q            ;WANT SYSTEM FLAG?
        JZ      FLAG3           ;NO
        MOV     A,M             ;GET T2 BYTE
        ORI     80H             ;SET SYSTEM FLAG
        MOV     M,A             ;REPLACE IT
        JMP     FLAG4
FLAG3   MOV     A,M             ;GET T2 BYTE
        ANI     7FH             ;KILL SYSTEM FLAG
        MOV     M,A             ;REPLACE IT
FLAG4   POP     D               ;RESTORE FCB ADDRESS
        LDA     FCB6            ;GET DRIVE CODE
        STAX    D               ;PUT IT IN THIS FCB
        MVI     C,FLAGS
        CALL    BDOS            ;CHANGE THE FLAGS
        ORA     A               ;CLEAR CARRY
        RET

;       LOADD - LOAD DEVICE DRIVER
;
;       ENTRY
;       HL = DEVICE DESCRIPTOR
;
;       EXIT
;       HL = DEVICE TABLE ADDRESS
;       A  = ERROR CODE (IF CY = 1)

LOADD   LXI     D,DEFALT        ;USE ANY DEFAULTS
        LXI     B,DTABLE        ;USE CFN TABLE
        DB      SCALL,xDECODE   ;TRY TO DECODE NAME
        RC                      ;COULDN'T DO IT
        LHLD    DCDVT           ;ELSE, GET DEVICE TABLE ADDRESS
        RET

;       OVERLAY 1 SYSTEM CALLS

;       MOUNT, DMOUN, MONMS, DMNMS
;       (DISK MOUNT AND DISMOUNT)

MOUNT
MONMS   JMP     IGNORE          ;IGNORE MOUNT

DMOUN
DMNMS   MVI     C,RESET
        CALL    CBDOS           ;RESET DISK SYSTEM
        ORA     A               ;CLEAR CARRY
        RET

;       RESET - RESET A DISK
;
;       THIS IS PROCESSED ELSEWHERE, BY RESET1

;;;;;;;;;;;;;;;;;;;;;;;;;
;                       ;
;       SUBROUTINES     ;
;                       ;
;;;;;;;;;;;;;;;;;;;;;;;;;

;       CP/M INTERFACE SUBROUTINES

;       DCIN - DIRECT CONSOLE INPUT
;       RETURNS CONSOLE CHARACTER IN A

DCIN    PUSH    H
        PUSH    D
        PUSH    B
        MVI     C,DCIO
        MVI     E,0FFH          ;FLAG INPUT
        CALL    BDOS
        ORA     A               ;SET FLAGS
        POP     B
        POP     D
        POP     H
        RET

;       CHOUT - CONSOLE OUTPUT
;       OUTPUTS CHARACTER IN A

CHOUT   PUSH    H
        PUSH    D
        PUSH    B
        PUSH    PSW
        MOV     E,A             ;SAVE CHARACTER IN E
        LDA     SxCONFL         ;GET CONSOLE FLAG
        RAR                     ;TEST FOR CONTROL-O
        JC      CHOUT1          ;SET, DON'T PRINT
        RAR                     ;TEST FOR CONTROL-P
        JNC     CHOUT0          ;NOT SET
        MVI     C,LSTOUT        ;ELSE, USE LSTOUT FUNCTION
        CALL    CBDOS           ;PRINT CHARACTER
CHOUT0  MVI     C,DCIO
        CALL    BDOS
CHOUT1  POP     PSW
        POP     B
        POP     D
        POP     H
        RET

;       CBDOS - CALL BDOS FUNCTIONS
;
;       ENTRY:
;       C  = FUNCTION
;       DE = PARAMETER
;
;       EXIT:
;       A  = RESULT

CBDOS   PUSH    H
        PUSH    D
        PUSH    B
        CALL    BDOS
        POP     B
        POP     D
        POP     H
        RET

;       DBLKIO - DISK BLOCK I/O
;
;       ENTRY:
;       A  = FUNCTION (READR OR WRITER)
;       DE = FCB OF FILE
;       HL = ADDRESS TO READ INTO OR WRITE FROM
;       BC = NUMBER OF RECORDS
;
;       EXIT:
;       HL = NEXT BYTE AFTER LAST RECORD
;       BC = UNUSED RECORDS (IF END FOUND OR ERROR)
;       DE = UNCHANGED
;       CY = 1 IF END FOUND OR ERROR

DBLKIO  STA     DBFUNC          ;SAVE FUNCTION
DBLKIO1 PUSH    B               ;SAVE COUNT
        XCHG                    ;DE = ADDRESS
        MVI     C,SETDMA
        CALL    CBDOS           ;SET DMA ADDRESS
        XCHG                    ;DE = FCB
        LDA     DBFUNC          ;GET FUNCTION
        MOV     C,A             ;IN C
        CALL    CBDOS           ;READ/WRITE A RECORD
        ORA     A               ;OPERATION OK?
        JNZ     DBLKIO2         ;NO, EXIT
        LXI     B,80H
        DAD     B               ;UPDATE ADDRESS
        PUSH    H               ;SAVE ADDRESS
        MOV     H,D
        MOV     L,E             ;HL = FCB
        MVI     A,R0            ;LOCATE R0
        CALL    DADA
        MOV     C,M             ;PUT IT IN BC
        INX     H
        MOV     B,M
        INX     B               ;INCREMENT R0
        MOV     M,B             ;AND REPLACE IT
        DCX     H
        MOV     M,C
        POP     H               ;RESTORE ADDRESS
        POP     B               ;RESTORE COUNT
        DCX     B               ;DECREMENT IT
        MOV     A,B
        ORA     C               ;TEST
        JNZ     DBLKIO1         ;CONTINUE IF NOT DONE
        RET                     ;ELSE, RETURN
DBLKIO2 POP     B               ;RESTORE COUNT
        STC                     ;MARK ERROR
        RET
DBFUNC  DB      0               ;FUNCTION

;       OTHER SUBROUTINES

;       INSTR - SEARCH STRING FOR SUBSTRING
;
;       HL = STRING
;       DE = SUBSTRING
;       B  = STRING LENGTH
;       C  = SUBSTRING LENGTH
;
;       RETURNS Z FLAG IF FOUND, WITH HL = END OF SUBSTRING

INSTR   MOV     A,B
        SUB     C               ;SUBTRACT SUB LENGTH FROM LENGTH
        MOV     B,A
INSTR0  PUSH    H               ;SAVE REGISTERS
        PUSH    D
        PUSH    B
        CALL    COMP            ;COMPARE STRINGS
        POP     B
        POP     D
        POP     H
        JZ      INSTR1          ;FOUND IT
        DCR     B               ;DONE?
        RM                      ;NO MATCH, RETURN
        INX     H               ;INCREMENT STRING POINTER
        JMP     INSTR0          ;TRY AGAIN
INSTR1  MVI     B,0             ;BC = SUB LENGTH
        DAD     B
        XRA     A               ;SET ZERO
        RET

;       SOB - SKIP OVER BLANKS

SOB     MOV     A,M             ;GET A CHARACTER
        CPI     ' '             ;SPACE?
        RNZ                     ;RETURN IF NOT
        INX     H               ;ELSE, INCREMENT POINTER
        JMP     SOB             ;TRY AGAIN

;       SJMP - SCALL TABLE JUMP PROCESSOR

SJMP    RLC                     ;DOUBLE OFFSET KEY
        XTHL                    ;GET RETURN ADDR
        CALL    DADA            ;ADD TABLE OFFSET
        MOV     A,M             ;GET TABLE ADDRESS
        INX     H
        MOV     H,M
        MOV     L,A
        LDA     SxCACC          ;GET USER'S A REGISTER
        XTHL                    ;PUT ROUTINE ADDR ON STACK
        RET                     ;AND GO TO IT

;       SEC2REC - CONVERT HDOS SECTORS TO CP/M RECORDS
;
;       ENTRY:
;       B  = NO. OF HDOS SECTORS
;       C  = 0 (OR BC = BYTES)
;
;       EXIT:
;       BC = NO. OF CP/M RECORDS

SEC2REC PUSH    H               ;SAVE HL
        MOV     L,B
        MVI     H,0             ;HL = NO OF SECTORS
        DAD     H               ;DOUBLE IT TO GET RECORDS
        MOV     B,H
        MOV     C,L             ;PUT RESULT IN BC
        POP     H
        RET

;       REC2SEC - CONVERT CP/M RECORDS TO HDOS SECTORS
;
;       ENTRY:
;       BC = NO. OF CP/M RECORDS
;
;       EXIT:
;       B  = NO. OF HDOS SECTORS
;       C  = 0 (OR BC = BYTES)

REC2SEC PUSH    PSW             ;SAVE A,F
        ORA     A               ;CLEAR CARRY
        MOV     A,B             ;GET HIGH BYTE
        RAR                     ;DIVIDE BY 2
        MOV     B,A
        MOV     A,C             ;GET LOW BITE
        RAR                     ;DIVIDE BY 2 (WITH CARRY)
        MOV     C,A
        MOV     B,C             ;B = NO. OF SECTORS
        MVI     C,0             ;C = 0
        POP     PSW
        RET

;       CHN2FCB - CONVERT HDOS CHANNEL NO. TO CP/M FCB ADDR
;
;       ENTRY:
;       A  = CHANNEL NUMBER (-1 THROUGH 5)
;
;       EXIT:
;       DE = FCB ADDRESS

CHN2FCB INR     A               ;MAKE CHANNELS IN 0-6 RANGE
        PUSH    H               ;SAVE HL
        LXI     H,FCBS          ;POINT TO FCB'S
        LXI     D,36            ;SIZE OF ONE FCB
C2F0    JZ      C2F1            ;EXIT IF DONE
        DAD     D               ;MOVE TO NEXT FCB
        DCR     A               ;DECREMENT COUNT
        JMP     C2F0            ;TEST IF DONE
C2F1    XCHG                    ;DE = FCB ADDRESS
        POP     H               ;RESTORE HL
        RET

;       CP/M FCB'S

FCBS
FCBM1   DS      36              ;CHANNEL -1
FCB0    DS      36              ;CHANNEL 0
FCB1    DS      36              ;CHANNEL 1
FCB2    DS      36              ;CHANNEL 2
FCB3    DS      36              ;CHANNEL 3
FCB4    DS      36              ;CHANNEL 4
FCB5    DS      36              ;CHANNEL 5
FCB6    DS      36              ;DIRECTORY CHANNEL

;       CHANNEL FLAGS
;       0 = CLOSED, 1 = OPEN FOR DISK, 2 = OPEN FOR PRINT, 3 = TT:

CHNFLGS
FLGM1   DB      0               ;CHANNEL -1 FLAG
FLG0    DB      0               ;CHANNEL 0
FLG1    DB      0               ;CHANNEL 1
FLG2    DB      0               ;CHANNEL 2
FLG3    DB      0               ;CHANNEL 3
FLG4    DB      0               ;CHANNEL 4
FLG5    DB      0               ;CHANNEL 5

;       GETCFLG - GET CHANNEL FLAG
;       ENTER WITH A = CHANNEL NO.
;       EXIT WITH A = CHANNEL FLAG

GETCFLG PUSH    H               ;SAVE HL
        LXI     H,CHNFLGS       ;POINT TO CHANNEL FLAGS
        INR     A               ;MAKE CHANNEL NO. 0-6
        CALL    DADA            ;MOVE TO OUR FLAG
        MOV     A,M             ;GET IT
        POP     H               ;RESTORE HL
        RET

;       CHN2IOC - GET IOC LOCATION FROM CHANNEL NO.
;       ENTER WITH A = CHANNEL NO.
;       EXIT WITH HL = IOC ADDRESS

CHN2IOC INR     A               ;MAKE CHANNEL IN 0-N RANGE
        LHLD    SxCFWA          ;GET FIRST IOC LOCATION
CHNIOC0 ORA     A               ;DONE?
        RZ                      ;RETURN IF SO
        PUSH    PSW             ;ELSE, SAVE CHANNEL NUMBER
        CALL    HLIHL           ;GET NEXT CHANNEL ADDRESS
        POP     PSW             ;RESTORE CHANNEL NO.
        DCR     A               ;DECREMENT IT
        JMP     CHNIOC0         ;AND TEST IF DONE

;       CFN - CRACK FILE NAME
;
;       ENTRY:
;       HL = FILE NAME
;       DE = DEFAULTS
;       A  = CHANNEL NUMBER
;
;       EXIT:
;       DE = FCB ADDRESS IF DISK
;       H  = 1 IF SY:, 2 IF LP:, 3 IF TT:, 4 IF AT:, 5 IF ND:
;       L  = DEVICE CAPABILITIES FLAG
;       CY = 1 IF ERROR, AND A = ERROR CODE

CFN     PUSH    PSW             ;SAVE CHANNEL NO.
        PUSH    B               ;SAVE BC
        LXI     B,DTABLE        ;POINT TO DECODE TABLE
        DB      SCALL,xDECODE   ;DECODE FILE NAME
        POP     B               ;RESTORE BC
        JNC     CFNG            ;GOOD FILE NAME
        POP     D               ;ELSE, FIX STACK
        RET                     ;RETURN, BAD FILE NAME
CFNG    POP     PSW             ;GET CHANNEL NUMBER AGAIN
        CALL    CHN2FCB         ;LOCATE FCB FOR THIS CHANNEL
        PUSH    D               ;SAVE IT
        PUSH    B               ;AND BC
        MVI     B,36
        XCHG                    ;HL = FCB
        CALL    ZERO            ;CLEAR FCB
        POP     B               ;RESTORE BC
        POP     D               ;AND FCB
        LXI     H,DTABLE        ;POINT TO DECODE TABLE
        MOV     A,M             ;GET DEVICE FLAG
        ANI     1               ;IS IT A DISK DEVICE?
        JNZ     CFN1            ;YES, MAKE FCB
CFN0    LDA     OPFLG           ;GET OPEN FLAG
        MOV     H,A             ;IN H
        LDA     DTABLE          ;GET DEVICE CAPABILITY FLAG
        MOV     L,A             ;IN L
        ORA     A               ;CLEAR CARRY
        RET
CFN1    PUSH    D               ;SAVE FCB ADDRESS
        LDA     IOCxUNI         ;GET UNIT NUMBER
        INR     A               ;MAKE IT 1-N
        STAX    D               ;STORE IT IN FCB
        INX     D
        LXI     H,DTABLE+4      ;POINT TO NAME
        PUSH    B               ;SAVE BC
        MVI     B,11            ;SET A COUNTER
CFN2    MOV     A,M             ;GET A CHARACTER
        ORA     A               ;IS IT ZERO?
        JNZ     CFN3            ;NO
        MVI     A,' '           ;ELSE, REPLACE WITH SPACE
CFN3    STAX    D               ;STORE IT IN FCB
        INX     D               ;INCREMENT POINTERS
        INX     H
        DCR     B               ;DECREMENT COUNTER
        JNZ     CFN2            ;LOOP UNTIL DONE
        POP     B               ;RESTORE BC
        POP     D               ;AND FCB ADDRESS
        JMP     CFN0            ;GET FLAGS AND EXIT
DTABLE  DS      19              ;DECODE TABLE

;       FNDFILE - FIND FILE
;
;       ENTRY:
;       HL = FILE NAME
;       DE = DEFAULTS
;
;       EXIT:
;       HL = CP/M FILE DESCRIPTOR IF FOUND
;       A  = FILE NOT FOUND ERROR IF NOT, AND CY = 1

FNDFILE MVI     A,6             ;USE CHANNEL 6 (DIR CHANNEL)
        CALL    CFN             ;CRACK FILE NAME
        RC                      ;BAD FILE NAME
FNDFIL0 MVI     C,SETDMA
        PUSH    D               ;SAVE FCB
        LXI     D,80H           ;USE DEFAULT DMA
        CALL    CBDOS           ;SET IT
        POP     D               ;GET FCB
        MVI     C,SFF
FNDNXT1 CALL    CBDOS           ;TRY TO FIND FILE
        INR     A               ;FOUND THE FILE?
        JNZ     FNDFIL1         ;YES
        MVI     A,ECxFNF        ;ELSE, SAY NOT FOUND
        STC
        RET
FNDFIL1 DCR     A               ;FIX A
        ADD     A               ;GET OFFSET TO FILE
        ADD     A
        ADD     A
        ADD     A
        ADD     A
        LXI     H,80H           ;POINT TO DMA
        CALL    DADA            ;FIND OUR FILE
        RET

;       FNDNEXT - FIND NEXT FILE

FNDNEXT MVI     C,SFN           ;SEARCH FOR NEXT
        JMP     FNDNXT1         ;FIND THE FILE

;        HDOS TABLES, ETC.

DVD     ORA     A               ;DUMMY DISK DEVICE DRIVER
        RET
LPDVD   ORA     A               ;WANTS TO READ?
        JZ      READ0+1         ;CAN'T DO IT
        DCR     A               ;WANTS TO WRITE?
        JZ      WRITE0-1        ;OK, DO IT
        ORA     A               ;ELSE, IGNORE REQUEST
        RET
TTDVD   ORA     A               ;WANTS TO READ?
        JZ      READ3           ;OK, DO IT
        DCR     A               ;WANTS TO WRITE?
        JZ      WRITE3          ;OK, DO IT
        ORA     A               ;IGNORE OTHER REQUESTS
        RET
ATDVD   LDA     IOBYTE
        XRI     1               ;SWITCH CONSOLE DEVICE
        STA     IOBYTE
        CALL    TTDVD           ;USE TT
        LDA     IOBYTE
        XRI     1               ;SWITCH CONCOLE BACK
        STA     IOBYTE
        RET
NDDVD   ORA     A               ;WANTS TO READ?
        JZ      NDREAD          ;OK
        RET

;       DEVICE TABLE

DEVTBL                          ;DEVICE TABLES
        DB      'SY'            ;SY DEVICE TABLE
        DB      11B             ;IN MEMORY, LOCKED
        DB      0C3H            ;JMP INSTRUCTION
        DW      DVD
        DB      1111B           ;DISK DEVICE: READ, WRITE, RANDOM
        DB      111111B         ;SIX UNITS MOUNTED
        DB      6               ;SIX UNITS MAX
        DW      UNITBL          ;UNIT TABLE ADDRESS
        DW      0               ;DRIVER USES NO SPACE
        DB      0               ;USES NO DISK SPACE
        DB      'LP'            ;LP DEVICE
        DB      11B             ;IN MEMORY, LOCKED
        DB      0C3H            ;JMP INSTRUCTION
        DW      LPDVD
        DB      10100B          ;CAN DO CHARACTER, WRITE
        DB      1B              ;ONE UNIT MOUNTED
        DB      1               ;ONE UNIT ONLY
        DW      UNITBL1
        DW      0
        DB      0
        DB      'TT'            ;TT DEVICE
        DB      11B             ;IN MEMORY, LOCKED
        DB      0C3H            ;JMP INSTRUCTION
        DW      TTDVD
        DB      10110B          ;CHARACTER, READ, WRITE
        DB      1B              ;ONE UNIT MOUNTED
        DB      1               ;ONLY ONE UNIT
        DW      UNITBL1
        DW      0
        DB      0
        DB      'AT'            ;AT DEVICE
        DB      11B             ;IN MEMORY, LOCKED
        DB      0C3H            ;JMP INSTRUCTION
        DW      ATDVD
        DB      10110B          ;CHARACTER, READ, WRITE
        DB      1B              ;ONE UNIT MOUNTEC
        DB      1               ;ONLY ONE UNIT
        DW      UNITBL1
        DW      0
        DB      0
        DB      'ND'            ;ND DEVICE
        DB      11B             ;IN MEMORY, LOCKED
        DB      0C3H            ;JMP INSTRUCTION
        DW      NDDVD
        DB      110B            ;READ, WRITE
        DB      1B              ;ONE UNIT MOUNTED
        DB      1               ;ONLY ONE UNIT
        DW      UNITBL1
        DW      0
        DB      0
        DB      0               ;FLAG END OF TABLE
UNITBL
        DB      1111B           ;UNIT FLAG
        DB      4               ;4 SECTORS/GROUP
        DW      0               ;GRT ADDRESS
        DW      0               ;GRT SECTOR ADDRESS
        DW      0               ;DIRECT.SYS SECTOR ADDRESS
        DB      1111B
        DB      4
        DW      0
        DW      0
        DW      0
        DB      1111B
        DB      4
        DW      0
        DW      0
        DW      0
        DB      1111B
        DB      4
        DW      0
        DW      0
        DW      0
        DB      1111B
        DB      4
        DW      0
        DW      0
        DW      0
        DB      1111B
        DB      4
        DW      0
        DW      0
        DW      0
UNITBL1 DB      10100B
        DB      0
        DW      0
        DW      0
        DW      0

;       DUMMY CHANNEL TABLE
;
;       THIS CHANNEL TABLE IS FILLED IN DURING THE
;       PROCESS OF OPENING A FILE, THEN THE INFORMATION
;       IS COPIED TO THE REAL CHANNEL TABLE

CHNTBL                          ;CHANNEL TABLES
IOCxLNK DW      CHNTBL          ;USE THIS TABLE FOR ALL CHANNELS
IOCxDDA DW      DVD
IOCxFLG DB      1111B           ;FILE TYPE FLAG
FTxDD   EQU     1               ;DIRECTORY DEVICE
FTxOR   EQU     10B             ;OPEN FOR READ
FTxOW   EQU     100B            ;OPEN FOR WRITE
FTxOU   EQU     1000B           ;OPEN FOR UPDATE
FTxOC   EQU     10000B          ;OPEN FOR CHAR MODE
        DW      0               ;GRT ADDRESS
        DB      4               ;SECTORS/GROUP
        DB      0               ;CURRENT GROUP
        DB      0               ;CURRENT SECTOR
        DB      0               ;LAST GROUP NO.
        DB      0               ;LAST SECTOR INDEX
IOCxDTA DW      DEVTBL          ;DEVICE TABLE ADDRESS
        DW      0               ;SECTOR FOR DIRECTORY
IOCxDEV DW      0               ;DEVICE CODE
IOCxUNI DB      0               ;UNIT NUMBER
DIRxNAM DS      8               ;FILE NAME
DIRxEXT DS      3               ;EXTENSION
        DB      0               ;PROJECT
        DB      20H             ;VERSION
        DB      0               ;CLUSTER FACTOR
DIRxFLG DB      0               ;FLAGS
        DB      0               ;RESERVED
        DB      0               ;FIRST GROUP
        DB      0               ;LAST GROUP
        DB      0               ;LAST SECTOR INDEX
        DW      1979H           ;CREATION DATE
        DW      1979H           ;ALTERATION DATE
IMOVLEN EQU     $-IOCxDDA       ;LENGTH OF CODE TO MOVE TO AIO
FLGOFF  EQU     DIRxFLG-DIRxNAM

DATE    DB      '19-Jan-83'
        DW      1A33H           ;CODED DATE
DATLEN  EQU     $-DATE

;       REAL CHANNEL TABLE

IOCM1   DW      IOC0
        DS      40
IOC0    DW      IOC1
        DS      40
IOC1    DW      IOC2
        DS      40
IOC2    DW      IOC3
        DS      40
IOC3    DW      IOC4
        DS      40
IOC4    DW      IOC5
        DS      40
IOC5    DW      0
        DS      40
LEND    DB      0

        END     START
