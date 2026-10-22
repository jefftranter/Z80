;-----------------------------------------------------------
; FNCLOCK - Set date and time from FujiNet on Heathkit H89.
;
; Jeff Tranter <tranter@pobox.com>
;-----------------------------------------------------------

; Define exactly one of HDOS or CPM in the assembler command line
; or uncomment one definition below.
;HDOS    EQU     1
;CPM     EQU     1

        INCLUDE "os.inc"
        INCLUDE "fujinet.inc"

IF HDOS
S_DATE  EQU     20BFH
S_DATC  EQU     20C8H
S_TIME  EQU     20CAH
ENDIF

;-----------------------------------------------------------
; Main program
;-----------------------------------------------------------

main:
        call    card_on

        ; Request date/time from FujiNet

        ld      hl,rq_time
        call    fn_call
        jr      nz,failed

        ; Validate returned fields

        call    validate
        jr      nz,baddate

        ; Convert ASCII date

IF HDOS
        call    setascii

        ; Convert packed date

        call    setdate

        ; Convert BCD time

        call    settime
ENDIF
IF CPM
        call    cpmset
ENDIF

        ; Display date and time

        call    crlf
        ld      hl,msg_ok
        call    print

IF HDOS
        ; Copy date from HDOS memory to a
        ; null-terminated buffer.

        ld      hl,S_DATE
        ld      de,datebuf
        ld      bc,9
        ldir

        xor     a
        ld      (datebuf+9),a

ENDIF
IF CPM
        call    cpmdate
ENDIF
        ld      hl,datebuf
        call    print

        ; Display separating space

        ld      hl,msg_space
        call    print

        ; Display time from HDOS memory

        call    showtime

        call    crlf

        ret

failed:
        call    crlf
        ld      hl,msg_err
        call    print
        ret

baddate:
        call    crlf
        ld      hl,msg_bad
        call    print
        ret

;-----------------------------------------------------------
; VALIDATE
;
; Check basic ranges.
;
; Returns Z if valid, NZ otherwise.
;-----------------------------------------------------------

validate:
        ; Day 1..31

        ld      a,(tbuf)
        or      a
        jr      z,invalid
        cp      32
        jr      nc,invalid

        ; Month 1..12

        ld      a,(tbuf+1)
        or      a
        jr      z,invalid
        cp      13
        jr      nc,invalid

        ; Year 2000..2099

        ld      a,(tbuf+2)
        cp      100
        jr      nc,invalid

        ; Hour 0..23

        ld      a,(tbuf+3)
        cp      24
        jr      nc,invalid

        ; Minute 0..59

        ld      a,(tbuf+4)
        cp      60
        jr      nc,invalid

        ; Second 0..59

        ld      a,(tbuf+5)
        cp      60
        jr      nc,invalid

        xor     a
        ret

invalid:
        ld      a,1
        or      a
        ret

;-----------------------------------------------------------
; SETASCII
;
; Build DD-MMM-YY in S.DATE.
;
; Example: 19-OCT-26
;
; Nine characters, no terminator.
;-----------------------------------------------------------

IF HDOS
; Build DD-MMM-YY in HDOS S.DATE.
setascii:
        ; Day, two ASCII digits

        ld      a,(tbuf)
        call    twoascii

        ld      a,d
        ld      (S_DATE),a

        ld      a,e
        ld      (S_DATE+1),a

        ld      a,'-'
        ld      (S_DATE+2),a

        ; Month abbreviation
        ; Calculate (month - 1) * 3.

        ld      a,(tbuf+1)
        dec     a

        ld      e,a
        ld      d,0

        ld      c,e
        ld      b,0

        sla     e
        rl      d

        ld      hl,months
        add     hl,de
        add     hl,bc

        ; Copy three-character month name

        ld      de,S_DATE+3
        ld      bc,3
        ldir

        ; Separator

        ld      a,'-'
        ld      (S_DATE+6),a

        ; Year, two ASCII digits

        ld      a,(tbuf+2)
        call    twoascii

        ld      a,d
        ld      (S_DATE+7),a

        ld      a,e
        ld      (S_DATE+8),a

        ret

; HDOS packed date: year << 9 | month << 5 | day.
setdate:
        ; Year

        ld      a,(tbuf+2)
;       add     a,30            ; Uncomment this line for HDOS without Y2K patches (stores year after 1970)

        ; Year shifted left nine bits

        ld      h,a
        ld      l,0
        add     hl,hl

        ; Month shifted left five bits

        ld      a,(tbuf+1)
        ld      e,a
        ld      d,0

        sla     e
        rl      d
        sla     e
        rl      d
        sla     e
        rl      d
        sla     e
        rl      d
        sla     e
        rl      d

        add     hl,de

        ; Add day

        ld      a,(tbuf)
        ld      e,a
        ld      d,0
        add     hl,de

        ; Store packed date, low byte first

        ld      (S_DATC),hl

        ret

;-----------------------------------------------------------
; SETTIME
;
; Convert binary hour/minute/second into packed BCD.
;-----------------------------------------------------------

settime:
        ld      a,(tbuf+3)
        call    binbcd
        ld      (S_TIME),a

        ld      a,(tbuf+4)
        call    binbcd
        ld      (S_TIME+1),a

        ld      a,(tbuf+5)
        call    binbcd
        ld      (S_TIME+2),a

        ret

;-----------------------------------------------------------
; BINBCD
;
; Convert binary 0..99 to packed BCD.
;
; Input:
;   A = binary number
;
; Output:
;   A = packed BCD
;
; Uses B,C.
;-----------------------------------------------------------

binbcd:
        ld      b,0

bb_loop:
        cp      10
        jr      c,bb_done

        sub     10
        inc     b
        jr      bb_loop

bb_done:
        ld      c,a
        ld      a,b

        add     a,a
        add     a,a
        add     a,a
        add     a,a

        or      c
        ret
ENDIF

IF CPM
; CP/M 2.2.04 clock structure: *(word *)0009H - 11.
; Offsets: 0 seconds, 1 minutes, 2 hours,
;          3 day, 4 month, 5 year (binary).
; Clock ISR may update time asynchronously: disable interrupts
; during both writing and reading the six-byte structure.
clockptr:
        ld      hl,(0009H)
        ld      de,11
        or      a
        sbc     hl,de
        ret

cpmset:
        call    clockptr
        di
        ld      a,(tbuf+5)
        ld      (hl),a
        inc     hl
        ld      a,(tbuf+4)
        ld      (hl),a
        inc     hl
        ld      a,(tbuf+3)
        ld      (hl),a
        inc     hl
        ld      a,(tbuf)
        ld      (hl),a
        inc     hl
        ld      a,(tbuf+1)
        ld      (hl),a
        inc     hl
        ld      a,(tbuf+2)
        ld      (hl),a
        ei
        ret

; Read date back from CP/M and format DD-MMM-YY.
cpmdate:
        call    clockptr
        di
        ld      de,cpmsnap
        ld      bc,6
        ldir
        ei
        ld      a,(cpmsnap+3)
        call    twoascii
        ld      a,d
        ld      (datebuf),a
        ld      a,e
        ld      (datebuf+1),a
        ld      a,'-'
        ld      (datebuf+2),a
        ld      a,(cpmsnap+4)
        dec     a
        ld      e,a
        ld      d,0
        ld      c,e
        ld      b,0
        sla     e
        rl      d
        ld      hl,months
        add     hl,de
        add     hl,bc
        ld      de,datebuf+3
        ld      bc,3
        ldir
        ld      a,'-'
        ld      (datebuf+6),a
        ld      a,(cpmsnap+5)
        call    twoascii
        ld      a,d
        ld      (datebuf+7),a
        ld      a,e
        ld      (datebuf+8),a
        xor     a
        ld      (datebuf+9),a
        ret
ENDIF

; Convert binary 0..99 to two ASCII digits (D,E).
twoascii:
        ld      d,'0'
ta_loop:
        cp      10
        jr      c,ta_done
        sub     10
        inc     d
        jr      ta_loop
ta_done:
        add     a,'0'
        ld      e,a
        ret

;-----------------------------------------------------------
; SHOWTIME
;
; Display HH:MM:SS from HDOS S.TIME.
;
; Reads three packed BCD bytes from HDOS memory.
; Builds an ASCII string and calls PRINT.
;
;-----------------------------------------------------------

showtime:
IF HDOS
        ld      hl,S_TIME
ENDIF
IF CPM
        ld      hl,cpmsnap+2
ENDIF
        ld      de,timebuf

        ; Hours

        ld      a,(hl)
IF HDOS
        call    bcdascii
ENDIF
IF CPM
        call    binaryascii
ENDIF
        call    store2

        ld      a,':'
        ld      (de),a
        inc     de

        ; Minutes

IF HDOS
        inc     hl
ENDIF
IF CPM
        dec     hl
ENDIF
        ld      a,(hl)
IF HDOS
        call    bcdascii
ENDIF
IF CPM
        call    binaryascii
ENDIF
        call    store2

        ld      a,':'
        ld      (de),a
        inc     de

        ; Seconds

IF HDOS
        inc     hl
ENDIF
IF CPM
        dec     hl
ENDIF
        ld      a,(hl)
IF HDOS
        call    bcdascii
ENDIF
IF CPM
        call    binaryascii
ENDIF
        call    store2

        ; Terminate string

        xor     a
        ld      (de),a

        ; Display

        ld      hl,timebuf
        call    print

        ret

;-----------------------------------------------------------
; BCDASCII
;
; Convert packed BCD byte to two ASCII digits.
;
; Input:
;   A = packed BCD
;
; Output:
;   C = ASCII tens
;   B = ASCII units
;-----------------------------------------------------------

IF CPM
; Convert binary byte to ASCII digits C,B.
; Preserve HL (source pointer) and DE (destination pointer).
binaryascii:
        push    hl
        push    de
        call    twoascii
        ld      c,d
        ld      b,e
        pop     de
        pop     hl
        ret
ENDIF
IF HDOS
bcdascii:
        push    af

        ; Tens digit

        and     0F0H
        rrca
        rrca
        rrca
        rrca

        add     a,'0'
        ld      c,a

        ; Units digit

        pop     af
        and     0FH

        add     a,'0'
        ld      b,a

        ret
ENDIF

;-----------------------------------------------------------
; STORE2
;
; Store ASCII digits C and B at DE.
;
; DE advances by two.
;-----------------------------------------------------------

store2:
        ld      a,c
        ld      (de),a
        inc     de

        ld      a,b
        ld      (de),a
        inc     de

        ret

;-----------------------------------------------------------
; FujiNet request
;-----------------------------------------------------------

rq_time:
        FNREQ   DEV_CLOCK, APETIME_GETTIME, F_A1, 0,0,0,0
        FNBUF   0, 0, tbuf, 6

;-----------------------------------------------------------
; Data
;-----------------------------------------------------------

tbuf:
        defs    6

datebuf:
        defs    10

timebuf:
        defs    9

IF CPM
cpmsnap: defs    6
ENDIF
months:
        defm    "JANFEBMARAPRMAYJUN"
        defm    "JULAUGSEPOCTNOVDEC"

msg_ok:
IF HDOS
        defm    "HDOS date and time set to ",0
ENDIF
IF CPM
        defm    "CP/M date and time set to ",0

ENDIF
msg_space:
        defm    " ",0

msg_err:
        defm    "FujiNet clock error",0

msg_bad:
        defm    "Invalid date/time",0

;-----------------------------------------------------------
; FujiNet example support routines
;-----------------------------------------------------------

        INCLUDE "mark.inc"
        INCLUDE "card.inc"
        INCLUDE "fujibus.inc"
        INCLUDE "con.inc"
        INCLUDE "tail.inc"
