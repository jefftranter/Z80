;-----------------------------------------------------------
; FNDATE - Set HDOS 3 date and time from FujiNet
;
; Heathkit H89 / Z80 / HDOS 3
;
; Based on FujiNet exclock.asm.
;
; Uses FujiNet APETIME_GETTIME.
;
; FujiNet returns six binary bytes:
;   0 = day
;   1 = month
;   2 = year since 2000
;   3 = hour
;   4 = minute
;   5 = second
;
; HDOS 3 system locations:
;   20BFH  S.DATE  ASCII date (9 bytes)
;   20C8H  S.DATC  Packed date (2 bytes)
;   20CAH  S.TIME  BCD time (3 bytes)
;
; Displays the date and time read back from HDOS.
;
;-----------------------------------------------------------

        INCLUDE "os.inc"
        INCLUDE "fujinet.inc"

S_DATE  EQU     20BFH
S_DATC  EQU     20C8H
S_TIME  EQU     20CAH

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

        call    setascii

        ; Convert packed date

        call    setdate

        ; Convert BCD time

        call    settime

        ; Display date and time

        call    crlf
        ld      hl,msg_ok
        call    print

        ; Copy date from HDOS memory to a
        ; null-terminated buffer.

        ld      hl,S_DATE
        ld      de,datebuf
        ld      bc,9
        ldir

        xor     a
        ld      (datebuf+9),a

        ld      hl,datebuf
        call    print

        ; Display separating space

        ld      hl,msg_space
        call    print

        ; Display time from HDOS memory

        call    showtime

        call    crlf

        jp      ex_done

failed:
        call    crlf
        ld      hl,msg_err
        call    print
        jp      ex_done

baddate:
        call    crlf
        ld      hl,msg_bad
        call    print
        jp      ex_done

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

;-----------------------------------------------------------
; TWOASCII
;
; Convert binary number 0..99 to two ASCII digits.
;
; Input:
;   A = binary number
;
; Output:
;   D = ASCII tens
;   E = ASCII units
;-----------------------------------------------------------

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
; SETDATE
;
; HDOS packed date:
;
;   bits 15..9 = year since 1970
;   bits  8..5 = month
;   bits  4..0 = day
;
;-----------------------------------------------------------

setdate:
        ; Year since 1970

        ld      a,(tbuf+2)
        add     a,30

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
        ld      hl,S_TIME
        ld      de,timebuf

        ; Hours

        ld      a,(hl)
        call    bcdascii
        call    store2

        ld      a,':'
        ld      (de),a
        inc     de

        ; Minutes

        inc     hl
        ld      a,(hl)
        call    bcdascii
        call    store2

        ld      a,':'
        ld      (de),a
        inc     de

        ; Seconds

        inc     hl
        ld      a,(hl)
        call    bcdascii
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

months:
        defm    "JANFEBMARAPRMAYJUN"
        defm    "JULAUGSEPOCTNOVDEC"

msg_ok:
        defm    "HDOS date and time set to ",0

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
