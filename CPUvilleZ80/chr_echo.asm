;Program to test serial port.
;Enter hex machine code with load command, or binary file with bload command.
;No port initialization commands
;When running, should echo typed characters to display.

        cpu    z80
        org    0800h
echo_loop_1:
        in      a,(3)           ;get status
        and     002h            ;check RxRDY bit
        jr      z,echo_loop_1   ;not ready, loop
        in      a,(2)           ;get char
        ld      b,a             ;save received char in b reg
echo_loop_2:
        in      a,(3)           ;get status
        and     001h            ;check TxRDY bit
        jr      z,echo_loop_2   ;loop if not set
        ld      a,b             ;get char back
        out     (2),a           ;send to output
        jr      echo_loop_1     ;start over
        end
