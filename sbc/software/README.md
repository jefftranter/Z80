Firmware and software for Z80 Single Board Computer

bas32k.asm - Grant Searle's port of Microsoft BASIC 4.7.

hexmon.asm - Intel hex paper tape and magnetic tape routines from the book "8080/Z80 Assembly Language - Techniques for Improved Programming" by Alan R. Miller (untested).

int32k.asm - Grant Searle's i/o routines and startup program to boot into BASIC.

jmon.asm   - Port of my JMON monitor to Z80 (ROM image).

ml.bas, ml.asm - Example of calling machine language from Basic.

patb.asm   - Port of Palo Alto Tiny BASIC Version Three, by Li-Chen Wang.

sysmon.asm - System Monitor from the book "8080/Z80 Assembly Language - Techniques for Improved Programming" by Alan R. Miller.

The code builds with the Z80asm cross-assembler. The files rom.bin, jmon.bin, or sysmon.bin can be programmed into an EPROM or EEPROM.

See:

1. http://searle.x10host.com/z80/SimpleZ80.html
2. http://www.nascomhomepage.com/pdf/Basic.pdf

Notes from Grant Searle's web page on BASIC:

ROM BASIC (Microsoft BASIC 4.7) - details of what has been included/excluded

1. The hex and binary identifiers have changed to &Hnnnn and &Bnnnn to match Microsoft implementations for other processors.
2. Width setting changed to 255 default, so no auto CR/LF are added when printing long strings.
3. HEX$(nn) and BIN$(nn) now have leading zeroes suppressed, as for other numeric output.
4. CR/LF processing changed slightly internally so that an LF is no longer auto-generated when sending a CR to the terminal.

Included tokens:

SGN, INT, ABS, USR, FRE, INP, POS, SQR, RND, LOG, EXP, COS, SIN, TAN, ATN, PEEK, DEEK, LEN, STR$, VAL, ASC, CHR$, LEFT$, RIGHT$, MID$

END, FOR, NEXT, DATA, INPUT, DIM, READ, LET, GOTO, RUN, IF, RESTORE, GOSUB, RETURN, REM, STOP, OUT, ON, NULL, WAIT, 

DEF, POKE, DOKE, LINES, CLS, WIDTH, MONITOR, PRINT, CONT, LIST, CLEAR, NEW

TAB, TO, FN, SPC, THEN, NOT, STEP

+, -, *, /, ^, AND, OR, >, <, =

Note: there is also SET, RESET, POINT that call user-defined entry
points, as in the ORIGINAL Nascom ROM (i.e. unchanged). Don't use
unless you have defined the calling points for them (see the assembly
listing for details).

Excluded tokens (don't do anything if called):

SCREEN, CLOAD, CSAVE

New Tokens (my additional implementations):

HEX$(nn) - Convert a SIGNED integer (-32768 to +32767) to a string containing the hex value.
BIN$(nn) - Convert a SIGNED integer (-32768 to +32767) to a string containing the binary value.
&Hnn - Interpret the value after the &H as a HEX value (signed 16 bit).
&Bnn - Interpret the value after the &B as a BINARY value (signed 16 bit).

IMPORTANT NOTES

Integers in this version of BASIC are SIGNED, i.e. -32768 to +32767.
This includes memory locations when peek, poke, deek,doke commands are
issued etc. So, to refer to location "n" above 32767, you must provide
the 2's complement number instead (i.e. "n-65536") otherwise you will
get an "?FC Error" message.

Functions that return integer values, such as the memory FRE(0), are
also signed, so anything larger than 32767 will appear as a negative
value.
