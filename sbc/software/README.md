Firmware and software for Z80 Single Board Computer

bas32k.asm - Grant Searle's port of Microsoft BASIC 4.7.

hexmon.asm - Intel hex paper tape and magnetic tape routines from the book "8080/Z80 Assembly Language - Techniques for Improved Programming" by Alan R. Miller.

int32k.asm - Grant Searle's i/o routines and startup program to boot into BASIC.

jmon.asm - Port of my JMON monitor to Z80 (ROM image).

jmon-patb.asm - Combined ROM with JMON monitor and Palo Alto Tiny basic.

ml.bas, ml.asm - Example of calling machine language from Basic.

patb.asm - Port of Palo Alto Tiny BASIC Version Three, by Li-Chen Wang (ROM image).

patb-z80.asm - Port of Palo Alto Tiny BASIC Version Three, by Li-Chen Wang (ROM image) - version using Z80 mnemonics.

sysmon.asm - System Monitor from the book "8080/Z80 Assembly Language - Techniques for Improved Programming" by Alan R. Miller.

The code builds with the z80asm cross-assembler. The files rom.bin, jmon.bin, jmon.patb.bin or sysmon.bin can be programmed into an EPROM or EEPROM.

See:

1. http://searle.x10host.com/z80/SimpleZ80.html
2. http://www.nascomhomepage.com/pdf/Basic.pdf

------------------------------------------------------------------------

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

------------------------------------------------------------------------

Here is a summary of Tiny Basic, taken from the July 1976 People's
Computer Company newsletter.

TINY BASIC

If you have been a faithful PCC reader, you know that a number of
people have defined "tiny" BASIC languages and implemented
interpreters for them. The latest of these is Li-Chen Wang's "Palo
Alto" tiny BASIC, which is outlined on this page.

The is the first tiny BASIC I've looked at and I expected it to be a
subset of "real" BASIC. I was surprised to find that compared to the
BASIC I have used (HP-2000) there are some extensions as well as
restrictions. For example:

You can print a prompt and input the responses with one instruction.

Compound statements (up to one line long) are permitted -- this
enables you to clean up the structure of your BASIC programs a bit.
Sure would be nice to continue past 1 line.

Logical terms may be used for arithmetic expressions (true=1 and
false=0).

All keywords can be abbreviated.

Some of the restrictions are:

There is no provision for file handling (except for "saving"
programs).

All constants and variable values are integers between -32767 and
32767.

There are only 26 scalar variables, named A thru Z.

There is only a single, unidimensional array, @(I). The dimension of
this array is set automatically to make use of all the memory which is
not used by the program.

There are only 3 functions:

ABS(X) - absolute value of X,
RND(X) - random number between 1 and X,
SIZE - the number of bytes unused by the program.

There are no string variables.

Only 3 "direct commands" are available: RUN, LIST, and NEW (like
SCRATCH) plus good old Control C.

OK, let's look at that the program can say in this tiny BASIC:

REMARK: nothing new here.

LET: like you're used to but remember that expressions may contain
logical terms so

LET V = (A>B)*X + (A<B)*Y

sets v to x, y or 0 depending on whether A is greater than, less than
or equal B.

PRINT: values are normally printed in 6 spaces; however you may
override this using "#n" to change field size, so that--

PRINT A,B,#3,C,D,E,#10,F,G

will print the values of A and B in 6 spaces, the values of C, D, and
E, in 3 spaces, and the values of F and G in 10 spaces. If there are
not enough spaces specified for a given value to be printed, the value
will be printed with enough spaces anyway.

You may also causes overprinting since:

PRINT 'ABC',-'XXX'

will print the string "ABC", a CR without a LF, and then the string
"XXX" (over the ABC) followed by a CR-LF.

INPUT: automatic prompts and input of expressions--

INPUT A,

When this command is executed, Tiny Basic will print "A" and wait to
read in an expression from the input device. The variable A will be
set to the value of this expression. Note that not only numbers, but
also expressions can be read as input.

INPUT "WHAT IS THE WEIGHT" A

This is the same as the command above, except the prompt "A":" is
replaced by "WHAT IS THE WEIGHT".

IF: IF A<B LET X=3 : PRINT'THIS STRING'

will test the value of the expression A<B. If it is true (1) the
commands in the rest of the statement will be executed. IF the value
of the expression is false (0), the rest of this statement will be
skipped. Note that the word "THEN" is not used.

GOTO: You may say GOTO (expression) for example GOTO A*10+B.

GOSUB and RETURN: the usual except that the GOSUB may also use any
expression for its argument.

FOR and NEXT: like you've seen before.

STOP: what do you supposed this does????

Two more things you'll need to know about are the enhanced RND functions
and abbreviations.

RND:

LET R=RND(3)

Assigns a random integer between 1 and 30 to R

LET R = RND(100), A = (R>3) + (R>15) + (R>56) + R(>98)

Assigns R a random integer between 0 and 4 with probability .02 of
being 0, .12 of being 1, 41 of being 2, .42 of being 3 and .02 of
being 4.

ABBREVIATIONS and BLANKS:

You may use blanks freely, except that numbers, command keywords
and function names can not have embedded blanks.

You may truncate all command keywords and function names and follow
them by a period. "P.", "PR.", "PRI.", and "PRIN." all stand for
"PRINT". Also the word LET in LET command can be omitted The
"shortest" abbreviation for all keywords are as follows:

A.=ABS  F.=FOR     GOS.=GOSUB G.=GOTO
IF.=IF  IN.=INPUT  L.=LIST    N.=NEW
N.=NEXT P.=PRINT   REM=REMARK R.=RETURN
R.=RND  R.=RUN     S.=SIZE    S.=STEP
S.=STOP TO=TO

If you would like complete documentation on this language, including a
well-commented source listing of the interpreter, get a copy of the
May 1976 issue of Dr.Dobb's Journal.
