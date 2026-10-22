CFLAGS=-mz80 +hdos -startup=3 -create-app -Cz--abs -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1

INC=$(HOME)/git/fujinet-manuals/h89/programmers-handbook/src/asm/inc

all: xdemo.abs xcls.abs fnclock.abs fnclock.com c_progs

c_progs: fileio.abs advent1.abs advent2.abs advent3.abs advent4.abs dir.abs wildcard.abs cecho.abs hdemo.abs

xdemo.abs: xdemo.bin
	./bin2abs.py xdemo.bin xdemo.abs

xdemo.hex: xdemo.p
	p2hex -F Intel xdemo.p

xdemo.bin: xdemo.p
	p2bin -r '$$-$$' xdemo.p

xdemo.p: xdemo.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . xdemo.asm

xcls.abs: xcls.bin
	./bin2abs.py xcls.bin xcls.abs

xcls.hex: xcls.p
	p2hex -F Intel xcls.p

xcls.bin: xcls.p
	p2bin -r '$$-$$' xcls.p

xcls.p: xcls.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . xcls.asm

cecho.abs: ../CPM/cecho.c
	. ../z88dk/env.sh && zcc $(CFLAGS) -o cecho.bin ../CPM/cecho.c

fileio.abs: ../CPM/fileio.c
	. ../z88dk/env.sh && zcc $(CFLAGS) -o fileio.bin ../CPM/fileio.c

wildcard.abs: wildcard.c
	. ../z88dk/env.sh && zcc $(CFLAGS) -o wildcard.bin wildcard.c

advent1.abs: ../CPM/advent1.c
	. ../z88dk/env.sh && zcc $(CFLAGS) -o advent1.bin ../CPM/advent1.c

advent2.abs: ../CPM/advent2.c
	. ../z88dk/env.sh && zcc $(CFLAGS) -o advent2.bin ../CPM/advent2.c

advent3.abs: ../CPM/advent3.c
	. ../z88dk/env.sh && zcc $(CFLAGS) -o advent3.bin ../CPM/advent3.c

advent4.abs: ../CPM/advent4.c
	. ../z88dk/env.sh && zcc $(CFLAGS) -o advent4.bin ../CPM/advent4.c

dir.abs: dir.c
	. ../z88dk/env.sh && zcc $(CFLAGS) -o dir.bin dir.c

fnclock.abs: fnclock.asm
	.  $(HOME)/git/mine/Z80/z88dk/env.sh &&	z88dk-z80asm -l -m -b -DHDOS -I=$(INC) fnclock.asm
	mv fnclock.bin fnclock.abs

fnclock.com: fnclock.asm
	.  $(HOME)/git/mine/Z80/z88dk/env.sh &&	z88dk-z80asm -l -m -b -DCPM -I=$(INC) fnclock.asm
	mv fnclock.bin fnclock.com

hdemo.abs: hdemo.c
	. ../z88dk/env.sh && zcc $(CFLAGS) -o hdemo.bin hdemo.c

clean:
	$(RM) *.p *.lst *.map *.lis *.o *.p *.bin *.hex *.abs fnclock.com
