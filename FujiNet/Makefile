INC=$(HOME)/git/fujinet-manuals/h89/programmers-handbook/src/asm/inc

all: fnclock.abs fnclock.com

fnclock.abs: fnclock.asm
	.  $(HOME)/git/mine/Z80/z88dk/env.sh &&	z88dk-z80asm -l -m -b -DHDOS -I=$(INC) fnclock.asm
	mv fnclock.bin fnclock.abs

fnclock.com: fnclock.asm
	.  $(HOME)/git/mine/Z80/z88dk/env.sh &&	z88dk-z80asm -l -m -b -DCPM -I=$(INC) fnclock.asm
	mv fnclock.bin fnclock.com

clean:
	$(RM) *.map *.lis *.o *.abs *.com
