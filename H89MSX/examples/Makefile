all: scale.com bars.com joy.com psg.com vdp.com abars.com abars.abs

scale.com: scale.c
	. ../../z88dk/env.sh && zcc -mz80 +cpm -subtype=hz89 -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o scale.com scale.c 

bars.com: bars.c
	. ../../z88dk/env.sh && zcc -mz80 +cpm -subtype=hz89 -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o bars.com bars.c 

joy.com: joy.c
	. ../../z88dk/env.sh && zcc -mz80 +cpm -subtype=hz89 -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o joy.com joy.c 


psg.com: psg.c
	. ../../z88dk/env.sh && zcc -mz80 +cpm -subtype=hz89 -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o psg.com psg.c 

vdp.com: vdp.c
	. ../../z88dk/env.sh && zcc -mz80 +cpm -subtype=hz89 -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o vdp.com vdp.c

abars.abs: abars.asm
	asl -q -L -D HDOS -cpu 8080 abars.asm
	p2bin -r '$$-$$' abars.p
	$(HOME)/git/mine/Z80/HDOS/bin2abs.py abars.bin abars.abs
	$(RM) abars.p abars.lst abars.bin

abars.com: abars.asm
	asl -q -L -D CPM -cpu 8080 abars.asm
	p2bin -r '$$-$$' abars.p
	mv abars.bin abars.com
	$(RM) abars.p abars.lst abars.bin

clean:
	$(RM) scale.com bars.com abars.com joy.com psg.com vdp.com abars.abs

