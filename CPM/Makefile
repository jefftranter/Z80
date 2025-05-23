PRAGMAS = -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1
OPTIONS = -mz80 +cpm -subtype=hz89

all: asm c native

asm: hello.com example1.com example2.com jmon.com cls.com cls-vt52.com cls-heath.com tinybas.com go.com list.com echo.com glob.com test.com copy.com

c: advent1.com advent2.com advent3.com demo.com disasm.com freq.com chello.com info.com nqueens.com rpncalc.com search.com sieve.com size.com yum.com cecho.com fileio.com arch.com unarch.com wildcard.com dump.com

native: arch unarch dir dump

arch:	arch.c
	gcc -Wall -o arch arch.c

unarch:	unarch.c
	gcc -Wall -o unarch unarch.c

dir:	dir.c
	gcc -Wall -o dir dir.c

dump:	dump.c
	gcc -Wall -o dump dump.c

hello.com: hello.asm
	asl -q -L hello.asm
	p2bin -q -r '$$-$$' hello.p
	mv hello.bin hello.com

cls.com: cls.asm
	asl -q -L cls.asm
	p2bin -q -r '$$-$$' cls.p
	mv cls.bin cls.com

cls-vt52.com: cls-vt52.asm
	asl -q -L cls-vt52.asm
	p2bin -q -r '$$-$$' cls-vt52.p
	mv cls-vt52.bin cls-vt52.com

cls-heath.com: cls-heath.asm
	asl -q -L cls-heath.asm
	p2bin -q -r '$$-$$' cls-heath.p
	mv cls-heath.bin cls-heath.com

example1.com: example1.asm
	asl -q -L example1.asm
	p2bin -q -r '$$-$$' example1.p
	mv example1.bin example1.com

example2.com: example2.asm
	asl -q -L example2.asm
	p2bin -q -r '$$-$$' example2.p
	mv example2.bin example2.com

jmon.abs: jmon.com
	../HDOS/bin2abs.py jmon.com jmon.abs

jmon.com: jmon.asm disasm.asm
	/usr/bin/z80asm -o jmon.bin -ljmon.lst -Ljmon.map jmon.asm
	mv jmon.bin jmon.com

tinybas.com: tinybas.asm
	asl -q -L tinybas.asm
	p2bin -q -r '$$-$$' tinybas.p
	mv tinybas.bin tinybas.com

go.com: go.asm
	asl -q -cpu 8080 -L go.asm
	p2bin -q -r '$$-$$' go.p
	mv go.bin go.com

list.com: list.asm
	asl -q -cpu 8080 -L list.asm
	p2bin -q -r '$$-$$' list.p
	mv list.bin list.com

echo.com: echo.asm
	asl -q -cpu 8080 -L echo.asm
	p2bin -q -r '$$-$$' echo.p
	mv echo.bin echo.com

glob.com: glob.asm
	asl -q -cpu 8080 -L glob.asm
	p2bin -q -r '$$-$$' glob.p
	mv glob.bin glob.com

test.com:
	asl -q -cpu 8080 -L test.lst test.asm
	p2bin -q -r '$$-$$' test.p
	mv test.bin test.com

copy.com:
	asl -q -cpu 8080 -L copy.lst copy.asm
	p2bin -q -r '$$-$$' copy.p
	mv copy.bin copy.com

chello.com: chello.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o chello.com chello.c

sieve.com: sieve.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o sieve.com sieve.c

nqueens.com: nqueens.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o nqueens.com nqueens.c

demo.com: demo.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o demo.com demo.c

rpncalc.com: rpncalc.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o rpncalc.com rpncalc.c

disasm.com: disasm.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o disasm.com disasm.c

advent1.com: advent1.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o advent1.com advent1.c

advent2.com: advent2.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o advent2.com advent2.c

advent3.com: advent3.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o advent3.com advent3.c

freq.com: freq.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o freq.com freq.c

info.com: info.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o info.com info.c

search.com: search.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o search.com search.c

size.com: size.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o size.com size.c

yum.com: yum.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o yum.com yum.c

cecho.com: cecho.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o cecho.com cecho.c

fileio.com: fileio.c
	. ../z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -lm -o fileio.com fileio.c

arch.com: arch.c
	. $(HOME)/git/mine/Z80/z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -o arch.com arch.c

unarch.com: unarch.c
	. $(HOME)/git/mine/Z80/z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -o unarch.com unarch.c

wildcard.com: wildcard.c
	. $(HOME)/git/mine/Z80/z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -o wildcard.com wildcard.c

dump.com: dump.c
	. $(HOME)/git/mine/Z80/z88dk/env.sh && zcc $(OPTIONS) $(PRAGMAS) -o dump.com dump.c

clean:
	$(RM) *.sys *.bin *.lst *.lis *.map *.p *.o *.com *.abs arch unarch dir dump

distclean: clean
	$(RM) cpm22.z80 cpm2-asm.zip
