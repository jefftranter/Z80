all: asm c

asm: hello.com example1.com example2.com jmon.com cls.com cls-vt52.com cls-heath.com tinybas.com go.com list.com echo.com glob.com test.com copy.com

c: advent1.com demo.com disasm.com freq.com chello.com info.com nqueens.com rpncalc.com search.com sieve.com size.com yum.com

hello.com: hello.asm
	asl -L hello.asm
	p2bin -r '$$-$$' hello.p
	mv hello.bin hello.com

cls.com: cls.asm
	asl -L cls.asm
	p2bin -r '$$-$$' cls.p
	mv cls.bin cls.com

cls-vt52.com: cls-vt52.asm
	asl -L cls-vt52.asm
	p2bin -r '$$-$$' cls-vt52.p
	mv cls-vt52.bin cls-vt52.com

cls-heath.com: cls-heath.asm
	asl -L cls-heath.asm
	p2bin -r '$$-$$' cls-heath.p
	mv cls-heath.bin cls-heath.com

example1.com: example1.asm
	asl -L example1.asm
	p2bin -r '$$-$$' example1.p
	mv example1.bin example1.com

example2.com: example2.asm
	asl -L example2.asm
	p2bin -r '$$-$$' example2.p
	mv example2.bin example2.com

jmon.abs: jmon.com
	../HDOS/bin2abs.py jmon.com jmon.abs

jmon.com: jmon.asm disasm.asm
	z80asm -b -l jmon.asm
	mv jmon.bin jmon.com

tinybas.com: tinybas.asm
	asl -L tinybas.asm
	p2bin -r '$$-$$' tinybas.p
	mv tinybas.bin tinybas.com

go.com: go.asm
	asl -cpu 8080 -L go.asm
	p2bin -r '$$-$$' go.p
	mv go.bin go.com

list.com: list.asm
	asl -cpu 8080 -L list.asm
	p2bin -r '$$-$$' list.p
	mv list.bin list.com

echo.com: echo.asm
	asl -cpu 8080 -L echo.asm
	p2bin -r '$$-$$' echo.p
	mv echo.bin echo.com

glob.com: glob.asm
	asl -cpu 8080 -L glob.asm
	p2bin -r '$$-$$' glob.p
	mv glob.bin glob.com

test.com:
	asl -cpu 8080 -L test.lst test.asm
	p2bin -r '$$-$$' test.p
	mv test.bin test.com

copy.com:
	asl -cpu 8080 -L copy.lst copy.asm
	p2bin -r '$$-$$' copy.p
	mv copy.bin copy.com

chello.com: chello.c
	zcc -v -mz80 +cpm -subtype=hz89 -lm -o chello.com chello.c

sieve.com: sieve.c
	zcc -v -mz80 +cpm -subtype=hz89 -lm -o sieve.com sieve.c

nqueens.com: nqueens.c
	zcc -v -mz80 +cpm -subtype=hz89 -lm -o nqueens.com nqueens.c

demo.com: demo.c
	zcc -v -mz80 +cpm -subtype=hz89 -lm -o demo.com demo.c

rpncalc.com: rpncalc.c
	zcc -v -mz80 +cpm -subtype=hz89 -lm -o rpncalc.com rpncalc.c

disasm.com: disasm.c
	zcc -v -mz80 +cpm -subtype=hz89 -lm -o disasm.com disasm.c

advent1.com: advent1.c
	zcc -v -mz80 +cpm -subtype=hz89 -lm -o advent1.com advent1.c

freq.com: freq.c
	zcc -v -mz80 +cpm -subtype=hz89 -lm -o freq.com freq.c

info.com: info.c
	zcc -v -mz80 +cpm -subtype=hz89 -lm -o info.com info.c

search.com: search.c
	zcc -v -mz80 +cpm -subtype=hz89 -lm -o search.com search.c

size.com: size.c
	zcc -v -mz80 +cpm -subtype=hz89 -lm -o size.com size.c

yum.com: yum.c
	zcc -v -mz80 +cpm -subtype=hz89 -lm -o yum.com yum.c

clean:
	$(RM) *.sys *.bin *.lst *.lis *.map *.p *.o *.com *.abs

distclean: clean
	$(RM) cpm22.z80 cpm2-asm.zip
