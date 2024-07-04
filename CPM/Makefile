all: hello.com example1.com example2.com jmon.com cls.com cls-vt52.com cls-heath.com tinybas.com go.com list.com echo.com glob.com test.com copy.com

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
	z80asm -o jmon.bin -ljmon.lst -Ljmon.map jmon.asm
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

clean:
	$(RM) *.sys *.bin *.lst *.map *.p *.com *.abs

distclean: clean
	$(RM) cpm22.z80 cpm2-asm.zip
