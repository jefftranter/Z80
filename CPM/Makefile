all: hello.com example1.com example2.com jmon.com cls.com tinybas.com go.com

hello.com: hello.asm
	asl -L hello.asm
	p2bin -r '$$-$$' hello.p
	mv hello.bin hello.com

cls.com: cls.asm
	asl -L cls.asm
	p2bin -r '$$-$$' cls.p
	mv cls.bin cls.com

example1.com: example1.asm
	asl -L example1.asm
	p2bin -r '$$-$$' example1.p
	mv example1.bin example1.com

example2.com: example2.asm
	asl -L example2.asm
	p2bin -r '$$-$$' example2.p
	mv example2.bin example2.com

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

clean:
	$(RM) *.sys *.bin *.lst *.map *.p *.com

distclean: clean
	$(RM) cpm22.z80 cpm2-asm.zip
