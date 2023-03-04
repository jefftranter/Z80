all:	int32k.bin bas32k.bin rom.bin sysmon.bin ml.bin jmon.bin patb.bin patb-z80.bin jmon-patb.bin

rom.bin: int32k.bin bas32k.bin
	cat int32k.bin bas32k.bin >rom.bin

int32k.bin: int32k.asm
	z80asm -o int32k.bin -lint32k.lst -Lint32k.map int32k.asm

bas32k.bin: bas32k.asm
	z80asm -o bas32k.bin -lbas32k.lst -Lbas32k.map bas32k.asm

sysmon.bin: sysmon.asm hexmon.asm
	z80asm -o part1.bin -lsysmon.lst -Lsysmon.map sysmon.asm
	z80asm -o part2.bin -lhexmon.lst -Lhexmon.map hexmon.asm
	cat part1.bin part2.bin >sysmon.bin
	$(RM) part1.bin part2.bin

ml.bin: ml.asm
	z80asm -o ml.bin -lml.lst -Lml.map ml.asm

jmon.bin: jmon.asm disasm.asm
	z80asm -o jmon.bin -ljmon.lst -Ljmon.map jmon.asm

patb.bin: patb.asm
	asl -L patb.asm
	p2bin -r '$$-$$' patb.p
	p2hex -r '$$-$$' -F Intel patb.p

patb-z80.bin: patb-z80.asm
	asl -L patb-z80.asm
	p2bin -r '$$-$$' patb-z80.p
	p2hex -r '$$-$$' -F Intel patb-z80.p

jmon-patb.bin: jmon-patb.asm disasm.asm
	asl -L jmon-patb.asm
	p2bin -r '$$-$$' jmon-patb.p
	p2hex -r '$$-$$' -F Intel jmon-patb.p


clean:
	$(RM) *.bin *.lst *.map *.hex *.p
