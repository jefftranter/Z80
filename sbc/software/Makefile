all:	int32k.bin bas32k.bin rom.bin sysmon.bin hexmon.bin ml.bin jmon.bin

rom.bin: int32k.bin bas32k.bin
	cat int32k.bin bas32k.bin >rom.bin

int32k.bin: int32k.asm
	z80asm -o int32k.bin -lint32k.lst -Lint32k.map int32k.asm

bas32k.bin: bas32k.asm
	z80asm -o bas32k.bin -lbas32k.lst -Lbas32k.map bas32k.asm

sysmon.bin: sysmon.asm
	z80asm -o sysmon.bin -lsysmon.lst -Lsysmon.map sysmon.asm

hexmon.bin: hexmon.asm
	z80asm -o hexmon.bin -lhexmon.lst -Lhexmon.map hexmon.asm

ml.bin: ml.asm
	z80asm -o ml.bin -lml.lst -Lml.map ml.asm

jmon.bin: jmon.asm disasm.asm
	z80asm -o jmon.bin -ljmon.lst -Ljmon.map jmon.asm

clean:
	$(RM) int32k.bin bas32k.bin sysmon.bin rom.bin hexmon.bin ml.bin jmon.bin
	$(RM) int32k.lst bas32k.lst sysmon.lst hexmon.lst ml.lst jmon.lst
	$(RM) int32k.map bas32k.map sysmon.map hexmon.map ml.map jmon.map
	$(RM) int32k.map bas32k.map sysmon.map hexmon.map ml.map jmon.map
