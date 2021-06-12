all:	int32k.bin bas32k.bin rom.bin sysmon.bin

rom.bin: int32k.bin bas32k.bin
	cat int32k.bin bas32k.bin >rom.bin

int32k.bin: int32k.asm
	z80asm -o int32k.bin -lint32k.lst int32k.asm

bas32k.bin: bas32k.asm
	z80asm -o bas32k.bin -lbas32k.lst bas32k.asm

sysmon.bin: sysmon.asm
	z80asm -o sysmon.bin -lsysmon.lst sysmon.asm

clean:
	$(RM) int32k.bin bas32k.bin sysmon.bin rom.bin
	$(RM) int32k.lst bas32k.lst sysmon.lst
