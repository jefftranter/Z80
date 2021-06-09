all:	int32k.bin bas32k.bin

int32k.bin: int32k.asm
	z80asm -l -o int32k.bin int32k.asm 2>int32k.lst

bas32k.bin: bas32k.asm
	z80asm -l -o bas32k.bin bas32k.asm 2>bas32k.lst

clean:
	$(RM) int32k.lst int32k.bin bas32k.lst bas32k.bin
