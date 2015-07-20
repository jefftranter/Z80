all: zx81rom.hex zx81rom.bin

zx81rom.hex: zx81rom.p
	p2hex -F Intel zx81rom.p

zx81rom.bin: zx81rom.p
	p2bin -r '$$-$$' zx81rom.p

zx81rom.p: zx81rom.asm
	asl -L zx81rom.asm

clean:
	$(RM) *.lst *.p *.bin *.hex
