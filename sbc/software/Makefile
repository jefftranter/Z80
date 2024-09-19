PRAGMAS = -pragma-define:CRT_MODEL=1 -pragma-define:CRT_ORG_CODE=0x8000 -pragma-define:CRT_ON_EXIT=0x0000 -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1

all:	int32k.bin bas32k.bin rom.bin sysmon.bin ml.bin jmon.bin patb.bin patb-z80.bin jmon-patb.bin hello.hex advent1.hex advent2.hex advent3.hex

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

jmon.bin: jmon.asm disasm.asm hexmon2.asm
	z80asm -o part1.bin -ljmon.lst -Ljmon.map jmon.asm
	z80asm -o part2.bin -lhexmon2.lst -Lhexmon2.map hexmon2.asm
	cat part1.bin part2.bin >jmon.bin
	$(RM) part1.bin part2.bin

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

hello.hex: hello.c hardware.asm
	. ../../z88dk/env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) hello.c hardware.asm -create-app -m -Cz--ihex -lm
	mv a.rom hello.bin
	mv a.map hello.map
	mv a.ihx hello.hex
	rm a.bin a_DATA.bin
	sed -i 's/^:00000001FF$$/:008000017F/' hello.hex

advent1.hex: ../../CPM/advent1.c hardware.asm
	. ../../z88dk/env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../../CPM/advent1.c hardware.asm -create-app -m -Cz--ihex
	mv a.rom advent1.bin
	mv a.map advent1.map
	mv a.ihx advent1.hex
	rm a.bin a_DATA.bin
	sed -i 's/^:00000001FF$$/:008000017F/' advent1.hex

advent2.hex: ../../CPM/advent2.c hardware.asm
	. ../../z88dk/env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../../CPM/advent2.c hardware.asm -create-app -m -Cz--ihex
	mv a.rom advent2.bin
	mv a.map advent2.map
	mv a.ihx advent2.hex
	rm a.bin a_DATA.bin
	sed -i 's/^:00000001FF$$/:008000017F/' advent2.hex

advent3.hex: ../../CPM/advent3.c hardware.asm
	. ../../z88dk/env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../../CPM/advent3.c hardware.asm -create-app -m -Cz--ihex
	mv a.rom advent3.bin
	mv a.map advent3.map
	mv a.ihx advent3.hex
	rm a.bin a_DATA.bin
	sed -i 's/^:00000001FF$$/:008000017F/' advent3.hex

clean:
	$(RM) *.bin *.lst *.map *.hex *.p
