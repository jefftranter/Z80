all: cpm22.sys 2k_rom_8.bin cpm-loader3.bin format.bin memory_test.bin putsys3.bin z80-cbios3.bin pcget.com pcput.com chr_echo.bin monitorz80.bin

cpm22.sys: cpm22.asm defines.asm
	z80asm -o cpm22.sys -lcpm22.lst -Lcpm22.map cpm22.asm

2k_rom_8.bin: 2k_rom_8.asm
	z80asm -o 2k_rom_8.bin -l2k_rom_8.lst -L2k_rom_8.map 2k_rom_8.asm

cpm-loader3.bin: cpm-loader3.asm defines.asm
	z80asm -o cpm-loader3.bin -lcpm-loader3.lst -Lcpm-loader3.map cpm-loader3.asm

format.bin: format.asm defines.asm
	z80asm -o format.bin -lformat.lst -Lformat.map format.asm

memory_test.bin: memory_test.asm
	z80asm -o memory_test.bin -lmemory_test.lst -Lmemory_test.map memory_test.asm

pcget.com: pcget.asm
	asl -L pcget.asm
	p2bin -r '$$-$$' pcget.p
	mv pcget.bin pcget.com

pcput.com: pcput.asm
	asl -L pcput.asm
	p2bin -r '$$-$$' pcput.p
	mv pcput.bin pcput.com

putsys3.bin: putsys3.asm defines.asm
	z80asm -o putsys3.bin -lputsys3.lst -Lputsys3.map putsys3.asm

z80-cbios3.bin: z80-cbios3.asm defines.asm
	z80asm -o z80-cbios3.bin -lz80-cbios3.lst -Lz80-cbios3.map z80-cbios3.asm

chr_echo.bin: chr_echo.asm
	asl -L chr_echo.asm
	p2bin -r '$$-$$' chr_echo.p

monitorz80.bin: monitorz80.asm
	asl -L monitorz80.asm
	p2bin -r '$$-$$' monitorz80.p

diff:	all
	for f in orig/* ; do echo $$f ; diff $$f . ; done

clean:
	$(RM) *.sys *.bin *.lst *.map *.p *.com

distclean: clean
