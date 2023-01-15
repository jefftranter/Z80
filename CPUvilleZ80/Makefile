all: cpm22.sys 2k_rom_8.bin cpm-loader3.bin format.bin memory_test.bin putsys3.bin z80-cbios3.bin pcget.com pcput.com tinybasic2dms.bin hello.com example1.com example2.com chr_echo.bin jmon.bin

cpm2-asm.zip:
	wget http://www.cpm.z80.de/download/cpm2-asm.zip

cpm22.z80: cpm2-asm.zip
	unzip cpm2-asm.zip CPM22.Z80
	dos2unix CPM22.Z80
	patch <cpm22.patch
	mv CPM22.Z80 cpm22.z80
	$(RM) cpm2-asm.zip

cpm22.sys: cpm22.z80
	z80asm -o cpm22.sys -lcpm22.lst -Lcpm22.map cpm22.z80

2k_rom_8.bin: 2k_rom_8.asm
	z80asm -o 2k_rom_8.bin -l2k_rom_8.lst -L2k_rom_8.map 2k_rom_8.asm

cpm-loader3.bin: cpm-loader3.asm
	z80asm -o cpm-loader3.bin -lcpm-loader3.lst -Lcpm-loader3.map cpm-loader3.asm

format.bin: format.asm
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

putsys3.bin: putsys3.asm
	z80asm -o putsys3.bin -lputsys3.lst -Lputsys3.map putsys3.asm

z80-cbios3.bin: z80-cbios3.asm
	z80asm -o z80-cbios3.bin -lz80-cbios3.lst -Lz80-cbios3.map z80-cbios3.asm

tinybasic2dms.bin: tinybasic2dms.asm
	asl -L tinybasic2dms.asm
	p2bin -r '$$-$$' tinybasic2dms.p

hello.com: hello.asm
	asl -L hello.asm
	p2bin -r '$$-$$' hello.p
	mv hello.bin hello.com

example1.com: example1.asm
	asl -L example1.asm
	p2bin -r '$$-$$' example1.p
	mv example1.bin example1.com

example2.com: example2.asm
	asl -L example2.asm
	p2bin -r '$$-$$' example2.p
	mv example2.bin example2.com

chr_echo.bin: chr_echo.asm
	asl -L chr_echo.asm
	p2bin -r '$$-$$' chr_echo.p

jmon.bin: jmon.asm disasm.asm
	z80asm -o jmon.bin -ljmon.lst -Ljmon.map jmon.asm

clean:
	$(RM) *.sys *.bin *.lst *.map *.p *.com

distclean: clean
	$(RM) cpm22.z80 cpm2-asm.zip
