cpm: world.com

hdos: world.abs

world.com: world.c monitor.c
	zcc +z80 -clib=classic -pragma-define:CRT_ORG_CODE=0x0100 world.c monitor.c -create-app -m
	mv a.rom world.com	

world.abs: world.c monitor.c
	zcc +z80 -clib=classic -pragma-define:CRT_ORG_CODE=0x2280 world.c monitor.c -create-app -m
	$(HOME)/git/mine/Z80/HDOS/bin2abs.py a.rom world.abs

disasm: a.rom
	# For CP/M
	z88dk-dis -x a.map -o 0x0100 a.rom
	# For HDOS
	#z88dk-dis -x a.map -o 0x2280 a.rom

clean:
	$(RM) *.map *.bin *.rom *.com *.abs
