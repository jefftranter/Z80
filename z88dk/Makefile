cpm: world.com

hdos: world.abs

world.com: world.c monitor.c
	zcc +z80 -clib=classic -crt0=crt0.asm -pragma-define:CRT_ORG_CODE=0x0100 world.c monitor.c -create-app -m
	mv a.rom world.com	

world.abs: world.c monitor.c
	zcc +z80 -clib=classic -crt0=crt0.asm -pragma-define:CRT_ORG_CODE=0x2280 world.c monitor.c -create-app -m
	../HDOS/bin2abs.py a.rom world.abs

clean:
	$(RM) *.map *.bin *.rom *.com *.abs
