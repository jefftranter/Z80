cpm: world.com

hdos: world.abs

world.com: world.c monitor.c crt0.asm
	zcc +z80 -clib=classic -crt0=crt0.asm -pragma-define:CRT_ORG_CODE=0x0100 -pragma-define:CRT_ON_EXIT=0x10002 world.c monitor.c -create-app -m
	mv a.rom world.com	

world.abs: world.c monitor.c crt0.asm
	zcc +z80 -clib=classic -crt0=crt0.asm -pragma-define:HDOS=0x1 -pragma-define:CRT_ORG_CODE=0x2280 -pragma-define:CRT_ON_EXIT=0x10002 world.c monitor.c -create-app -m
	../HDOS/bin2abs.py a.rom world.abs

advent1.abs: ../CPM/advent1.c monitor.c crt0.asm
	zcc +z80 -clib=classic -crt0=crt0.asm -pragma-define:CRT_MODEL=0 -pragma-define:HDOS=0x1 -pragma-define:CRT_ORG_CODE=0x2280 -pragma-define:CRT_ON_EXIT=0x10002 ../CPM/advent1.c monitor.c -create-app -m
	../HDOS/bin2abs.py a.rom advent1.abs

cecho.abs: ../CPM/cecho.c monitor.c crt0.asm
	zcc +z80 -clib=classic -crt0=crt0.asm -pragma-define:CRT_MODEL=0 -pragma-define:HDOS=0x1 -pragma-define:CRT_ENABLE_COMMANDLINE=0x1 -pragma-define:CRT_ORG_CODE=0x2280 -pragma-define:CRT_ON_EXIT=0x10002 ../CPM/cecho.c monitor.c -create-app -m
	../HDOS/bin2abs.py a.rom cecho.abs

clean:
	$(RM) *.map *.bin *.rom *.com *.abs
