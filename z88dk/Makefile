PRAGMAS = -pragma-define:CRT_MODEL=0 -pragma-define:HDOS=0x1 -pragma-define:CRT_ENABLE_COMMANDLINE=0x1 -pragma-define:CRT_ORG_CODE=0x2280 -pragma-define:CRT_ON_EXIT=0x10002 -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1

all: world.abs advent1.abs advent2.abs advent3.abs cecho.abs fileio.abs scall.abs

world.abs: world.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) world.c hdos.c -create-app -m
	../HDOS/bin2abs.py a.rom world.abs

scall.abs: scall.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) scall.c hdos.c -create-app -m
	../HDOS/bin2abs.py a.rom scall.abs

advent1.abs: ../CPM/advent1.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/advent1.c hdos.c -create-app -m
	../HDOS/bin2abs.py a.rom advent1.abs

advent2.abs: ../CPM/advent2.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/advent2.c hdos.c -create-app -m
	../HDOS/bin2abs.py a.rom advent2.abs

advent3.abs: ../CPM/advent3.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/advent3.c hdos.c -create-app -m
	../HDOS/bin2abs.py a.rom advent3.abs

cecho.abs: ../CPM/cecho.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/cecho.c hdos.c -create-app -m
	../HDOS/bin2abs.py a.rom cecho.abs

fileio.abs: ../CPM/fileio.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/fileio.c hdos.c -create-app -m
	../HDOS/bin2abs.py a.rom fileio.abs

clean:
	$(RM) *.map *.bin *.rom *.abs
