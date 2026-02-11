ORG = 0x2680
PRAGMAS = -DHDOS -pragma-define:CRT_MODEL=0 -pragma-define:HDOS=0x1 -pragma-define:CRT_ENABLE_COMMANDLINE=0x1 -pragma-define:CRT_ORG_CODE=$(ORG) -pragma-define:CRT_ON_EXIT=0x10002 -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -pragma-define:CRT_STACK_SIZE=280 -pragma-define:CLIB_MALLOC_HEAP_SIZE=0 -pragma-define:CLIB_EXIT_STACK_SIZE=0 -pragma-define:TAR__register_sp=$(ORG)

all: world.abs advent1.abs advent2.abs advent3.abs cecho.abs fileio.abs scall.abs mathdemo.abs

world.abs: world.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) world.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom world.abs

scall.abs: scall.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) scall.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom scall.abs

advent1.abs: ../CPM/advent1.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/advent1.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom advent1.abs

advent2.abs: ../CPM/advent2.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/advent2.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom advent2.abs

advent3.abs: ../CPM/advent3.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/advent3.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom advent3.abs

cecho.abs: ../CPM/cecho.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/cecho.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom cecho.abs

fileio.abs: ../CPM/fileio.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/fileio.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom fileio.abs

mathdemo.abs: ../CPM/mathdemo.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) -lm ../CPM/mathdemo.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom mathdemo.abs

clean:
	$(RM) *.map *.bin *.rom *.abs
