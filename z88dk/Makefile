ORG = 0x2680
PRAGMAS = -DHDOS -pragma-define:CRT_MODEL=0 -pragma-define:HDOS=0x1 -pragma-define:CRT_ENABLE_COMMANDLINE=0x1 -pragma-define:CRT_ORG_CODE=$(ORG) -pragma-define:CRT_ON_EXIT=0x10002 -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -pragma-define:CRT_STACK_SIZE=280 -pragma-define:CLIB_MALLOC_HEAP_SIZE=0 -pragma-define:CLIB_EXIT_STACK_SIZE=0 -pragma-define:TAR__register_sp=$(ORG)

all: world.abs advent1.abs advent2.abs advent3.abs advent4.abs cecho.abs fileio.abs scall.abs mathdemo.abs datetime.abs

msx: scale.abs bars.abs joy.abs psg.abs vdp.abs

world.abs: world.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) world.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom world.abs

scall.abs: scall.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) scall.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom scall.abs

datetime.abs: datetime.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) datetime.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom datetime.abs

advent1.abs: ../CPM/advent1.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/advent1.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom advent1.abs

advent2.abs: ../CPM/advent2.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/advent2.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom advent2.abs

advent3.abs: ../CPM/advent3.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/advent3.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom advent3.abs

advent4.abs: ../CPM/advent4.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/advent4.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom advent4.abs

cecho.abs: ../CPM/cecho.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/cecho.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom cecho.abs

fileio.abs: ../CPM/fileio.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) ../CPM/fileio.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom fileio.abs

mathdemo.abs: ../CPM/mathdemo.c hdos.c crt0.asm
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) -lm ../CPM/mathdemo.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom mathdemo.abs

scale.abs: ../H89MSX/examples/scale.c
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) -lm ../H89MSX/examples/scale.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom scale.abs

bars.abs: ../H89MSX/examples/bars.c
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) -lm ../H89MSX/examples/bars.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom bars.abs

joy.abs: ../H89MSX/examples/joy.c
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) -lm ../H89MSX/examples/joy.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom joy.abs

psg.abs: ../H89MSX/examples/psg.c
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) -lm ../H89MSX/examples/psg.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom psg.abs

vdp.abs: ../H89MSX/examples/vdp.c
	. ./env.sh && zcc +z80 -clib=classic -crt0=crt0.asm $(PRAGMAS) -lm ../H89MSX/examples/vdp.c hdos.c -create-app -m
	../HDOS/bin2abs.py -a $(ORG) a.rom vdp.abs

clean:
	$(RM) *.map *.bin *.rom *.abs
