all: xdemo.abs xcls.abs c_progs

c_progs: fileio.abs advent1.abs advent2.abs advent3.abs advent4.abs

xdemo.abs: xdemo.bin
	./bin2abs.py xdemo.bin xdemo.abs

xdemo.hex: xdemo.p
	p2hex -F Intel xdemo.p

xdemo.bin: xdemo.p
	p2bin -r '$$-$$' xdemo.p

xdemo.p: xdemo.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . xdemo.asm

xcls.abs: xcls.bin
	./bin2abs.py xcls.bin xcls.abs

xcls.hex: xcls.p
	p2hex -F Intel xcls.p

xcls.bin: xcls.p
	p2bin -r '$$-$$' xcls.p

xcls.p: xcls.asm
	asl -L -cpu 8080 -listradix 8 -splitbyte . xcls.asm

fileio.abs: ../CPM/fileio.c
	. ../z88dk/env.sh && zcc -mz80 +hdos -create-app -Cz--abs -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o fileio.bin ../CPM/fileio.c

advent1.abs: ../CPM/advent1.c
	. ../z88dk/env.sh && zcc -mz80 +hdos -create-app -Cz--abs -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o advent1.bin ../CPM/advent1.c

advent2.abs: ../CPM/advent2.c
	. ../z88dk/env.sh && zcc -mz80 +hdos -create-app -Cz--abs -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o advent2.bin ../CPM/advent2.c

advent3.abs: ../CPM/advent3.c
	. ../z88dk/env.sh && zcc -mz80 +hdos -create-app -Cz--abs -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o advent3.bin ../CPM/advent3.c

advent4.abs: ../CPM/advent4.c
	. ../z88dk/env.sh && zcc -mz80 +hdos -create-app -Cz--abs -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o advent4.bin ../CPM/advent4.c

clean:
	$(RM) *.p

distclean:
	$(RM) *.lst *.p *.bin *.hex *.abs
