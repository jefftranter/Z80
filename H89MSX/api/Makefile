all: ex1.com ex1.abs

ex1.com: ex1.c ay8910.c
	. ../../z88dk/env.sh && zcc -mz80 +cpm -subtype=hz89 -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o ex1.com ay8910.c ex1.c 

ex1.abs: ex1.c ay8910.c
	. ../../z88dk/env.sh && zcc -mz80 +hdos -create-app -Cz--abs -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o ex1.bin ay8910.c ex1.c

clean:
	$(RM)  ex1.com ex1.abs ex1.bin
