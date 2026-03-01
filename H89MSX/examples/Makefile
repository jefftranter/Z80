all: scale.com bars.com

scale.com: scale.c
	. ../../z88dk/env.sh && zcc -mz80 +cpm -subtype=hz89 -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o scale.com scale.c 

bars.com: bars.c
	. ../../z88dk/env.sh && zcc -mz80 +cpm -subtype=hz89 -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lm -o bars.com bars.c 

clean:
	$(RM) scale.com bars.com

