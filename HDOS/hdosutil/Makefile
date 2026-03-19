libtest.abs: libtest.c hdosutil.lib
	. ../../z88dk/env.sh && zcc +hdos -create-app -Cz--abs -pragma-define:CLIB_DISABLE_FGETS_CURSOR=1 -lhdosutil -o libtest.bin libtest.c

hdosutil.lib: beep.o clock.o delay.o rddate.o rdtime.o scall.o wrdate.o wrtime.o
	. ../../z88dk/env.sh && z80asm -xhdosutil beep.o clock.o delay.o rddate.o rdtime.o scall.o wrdate.o wrtime.o

beep.o: beep.c
	. ../../z88dk/env.sh && zcc -mz80 +hdos -c beep.c

clock.o: clock.c
	. ../../z88dk/env.sh && zcc -mz80 +hdos -c clock.c

delay.o: delay.c
	. ../../z88dk/env.sh && zcc -mz80 +hdos -c delay.c

rddate.o: rddate.c
	. ../../z88dk/env.sh && zcc -mz80 +hdos -c rddate.c

rdtime.o: rdtime.c
	. ../../z88dk/env.sh && zcc -mz80 +hdos -c rdtime.c

scall.o: scall.c
	. ../../z88dk/env.sh && zcc -mz80 +hdos -c scall.c

wrdate.o: wrdate.c
	. ../../z88dk/env.sh && zcc -mz80 +hdos -c wrdate.c

wrtime.o: wrtime.c
	. ../../z88dk/env.sh && zcc -mz80 +hdos -c wrtime.c

clean:
	$(RM) *.o *.bin *.abs *.lib
