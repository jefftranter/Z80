all: xdemo.abs xcls.abs

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

clean:
	$(RM) *.p

distclean:
	$(RM) *.lst *.p *.bin *.hex *.abs
