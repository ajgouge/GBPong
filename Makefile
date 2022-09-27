
bin/pong.gb: obj/pong.o
	rgblink -o bin/pong.gb -n bin/pong.sym -m bin/pong.map obj/pong.o
	rgbfix -v -p 0 bin/pong.gb

obj/pong.o: src/pong.asm
	rgbasm -E -o obj/pong.o src/pong.asm

clean:
	rm obj/* bin/*
