all : asms runner

asms :
	./disassembler.py -d < ../angabe/bin1.obf > bin1.asm
	./disassembler.py -d < ../angabe/bin2.obf > bin2.asm
	./disassembler.py -d < ../angabe/bin3.obf > bin3.asm

runner : runner.c out.c
	gcc -o runner -g -Wall -std=c99 runner.c -lm `pkg-config --cflags --libs glib-2.0`
