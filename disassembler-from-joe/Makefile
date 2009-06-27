all : asms runner1 runner2 runner3

asms :
	./disassembler.py -d < ../angabe/bin1.obf > bin1.asm
	./disassembler.py -d < ../angabe/bin2.obf > bin2.asm
	./disassembler.py -d < ../angabe/bin3.obf > bin3.asm

runner1 : runner.c bin1.c
	gcc -o runner1 -DBIN1 -g -Wall -std=c99 runner.c -lm `pkg-config --cflags --libs glib-2.0`

runner2 : runner.c bin2.c
	gcc -o runner2 -DBIN2 -g -Wall -std=c99 runner.c -lm `pkg-config --cflags --libs glib-2.0`

runner3 : runner.c bin3.c
	gcc -o runner3 -DBIN3 -g -Wall -std=c99 runner.c -lm `pkg-config --cflags --libs glib-2.0`

bin1.c : disassembler.py
	./disassembler.py -c < ../angabe/bin1.obf > bin1.c

bin2.c : disassembler.py
	./disassembler.py -c < ../angabe/bin2.obf > bin2.c

bin3.c : disassembler.py
	./disassembler.py -c < ../angabe/bin3.obf > bin3.c

bin4.c : disassembler.py
	./disassembler.py -c < ../angabe/bin4.obf > bin4.c
