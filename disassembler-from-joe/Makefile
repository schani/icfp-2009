all : asms runner1 runner2 runner3 runner4

asms :
	./disassembler.py -d < ../angabe/bin1.obf > bin1.asm
	./disassembler.py -d < ../angabe/bin2.obf > bin2.asm
	./disassembler.py -d < ../angabe/bin3.obf > bin3.asm
	./disassembler.py -d < ../angabe/bin4.obf > bin4.asm

runner1 : runner.c bin1.c
	gcc -o runner1 -DBIN1 -g -Wall -std=c99 runner.c -lm `pkg-config --cflags --libs glib-2.0`

runner2 : runner.c bin2.c ../search/search.c ../search/search.h
	gcc -o runner2 -I../search -DBIN2 -g -Wall -std=c99 runner.c ../search/search.c -lm `pkg-config --cflags --libs glib-2.0`

runner3 : runner.c bin3.c ../search/search.c ../search/search.h
	gcc -o runner3 -I../search -DBIN3 -g -Wall -std=c99 runner.c ../search/search.c -lm `pkg-config --cflags --libs glib-2.0`

runner4 : runner.c bin4.c
	gcc -o runner4 -DBIN4 -g -Wall -std=c99 runner.c -lm `pkg-config --cflags --libs glib-2.0`



bin1.c : disassembler.py
	./disassembler.py -c < ../angabe/bin1.obf > bin1.c

bin2.c : disassembler.py
	./disassembler.py -c < ../angabe/bin2.obf > bin2.c

bin3.c : disassembler.py
	./disassembler.py -c < ../angabe/bin3.obf > bin3.c

bin4.c : disassembler.py
	./disassembler.py -c < ../angabe/bin4.obf > bin4.c


t34test : t34test.c
	gcc -Wall -O3 -lm -o t34test t34test.c
