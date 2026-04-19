TXT=$(wildcard *.txt)
MEM=$(TXT:.txt=.mem)

all: bitmap2mem $(MEM)

bitmap2mem: bitmap2mem.c
	gcc $^ -o $@

%.mem: %.txt
	./bitmap2mem $^ > $@
