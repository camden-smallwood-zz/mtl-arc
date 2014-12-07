all:
	gcc -o mtl-arc *.c -std=c11 -D_GNU_SOURCES -lm

clean:
	rm mtl-arc