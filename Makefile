all:
	gcc -o mtl-arc -std=c11 *.c

clean:
	rm mtl-arc
