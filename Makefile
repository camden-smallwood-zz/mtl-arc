all:
	gcc -o mtl-arc mtl-arc.c -std=gnu11 -lm

clean:
	rm mtl-arc
