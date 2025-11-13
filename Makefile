
all: AMIGA_ROOT build

AMIGA_ROOT:
	mkdir -p AMIGA_ROOT/s
	echo "4th" > AMIGA_ROOT/s/startup-sequence

build:
	vasmm68k_mot -m68000 -Fhunk 4th.s -o 4th.o
	vlink -bamigahunk -Bstatic 4th.o -o AMIGA_ROOT/4th

run:
	fs-uae -f runtime.fs-uae

