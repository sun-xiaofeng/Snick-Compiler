# Makefile for the brill emulator

HDR =		brill.h std.h missing.h
SRC = 		brill.c pbrill.c lbrill.c
OBJ = 		brill.o pbrill.o lbrill.o
CC =	 	gcc
# We need -ffloat-store to help ensure consistent results for 
# floating point operations on different architectures and with 
# different optimization levels.
CFLAGS =	-Wall -ffloat-store -g -O0
LINKFLAGS =	-g

brill:		$(OBJ)
		$(CC) $(CFLAGS) $(LINKFLAGS) -o brill $(OBJ) -lm

pbrill.c pbrill.h:	pbrill.y
		bison -p brillyy -d pbrill.y -o pbrill.c

lbrill.c:		lbrill.l pbrill.h
		flex -s -t -Pbrillyy lbrill.l > lbrill.c

clean:
		-/bin/rm -f $(OBJ) pbrill.c pbrill.h lbrill.c
clobber: clean
		-/bin/rm -f brill

$(OBJ):		$(HDR)
