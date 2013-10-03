CC=gcc
LDLIBS=-lm -ltommath
CFLAGS=-Wall -ansi -O3

baseconvert: baseconvert.o baseconvert-cli.c

baseconvert.o: baseconvert.c baseconvert.h

clean:
	$(RM) *.o baseconvert
