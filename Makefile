CC=gcc
LDLIBS=-lm -ltommath
CFLAGS=-Wall -ansi -O3

baseconvert: baseconvert.o baseconvert-cli.c

baseconvert.o: baseconvert.c baseconvert.h

install:
	install -Dm644 baseconvert.h "$prefix/usr/include/baseconvert.h"
	install -Dm644 baseconvert.o "$prefix/usr/include/baseconvert.o"
	install -Dm755 baseconvert "$prefix/usr/bin/baseconvert"

clean:
	$(RM) *.o baseconvert
