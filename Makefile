CC=gcc
LDLIBS=-lm -ltommath
CFLAGS=-Wall -ansi -O3

baseconvert: baseconvert.o baseconvert-cli.c

baseconvert.o: baseconvert.c baseconvert.h

install:
	install -Dm644 baseconvert.h "$(prefix)/usr/include/baseconvert.h"
	install -Dm644 baseconvert.o "$(prefix)/usr/include/baseconvert.o"
	install -Dm644 bases/        "$(prefix)/usr/share/baseconvert/"
	install -d                   "$(prefix)/usr/bin/baseconvert"
	install -Dm644 bases/*       "$(prefix)/usr/bin/baseconvert"

clean:
	$(RM) *.o baseconvert
