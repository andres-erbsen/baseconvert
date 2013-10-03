/* vim: set ts=4 sw=4 expandtab : */
#include "baseconvert.h"
#include <memory.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>

typedef unsigned int uint;

#define USAGE "Usage: baseconvert IN_DIGITS_FILE OUT_DIGITS_FILE\n\
Digits files: ith character represents i in that base.\n\
Data to be converted is read from stdin and results are written to stdout.\n\
Example: echo -n 15 | ./baseconvert decimal.txt base2.txt # gives 1111"

static void die (const char * format, ...) {
    va_list vargs;
    va_start(vargs, format);
    vfprintf(stderr, format, vargs);
    fprintf(stderr, "\n");
    exit (1);
}

/* Read all that there is from a file handle */
uint freadall(char** target, FILE* fh) {
    uint bufsize = 1024;
    char *buf = malloc(bufsize);
    if (buf == NULL) return 0;
    uint len = 0;
    while (1) {	
        if ( (len = len + fread(buf+len,1,bufsize-len,stdin)) == bufsize ) {
            bufsize *= 2;
            buf = realloc(buf,bufsize);
        } else if ( ferror(stdin) ) {
            perror("Reading everything there is from a handle");
        }
        else break; /* EOF */
    }
    *target = buf;
    return len;
}

int main(int argc, char** argv) {
    unsigned long in_radix=0, out_radix=0;
    char *in_digits=NULL, *out_digits=NULL;
    int i;

    /* Command line arguments */
	for (i = 1; i < argc; i++) {
		if (in_digits == NULL) {
			in_digits = argv[i];
		} else if (out_digits == NULL) {
			out_digits = argv[i];
		} else die(USAGE);
	}
	if (in_digits == NULL || out_digits == NULL) die(USAGE);

    /* Read digits of the input base */
	FILE *fh = fopen(in_digits, "rb");
	in_digits = NULL;
	if ( fh != NULL ) {
		fseek(fh, 0L, SEEK_END);
		in_radix = ftell(fh);
		rewind(fh);
		in_digits = malloc(in_radix);
		if ( in_digits != NULL ) fread(in_digits, in_radix, 1, fh);
		fclose(fh);
	}
	if (in_digits == NULL) die("In digits file bad.");
	/* fwrite(in_digits, in_radix, 1, stderr); putchar('\n'); */

	fh = fopen(out_digits, "rb");
	out_digits = NULL;
	if ( fh != NULL ) {
		fseek(fh, 0L, SEEK_END);
		out_radix = ftell(fh);
		rewind(fh);
		out_digits = malloc(out_radix);
		if ( out_digits != NULL ) fread(out_digits, out_radix, 1, fh);
		fclose(fh);
	}
	if (out_digits == NULL) die("Out digits file bad.");
	/* fwrite(out_digits, out_radix, 1, stderr); putchar('\n'); */
    
    uint8_t in_max_digit = in_radix-1, out_max_digit = out_radix-1;
    char* stdinbytes;
    freopen(NULL, "rb", stdin);
    uint in_len = freadall(&stdinbytes,stdin);
    if (!baseconvert_is_valid_in_base(in_max_digit,in_digits,stdinbytes,in_len)) {
        if (stdinbytes[in_len-1] == '\n')
            die("Bad input. Maybe because of a newline at the end?");
        else die("Bad input.");
    }
    uint out_len = baseconvert_targetlen(in_max_digit, out_max_digit, in_len);
    char* converted = malloc(out_len);
    if (converted == NULL) die("Failed to allocate memory.");
    out_len = baseconvert(in_max_digit, in_digits, stdinbytes, in_len,
                         out_max_digit, out_digits, converted, out_len);
	if (fwrite(converted, out_len, 1, stdout) != 1) perror("Writing final output");
    return 0;
}
