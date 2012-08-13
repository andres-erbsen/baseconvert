/* vim: set ts=4 sw=4 expandtab : */
#include <stdio.h>
#include <math.h>
#include <stdint.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include "tommath.h"
typedef unsigned int uint;

/* gcc -O3 -funroll-loops -finline-functions baseconvert.c -lm -ltommath -o baseconvert */


#define USAGE "Usage: baseconvert IN_DIGITS_FILE OUT_DIGITS_FILE\n\
Digits files: ith character represents i in that base.\n\
Data to be converted is read from stdin and results are written to stdout.\n\
Example: echo -n 15 | ./baseconvert decimal.txt base2.txt # gives 1111"

/** all shortcuts available using 32-bit unsigned integers **/
/* BIGGER_RADIX_GROUP[i] digits of base BIGGER_RADIX[i] can be converted to */
/* SMALLER_RADIX_GROUP[i] digits if base SMALLER_RADIX[i] or vice versa */
/* for l in zip(*filter(lambda l: int(l[0])**int(l[1]) < 2**32, (re.findall('[0-9]+',s) for s in open('shortcuts.txt').read().splitlines()))): print('{'+', '.join(l),'};\n') */
#define N_TRICKS 47
static const uint16_t BIGGER_RADIX[N_TRICKS] = {4, 8, 8, 9, 16, 16, 16, 25, 27, 27, 32, 32, 32, 32, 36, 49, 64, 64, 64, 64, 64, 81, 81, 81, 100, 121, 125, 125, 128, 128, 128, 128, 144, 169, 196, 216, 216, 225, 243, 243, 243, 243, 256, 256, 256, 256, 256};

static const uint16_t BIGGER_RADIX_GROUP[N_TRICKS] = {1, 1, 2, 1, 1, 1, 3, 1, 1, 2, 1, 2, 3, 4, 1, 1, 1, 1, 1, 2, 5, 1, 1, 3, 1, 1, 1, 2, 1, 2, 3, 4, 1, 1, 1, 1, 2, 1, 1, 2, 3, 4, 1, 1, 3, 1, 3};

static const uint16_t SMALLER_RADIX[N_TRICKS] = {2, 2, 4, 3, 2, 4, 8, 5, 3, 9, 2, 4, 8, 16, 6, 7, 2, 4, 8, 16, 32, 3, 9, 27, 10, 11, 5, 25, 2, 4, 8, 16, 12, 13, 14, 6, 36, 15, 3, 9, 27, 81, 2, 4, 8, 16, 64};

static const uint16_t SMALLER_RADIX_GROUP[N_TRICKS] = {2, 3, 3, 2, 4, 2, 4, 2, 3, 3, 5, 5, 5, 5, 2, 2, 6, 3, 2, 3, 6, 4, 2, 4, 2, 2, 3, 3, 7, 7, 7, 7, 2, 2, 2, 3, 3, 2, 5, 5, 5, 5, 8, 4, 8, 2, 4};


static void die (const char * format, ...) {
    va_list vargs;
    va_start(vargs, format);
    vfprintf(stderr, format, vargs);
    fprintf(stderr, "\n");
    exit (1);
}

unsigned char ord(char c) {
    return (unsigned char) c;
}

/** Brute force base conversion using multiprecision arithmetic **/
uint _baseconvert_dumb( uint in_radix
                      , uint* in_value /* NOT digits, reverse lookup */
                      , char* in_chars
                      , uint  in_len
                      , uint out_radix
                      , char* out_digits
                      , char* out_chars
                      , uint  out_len )
{
    /* fprintf(stderr,"dumb conversion...\n"); */
    uint i;
    unsigned long d = 0;
    mp_int acc;
    mp_init(&acc);
    /* from in_radix to libtom mp_int */
    for (i = 0; i < in_len; i++) { /*  acc = acc * in_radix + newdigit  */
        mp_mul_d(&acc, in_radix, &acc);
        mp_add_d(&acc, in_value[ ord(in_chars[i]) ], &acc);
    }
    /* from mp_int to out_radix */
    char* s = out_chars + out_len; /*  start filling in digits from the right  */
    do {
        s--;
        mp_div_d(&acc, out_radix, &acc, &d); /* (acc, d) = divmod(acc, out_radix) */
        assert(s >= out_chars);
        *s = out_digits[d];
    } while ( !mp_iszero(&acc) );
    mp_clear(&acc);
    uint len = out_len - (s - out_chars);
    if (s != out_chars) memmove(out_chars, s, len);
    return len;
}

/** Clever, groupwise base conversion **/
/* Get size of groups possible to convert using uint32 state */
void _baseconvert_group_size_uint32( uint in_radix
                            , uint out_radix
                            , uint8_t* in_group
                            , uint8_t* out_group)
{
    uint bigger_radix, smaller_radix, i;
    if (in_radix > out_radix) {
        bigger_radix = in_radix;
        smaller_radix = out_radix;
    } else {
        bigger_radix = out_radix;
        smaller_radix = in_radix;
    }
    for (i=0; i<N_TRICKS; i++) {
        if (BIGGER_RADIX[i] == bigger_radix && SMALLER_RADIX[i] == smaller_radix) {
            if (in_radix > out_radix) {
                *in_group = BIGGER_RADIX_GROUP[i];
                *out_group = SMALLER_RADIX_GROUP[i];
            } else {
                *in_group = SMALLER_RADIX_GROUP[i];
                *out_group = BIGGER_RADIX_GROUP[i];
            }
            break;
        }
    }

}

/* These are macros because they will be reused for uint64 shortcuts */
#define SHAKE() do { \
    if (acc || gotsomething) { /* omit leading 0 groups */ \
        for (j = out_group - 1;  j >= 0;  j--)  { \
            s[j] = out_digits[acc % out_radix]; \
            acc /= out_radix; \
            if (gotsomething == 0 && acc == 0 ) { \
                memmove(s,s+j,out_group - j); /* delete leading 0s */ \
                s -= j; /* out_group is added later */ \
                break; \
            } \
        } \
        gotsomething = 1; \
        s += out_group; \
    } \
} while(0);

#define STEP(val) do { \
    acc *= in_radix; \
    acc += val; \
    stepsleft--; \
    if (stepsleft == 0) { \
        SHAKE(); \
        assert(acc == 0); \
        stepsleft = in_group; \
    } \
} while (0);


uint _baseconvert_group_uint32( uint in_radix
                              , uint* in_value /* NOT digits, reverse lookup */
                              , char* in_chars
                              , uint  in_len
                              , uint8_t in_group
                              , uint out_radix
                              , char* out_digits
                              , char* out_chars
                              , uint  out_len 
                              , uint8_t out_group)
{
    /* fprintf(stderr,"groupwise conversion!\n"); */
    uint8_t stepsleft = in_group, gotsomething=0;
    char* s = out_chars;
    uint32_t acc = 0;
    int i, j;

    for (i=0; i<(in_group-in_len%in_group)%in_group; i++) STEP(0);
    for (i=0; i<in_len; i++) STEP( in_value[ ord(in_chars[i]) ] );
    if (stepsleft != out_group) SHAKE();
    return s - out_chars - out_group; /* s is `out_group` ahead of end */
}

/* TODO: groupwise conversions for which state does not fit in uint32 */

#undef STEP
#undef SHAKE


/* Is this a valid number in base X? */
int baseconvert_is_valid_base(uint radix, char* digits, char* chars, uint len) {
    if (radix < 2) return 0;
    if (len == 0) return 0;
    char allowed[256];
    memset(allowed,0,256);
    int i;
    for (i=0; i<radix; i++) allowed[ord(digits[i])] = 1;
    for (i=0; i<len; i++) if (!allowed[ord(chars[i])]) return 0;
    return 1;
}

/* Wrap groupwise and bignum-based conversion, automatically choose between them */
uint baseconvert( uint in_radix
                 , char* in_digits
                 , char* in_chars
                 , uint  in_len
                 , uint out_radix
                 , char* out_digits
                 , char* out_chars
                 , uint  out_len )
{
    uint i;
    /* digit -> value lookup table */
    uint in_value[256];
	for (i=0; i<in_radix; i++) in_value[ord(in_digits[i])] = i;
    /** Find out whether there exists a shortcut **/
    uint8_t in_group=0, out_group=0;
    _baseconvert_group_size_uint32(in_radix, out_radix, &in_group, &out_group);
    if (in_group && out_group) {
        /** Convert `in_group` digits to `out_group` digits using uint32 **/
        /* fprintf(stderr, "%d^%d == %d^%d\n", in_radix, in_group, out_radix, out_group); */
        return _baseconvert_group_uint32( in_radix, in_value, in_chars, in_len
                                        , in_group, out_radix, out_digits
                                        , out_chars, out_len, out_group);
    }
    /* If nothing else works... */
    return _baseconvert_dumb(in_radix, in_value, in_chars, in_len,
                            out_radix, out_digits, out_chars, out_len);
}

uint baseconvert_targetlen(uint in_radix, uint out_radix, uint in_len) {
    return 1 + in_len * log(in_radix)/log(out_radix) * 1.0000001;
}

/* Read all that there is from a file handle */
uint freadall(char** target, FILE* fh) {
    uint bufsize = 1024;
    char *buf = malloc(bufsize);
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
    
    char* stdinbytes;
    freopen(NULL, "rb", stdin);
    uint in_len = freadall(&stdinbytes,stdin);
    if (!baseconvert_is_valid_base(in_radix,in_digits,stdinbytes,in_len)) {
        die("Bad input. Maybe because of a newline at the end?");
    }
    uint out_len = baseconvert_targetlen(in_radix, out_radix, in_len);
    char* converted = malloc(out_len);
    out_len = baseconvert(in_radix, in_digits, stdinbytes, in_len,
                         out_radix, out_digits, converted, out_len);
	if (fwrite(converted, out_len, 1, stdout) != 1) perror("Writing final output");
    return 0;
}
