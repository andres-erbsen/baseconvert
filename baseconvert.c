/* vim: set ts=4 sw=4 expandtab : */
#include "baseconvert.h"
#include <math.h>
#include <stdint.h>
#include <assert.h>
#include "tommath.h"

typedef unsigned int uint;

/* BIGGER_GROUP[i] digits of base with BIGGER_MAX_DIGIT[i] can be converted to */
/* SMALLER_GROUP[i] digits if base SMALLER_MAX_DIGIT[i] or vice versa */
/* based on shortcuts.txt, all but the last 4 can be used with uint32 */
#define N_TRICKS_UINT32 47
#define N_TRICKS_UINT64 51
#define N_TRICKS N_TRICKS_UINT64
static const uint16_t BIGGER_MAX_DIGIT[N_TRICKS] = {3, 7, 7, 8, 15, 15, 15, 24, 26, 26, 31, 31, 31, 31, 35, 48, 63, 63, 63, 63, 63, 80, 80, 80, 99, 120, 124, 124, 127, 127, 127, 127, 143, 168, 195, 215, 215, 224, 242, 242, 242, 242, 255, 255, 255, 255, 255,  127,127,255,255 };

static const uint16_t BIGGER_GROUP[N_TRICKS] = {1,1,2,1,1,1,3,1,1,2,1,2,3,4,1,1,1,1,1,2,5,1,1,3,1,1,1,2,1,2,3,4,1,1,1,1,2,1,1,2,3,4,1,1,3,1,3,  5,6,5,7};

static const uint16_t SMALLER_MAX_DIGIT[N_TRICKS] = {1, 1, 3, 2, 1, 3, 7, 4, 2, 8, 1, 3, 7, 15, 5, 6, 1, 3, 7, 15, 31, 2, 8, 26, 9, 10, 4, 24, 1, 3, 7, 15, 11, 12, 13, 5, 35, 14, 2, 8, 26, 80, 1, 3, 7, 15, 63,  31,63,31,127 };

static const uint16_t SMALLER_GROUP[N_TRICKS] = {2,3,3,2,4,2,4,2,3,3,5,5,5,5,2,2,6,3,2,3,6,4,2,4,2,2,3,3,7,7,7,7,2,2,2,3,3,2,5,5,5,5,8,4,8,2,4,  7,7,8,8};


unsigned char ord(char c) {
    return (unsigned char) c;
}

/** Brute force base conversion using multiprecision arithmetic **/
uint _baseconvert_dumb( uint8_t in_max_digit
                      , uint8_t* in_value /* NOT digits, reverse lookup */
                      , char* in_chars
                      , uint  in_len
                      , uint8_t out_max_digit
                      , char* out_digits
                      , char* out_chars
                      , uint  out_len )
{
    /* fprintf(stderr,"dumb conversion...\n"); */
    uint i;
    unsigned long d = 0, in_radix = in_max_digit+1, out_radix = out_max_digit+1;
    mp_int acc;
    mp_init(&acc);
    /* from input base to libtom mp_int */
    for (i = 0; i < in_len; i++) { /*  acc = acc * in_radix + newdigit  */
        mp_mul_d(&acc, in_radix, &acc);
        mp_add_d(&acc, in_value[ ord(in_chars[i]) ], &acc);
    }
    /* from mp_int to output base */
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
void baseconvert_group_size( uint m
                            , uint n
                            , uint8_t in_max_digit
                            , uint8_t out_max_digit
                            , uint8_t* in_group
                            , uint8_t* out_group)
{
    uint bigger_max_digit, smaller_max_digit, i;
    if (in_max_digit > out_max_digit) {
        bigger_max_digit = in_max_digit;
        smaller_max_digit = out_max_digit;
    } else {
        bigger_max_digit = out_max_digit;
        smaller_max_digit = in_max_digit;
    }
    for (i=m; i<n; i++) {
        if (BIGGER_MAX_DIGIT[i] == bigger_max_digit && SMALLER_MAX_DIGIT[i] == smaller_max_digit) {
            if (in_max_digit > out_max_digit) {
                *in_group = BIGGER_GROUP[i];
                *out_group = SMALLER_GROUP[i];
            } else {
                *in_group = SMALLER_GROUP[i];
                *out_group = BIGGER_GROUP[i];
            }
            break;
        }
    }

}

/* These are macros because they will be reused for uint32 and uint64 shortcuts */
#define SHAKE() do { \
    /*fprintf(stderr," -> %u -> ",acc);*/ \
    group = nextgroup; \
    nextgroup += out_group_size; \
    s = nextgroup - 1; \
    while ( (acc || group != out_chars) && s >= group )  { \
        *s = out_digits[acc % out_radix]; \
        acc /= out_radix; \
        s--; \
    } \
    s++; /* now s >= group, could have been s = group - 1 otherwise */ \
    /* omit leading 0s */ \
    if (s > group) { \
        memmove(group, s, nextgroup - s); \
        nextgroup = group + (nextgroup - s); \
    } \
	/*fwrite(group, nextgroup - group, 1, stderr); fwrite("\n", 1, 1, stderr);*/ \
} while(0);

#define STEP(val) do { \
    /*fprintf(stderr,"%d,",val);*/ \
    acc *= in_radix; \
    acc += val; \
    stepsleft--; \
    if (stepsleft == 0) { \
        SHAKE(); \
        assert(acc == 0); \
        stepsleft = in_group_size; \
    } \
} while (0);


#define ROLL(STATET) \
uint _baseconvert_group_ ## STATET \
                              ( uint8_t in_max_digit \
                              , uint8_t* in_value /* NOT digits, reverse lookup */ \
                              , char* in_chars \
                              , uint  in_len \
                              , uint8_t in_group_size \
                              , uint8_t out_max_digit \
                              , char* out_digits \
                              , char* out_chars \
                              , uint  out_len \
                              , uint8_t out_group_size) \
{ \
    /* fprintf(stderr,"groupwise conversion!\n"); */ \
    STATET acc = 0, in_radix = in_max_digit+1, out_radix = out_max_digit+1; \
    unsigned char stepsleft = in_group_size; \
    char *nextgroup = out_chars, *group = 0, *s = 0; \
    uint i; \
    /* fprintf(stderr,"%u %u\n",in_group_size, out_group_size);*/ \
    /* pretend leading 0s */ \
    for (i=0; i<(in_group_size-in_len%in_group_size)%in_group_size; i++) STEP(0);  \
    for (i=0; i<in_len; i++) STEP( in_value[ ord(in_chars[i]) ] ); \
    assert (stepsleft == in_group_size); \
    if (nextgroup == out_chars) { /* prevent empty output, add a 0 */ \
        *nextgroup = *out_digits; \
        nextgroup++; \
    } \
    return nextgroup - out_chars; \
}

ROLL(uint32_t)
ROLL(uint64_t)

#undef STEP
#undef SHAKE
#undef ROLL


/* Is this a valid number in base X? */
int baseconvert_is_valid_in_base(uint8_t max_digit, char* digits, char* chars, uint len) {
    if (max_digit == 0) return 0;
    if (len == 0) return 0;
    char allowed[256];
    memset(allowed,0,256);
    int i;
    for (i=0; i<=max_digit; i++) allowed[ord(digits[i])] = 1;
    for (i=0; i<len; i++) if (!allowed[ord(chars[i])]) return 0;
    return 1;
}

/* Wrap groupwise and bignum-based conversion, automatically choose between them */
uint baseconvert( uint8_t in_max_digit
                 , char* in_digits
                 , char* in_chars
                 , uint  in_len
                 , uint8_t out_max_digit
                 , char* out_digits
                 , char* out_chars
                 , uint  out_len )
{
    /* fprintf(stderr,"baseconvert(%d, [...], [...], %d, %d, [...] [...], %d)\n", in_max_digit, in_len, out_max_digit, out_len); */
    uint i;
    /* digit -> value lookup table */
    uint8_t in_value[256];
	for (i=0; i<=in_max_digit; i++) in_value[ord(in_digits[i])] = i;
    /** Find out whether there exists a shortcut **/
    uint8_t in_group=0, out_group=0;
    /** Convert `in_group` digits to `out_group` digits using uint32 **/
    baseconvert_group_size(0,N_TRICKS_UINT32,
                            in_max_digit, out_max_digit, &in_group, &out_group);
    if (in_group && out_group) {
        return _baseconvert_group_uint32_t( in_max_digit, in_value, in_chars, in_len
                                          , in_group, out_max_digit, out_digits
                                          , out_chars, out_len, out_group);
    }
    baseconvert_group_size(N_TRICKS_UINT32,N_TRICKS_UINT64,
                            in_max_digit, out_max_digit, &in_group, &out_group);
    if (in_group && out_group) {
        return _baseconvert_group_uint64_t( in_max_digit, in_value, in_chars, in_len
                                          , in_group, out_max_digit, out_digits
                                          , out_chars, out_len, out_group);
    }
    /* If nothing else works... */
    return _baseconvert_dumb(in_max_digit, in_value, in_chars, in_len,
                            out_max_digit, out_digits, out_chars, out_len);
}

uint baseconvert_targetlen(uint8_t in_max_digit, uint8_t out_max_digit, uint in_len) {
    if (in_max_digit == out_max_digit) return in_len;
    return 1 + in_len * log(in_max_digit+1)/log(out_max_digit+1) * 1.0000002;
}
