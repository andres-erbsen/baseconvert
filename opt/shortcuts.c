#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>

#ifdef __GNUC__
	#define clz __builtin_clz
#endif

int magic(int a, int b) {
	// Think of it as of euclid's algorithmm for expenents
	if (a < 2 || b < 2) return 0;
	int r;
	while (1) {
		if (b > a) {
			r = a;
			a = b;
			b = r;
		}
		r = a%b;
		a /= b;
		if (r) return 0;
		else if (a == 1) return b;
	}
}

int ipow(int a, int b) { // a^b in O(bits(int)). Square and multiply.
	int ret = 1;
	while (b > 0) {
		if (b & 1) ret *= a;
		a *= a;
		b >>= 1;
	}
	return ret;
}

int ilog(int a, int b) { // ceil(log_b(a)) in O(bits(int)^2).
	assert( a > 0 && b > 1);
	if (a == 1) return 0;
	if (a <= b) return 1;
	#define bits(x) (8*sizeof(x))
	int l2a = bits(int) - clz(a);
	int l2b = bits(int) - clz(b);
	#undef bits
	int right = (l2a-1)/(l2b-1)+1;
	int left = l2a/l2b-1;
	while (left < right) {
		int mid = left + (right - left) / 2;
		if (ipow(b,mid) < a) left = mid + 1;
		else right = mid;
	}
	return left;
}

int main() {
	int a, b,g;
	for (a = 2; a <= 256; a++) for (b = 2; b < a; b++) {
		g = magic(a,b);
		if (g > 0) printf("%d^%d == %d^%d\n",a,ilog(b,g),b,ilog(a,g));
	}
	return;
}
