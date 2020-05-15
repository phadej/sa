#include <stdio.h>
#include <stdint.h>

/* murmurhash 32 mixer */
uint32_t mix(uint32_t h) {
	h ^= h >> 16;
	h *= 0x85ebca6b;
	h ^= h >> 13;
	h *= 0xc2b2ae35;
	h ^= h >> 16;
	return h;
}

#define N  100000
#define HN  50000.0L

/*  https://dl.acm.org/doi/pdf/10.1145/2660193.2660195?download=true */

int main() {
	long A [64][64];

	for (uint32_t n = 0; n < N; n++) {
		uint32_t v = n;
		uint32_t w = mix(v);

		for (int i = 0; i < 32; i++) {
			uint32_t x = w ^ mix(v ^ (1 << i));
			for (int j = 0; j < 32; j++) {
				if (((x >> j) & 1) != 0) {
					A[i][j] += 1;
				}
			}
		}
	}

	{
		double sumsq = 0.0;
		for (int i = 0; i < 32; i++) {
			for (int j = 0; j < 32; j++) {
				double v = (A[i][j] - HN) / HN;
				printf("%+.3lf ", 10 * v);
				sumsq += v*v;
			}
			printf("\n");
		}
		
		double result = sumsq / (32 * 32);

		printf("result %lf\n", result);
	}

	/* Another result */
	{
		double sumsq = 0.0;
		for (int i = 0; i < 32; i++) {
			for (int j = 0; j < 32; j++) {
				double v = A[i][j] / HN;
				sumsq += v*v;
			}
		}
		
		double result = sumsq / (32 * 32);

		printf("result %lf\n", result);
	}
}
