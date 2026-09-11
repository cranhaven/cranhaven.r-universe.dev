#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include "myomp.h"

#define RAND_MAX_PCG32 (~(uint32_t) 0)

static uint64_t const multiplier = 6364136223846793005ULL;
static uint64_t mcg_state = 0xcafef00dd15ea5e5ULL;	/* Must be odd */
#if __VOPENMP
#pragma omp threadprivate(mcg_state)
#endif

/**
@brief PCG32 for random numbers
@return uint32_t
*/
static inline uint32_t pcg32_rand(void)
{
	uint64_t x = mcg_state;
	unsigned count = (unsigned)(x >> 61);	// 61 = 64 - 3

	mcg_state = x * multiplier;
	x ^= x >> 22;
	return (uint32_t)(x >> (22 + count));	// 22 = 32 - 3 - 7
}

/**
@brief PCG32 set seed
@param seed Random seed (uint64_t)
*/
static inline void pcg32_srand(uint64_t seed)
{
	mcg_state = 2 * seed + 1;
	(void) pcg32_rand();
}
/**
 * @brief Random Dirichlet sequentially cumulated
 * @param x pointer to an empty vector
 * @param n length of the vector `x`
 */
static inline void rdiri_seq_cum(double *x, int n) {
	int i;
	uint32_t r;
	double sm = 0.0;
	double const den = 1.0 / (double) RAND_MAX_PCG32;
	if (x && n > 0)	{
		for (i = 0; i < n; i++) {
			do {
				r = pcg32_rand();
			} while(r < 1);
			x[i] = -log((double) r * den);
			sm += x[i];
		}
		/* Normalizing constant */
		sm = 1.0 / sm;
		/* Cumulate the Dirichlet random outputs */
		x[0] *= sm;
		for (i = 1; i < n; i++) {
			x[i] *= sm;
			x[i] += x[i - 1];
		}
	}
}

static int cmp_scores(const void *aa, const void *bb) {
	double a = *(double *) aa;
	double b = *(double *) bb;
	return 2 * (a >= b) - 1;
}

/**
 * @brief Computation of the empirical percentile
 * @param s pointer to a sorted vector of final scores
 */
static double emp_per(int b, double *s, int N, double theta) {
	double res = (double) nan("");
	double *w;
	double lf, rg;
	unsigned int lb = 0, ub = N - 1, mb = ub >> 1;
	pcg32_srand((((uint64_t) N ^ (uint64_t) b << 3ULL)) >> 1ULL);
	w = (double *) malloc(N * sizeof(double));
	if (w && s) {
		/* Generate from a Dirichlet_{N}(1, ..., 1) */
		rdiri_seq_cum(w, N);
		/* Check for extreme cases of theta */
		if (theta <= w[lb]) {
			res = s[lb];
		} 
		else if (theta >= w[ub]) {
			res = s[ub];
		}
		else {/* Find the $100\theta$ percentile via binary search */
			while (mb > lb) {
				lb = (w[mb] <= theta) * mb + (w[mb] > theta) * lb;
				ub = (w[mb] <= theta) * ub + (w[mb] > theta) * mb;
				mb = lb + ((ub - lb) >> 1);
			}
			/* Linear interpolation from the data */
			lf = theta - w[lb];
			rg = w[ub] - theta;
			res = s[lb] * lf + s[ub] * rg;
			lf += rg;
			res /= lf;
		}
	}
	if (w) free(w);
	return res;
}

void bayes_boot(double *th, int *B, double *s, int *nn, double *theta) {
	int const N = nn[0];
	int b;
	/* Checking for valid thresholds */
	if (!isfinite(*theta) || *theta < 0.0 || *theta > 1.0) return;
	/* Sort the data */
	qsort(s, N, sizeof(double), cmp_scores);
	/* Bootstrap loop */
	#if __VOPENMP
	#pragma omp parallel for private(b)
	#endif
	for (b = 0; b < *B; b++) {
		th[b] = emp_per(b, s, N, *theta);
	}
}
