#include <math.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <stdio.h>
#include "myomp.h"

#define MAX_ITER 2000
#define EPS_TOLL 1e-16

/**
@brief Median (based on histogram) - Considered for the "truth" below
@param x Pointer to vector of data (assumed without missing data)
@param n Lenght of `x`
@return double
*/
double median(double *x, size_t n) {
	size_t i, idx;
	size_t nbins, count = 0;
	double *wts;
	double u, v, ttwt;
	double range, cdf;

	if (n < 1) return NAN;
	if (n == 1) return *x;
	if (n == 2) return 0.5 * (x[0] + x[1]);

	/* Compute the optimal number of histogram bins */
	range = sqrt((double) n);
	ttwt = 1.0 / (double) n; /* Invert the number of samples */
	for (nbins = 1; nbins < (size_t) range; nbins <<= 1);
	/* Initialize variables to compute upper and lower bounds */
	u = *x;
	v = u;
	wts = (double *) malloc(nbins * sizeof(double));
	if (wts) {
		/* Compute range of values */
		for (i = 1; i < n; i++) { // This loop can run in parallel
			u += (double) (x[i] > u) * (x[i] - u); /* Compute the maximum */
			v += (double) (x[i] < v) * (x[i] - v);/* Compute the minimum */
		}
		do {
			range = u - v;
			range = (double)nbins / range; /* Inverse of the length of each bin */
			memset(wts, 0, nbins * sizeof(double)); /* Set the histogram counts to zero */
			for (i = 0; i < n; i++) { // This loop can run in parallel with atomic instructions
				idx = (size_t) fmax(0.0, fmin((double) nbins - 1.0, range * (x[i] - v))); /* Compute ID of the bin */
				wts[idx] += ttwt; /* Standardize the weight as a "density" */
			}
			cdf = 0.0; /* Initialize the CDF value */
			for (i = 0; cdf < 0.5 && i < nbins; i++) { // This loop is sequential
				cdf += wts[i]; /* Update the CDF in a cumulative way */
			}
			i -= (size_t)(i >= 1); /* Update the ID of the optimal bin */
			range = 1.0 / range; /* Get the length of each bin */
			v += (double) i * range; /* Set the minium of the optimal bin */
			u = v + range; /* Set the maximum of the optial bin */
		} while (range > EPS_TOLL && count++ < MAX_ITER); /* Repeat while the conditions are both true */
	}
	if (wts) free(wts);
	return v;
}

/**
@brief Normalize a vector (with NAs) based on robust location-scale stats
@param res pointer to the residual matrix
		  (assumed to be already initialized to zeros)
@param dta input dataset
@param dim size of the input dataset
@param gr  integer vector with group ids
@param g   integer id value of a specific group
@param res pointer to the residual matrix
		  (assumed to be already initialized to zeros)
*/
void group_normalize(double *res, double *dta, int *dim, int *gr, int g) {
	size_t i = 0, j = 0;
	size_t nn = (size_t) *dim;
	double m = 0.0, v = 1.0, *x;
	x = (double *) malloc(nn * sizeof(double));
	if (x) {
		while(i < nn) {
			if (!isnan(dta[i]) && gr[i] == g) {
				x[j] = dta[i];
				j++;
			}
			i++;
		}
		if (j > 0) {
			m = median(x, j);
			for (i = 0; i < j; i++) {
				x[i] -= m;
				x[i] = fabs(x[i]);
			}
			v = median(x, j);
			v = (double) (v <= 0.0) + fmax(0.0, v);
			v = 1.0 / v;
			for (i = 0; i < nn; i++) {
				res[i] += (double) (gr[i] == g) * (dta[i] - m) * v;
			}
		}
	}
	if (x) free(x);
}

/**
@brief Robust normalization of each columns by group
@param dta input dataset
@param dim size of the input dataset
@param gr  integer vector with group ids
@param ng  pointer to total number of groups
@param res pointer to the residual matrix
		  (assumed to be already initialized to zeros)
*/
void normalize(double *dta, int *dim, int *gr, int *ng, double *res) {
	int i, g;
	#if __VOPENMP
	#pragma omp parallel for default(shared) private(i, g) collapse(2)
	#endif
	for (i = 0; i < dim[1]; i++) {
		for (g = 1; g <= *ng; g++) {
			group_normalize(&res[*dim * i], &dta[*dim * i], dim, gr, g);
		}
	}
}

/**
 * @brief Scoring system for zeros and historical outliers
 * @param hScore pointer to an empty vector (for the output of historical outliers)
 * @param zScore pointer to an empty vector (for the output of zeros or negative values)
 * @param x pointer to the vector of current data
 * @param w pointer to the vector of previous data
 * @param n size of the data vector
 */
void history_check(double *hScore, double *zScore, double *x, double *w, int *n) {
	double *hdta;
	double tmp, vr = 0.0;
	int i, nn = 0;
	hdta = (double *) malloc(*n * sizeof(double));
	if (hdta) {
		#if __VOPENMP
		#pragma omp parallel for simd reduction(+ : vr, nn)
		#endif
		for (i = 0; i < *n; i++) {
			zScore[i] = (double) (x[i] > 0.0);
			hdta[i] = log(x[i] / w[i]);
			hdta[i] = isfinite(hdta[i]) ? fabs(hdta[i]) : 0.0;
			vr += hdta[i];
			nn += (int) (hdta[i] > 0.0);
		}
		vr /= (double) (nn <= 0) + (double) ((nn > 0) * nn);
		#if __VOPENMP
		#pragma omp parallel for simd private(tmp)
		#endif
		for (i = 0; i < *n; i++) {
			tmp = vr / hdta[i];
			hScore[i] = tmp < 1.0 ? tmp : 1.0;
		}
	}
	if (hdta) free(hdta);
}

/**
 * @brief Checking distribution tails of a vector (with NAs)
 *        based on robust location-scale stats
 * @param res pointer to the scoring vector
 * @param dta input vector of data
 * @param dim size of the input vector of data
 * @param gr  integer vector with group ids
 * @param g   integer id value of a specific group
 */
void group_tail(double *res, double *dta, int *dim, int *gr, int g) {
	size_t i = 0, j = 0;
	size_t nn = (size_t) *dim;
	double m = 0.0, v = 1.0, *x;
	x = (double *) malloc(nn * sizeof(double));
	if (x) {
		while(i < nn) {
			if (!isnan(dta[i]) && gr[i] == g) {
				x[j] = dta[i];
				j++;
			}
			i++;
		}
		if (j > 0) {
			m = median(x, j);
			for (i = 0; i < j; i++) {
				x[i] -= m;
				x[i] = fabs(x[i]);
			}
			v = median(x, j);
			v = (double) (v <= 0.0) + fmax(0.0, v);
			v = 1.0 / v;
			for (i = 0; i < nn; i++) {
				res[i] += (double) (gr[i] == g) * (dta[i] - m) * v;
			}
		}
	}
	if (x) free(x);
}

/**
 * @brief Checking distribution tails of each columns by group
 * @param dta input dataset
 * @param dim size of the input dataset
 * @param gr  integer vector with group ids
 * @param ng  pointer to total number of groups
 * @param tScore pointer to the scoring vector for tail outliers
 */
void tail_check(double *dta, int *dim, int *gr, int *ng, double *tScore) {
	int i, g;
	#if __VOPENMP
	#pragma omp parallel for default(shared) private(i, g) collapse(2)
	#endif
	for (i = 0; i < dim[1]; i++) {
		for (g = 1; g <= *ng; g++) {
			group_tail(&tScore[*dim * i], &dta[*dim * i], dim, gr, g);
		}
	}
	#if __VOPENMP
	#pragma omp parallel for simd
	#endif
	for (i = 0; i < dim[0] * dim[1]; i++) {
		tScore[i] = tScore[i] > 1.0 ? (1.0 / tScore[i]) : 1.0;
	}
}

/**
 * @brief Column check for relational outliers
 * @param E output matrix (scores for a given `s`)
 * @param A input matrix
 * @param dim size of input matrix
 * @param s skipping index
 */
void col_check(double *E, double *A, int *dim, int s) {
	int i, j, k;
	double tmp, v, *Q, *qty;

	Q = (double *) malloc(dim[0] * (dim[1] - 1) * sizeof(double));
	qty = (double *) malloc((dim[1] - 1) * sizeof(double));
	if (Q && qty) {
		/* Compute only the matrix Q of the QR-decomposition */
		for (i = 0; i < dim[1] - 1; i++) {
			for (j = 0; j < dim[0]; j++)
				Q[*dim * i + j] = A[*dim * (i + (i >= s))+ j];
			for (k = 0; k < i; k++) {
				tmp = 0.0;
				v = 0.0;
				for (j = 0; j < dim[0]; j++) {
					tmp += Q[*dim * k + j] * A[*dim * (i + (i >= s))+ j];
					v += Q[*dim * k + j] * Q[*dim * k + j];
				}
				tmp /= v;
				for (j = 0; j < dim[0]; j++)
					Q[*dim * i + j] -= tmp * Q[*dim * k + j];
			}
			/* Normalization of the column vector */
			tmp = 0.0;
			for (j = 0; j < dim[0]; j++) {
				v = Q[*dim * i + j];
				tmp += v * v;
			}
			tmp = 1.0 / sqrt(tmp);
			for (j = 0; j < dim[0]; j++)
				Q[*dim * i + j] *= tmp;
		}
		/* Computing Q^t y */
		for (i = 0; i < dim[1] - 1; i++) {
			tmp = 0.0;
			for (j = 0; j < dim[0]; j++)
				tmp += Q[*dim * i + j] * A[*dim * s + j];
			qty[i] = tmp;
		}
		/* Computing the residuals (i.e. $y - Q(Q^t y)$) */
		v = 0.0;
		for (j = 0; j < dim[0]; j++) {
			tmp = 0.0;
			for (i = 0; i < dim[1] - 1; i++)
				tmp += Q[*dim * i + j] * qty[i];
			E[*dim * s + j] = fabs(A[*dim * s + j] - tmp); 
			v += E[*dim * s + j] * E[*dim * s + j];
		}
		v = sqrt(v / (double) (dim[0] - 1));
		for (j = 0; j < dim[0]; j++) {
			tmp = v / E[*dim * s + j];
			tmp = isfinite(tmp) ? tmp : 1.0;
			E[*dim * s + j] = tmp >= 1.0 ? 1.0 : tmp;
		}
	}
	if (Q) free(Q);
	if (qty) free(qty);
}

/**
 * @brief Checking for relational outliers
 * @param A input matrix
 * @param dim size of input matrix
 */
void relat_check(double *A, int *dim) { /** FIXME: introduce new pointer in input */
	int i;
	double *E;

	E = (double *) malloc(dim[0] * dim[1] * sizeof(double));
	if (E) {
		#if __VOPENMP
		#pragma omp parallel for default(shared) private(i)
		#endif
		for (i = 0; i < dim[1]; i++) {
			col_check(E, A, dim, i); /** FIXME: introduce new pointer in input */
		}
		#if __VOPENMP
		#pragma omp parallel for simd
		#endif
		for (i = 0; i < dim[0] * dim[1]; i++) {
			A[i] = E[i];
		}
	}
	if (E) free(E);
}

/****************************************************/
/* Residual functions for Bayesian analyses (below) */
/****************************************************/

/**
 * @brief Residual system for historical outliers and zeros scores
 * @param hRes pointer to an empty vector (for the output of historical residuals)
 * @param zScore pointer to an empty vector (for the output of zeros or negative values)
 * @param x pointer to the vector of current data
 * @param w pointer to the vector of previous data
 * @param n size of the data vector
 */
void history_res(double *hRes, double *zScore, double *x, double *w, int *n) {
	double tmp, vr = 0.0;
	int i, nn = 0;
	#if __VOPENMP
	#pragma omp parallel for simd private(tmp) reduction(+ : vr, nn)
	#endif
	for (i = 0; i < *n; i++) {
		zScore[i] = (double) (x[i] > 0.0);
		hRes[i] = log(x[i] / w[i]);
		hRes[i] = isfinite(hRes[i]) ? hRes[i] : 0.0;
		tmp = fabs(hRes[i]);
		vr += tmp;
		nn += (int) (tmp > 0.0);
	}
	vr /= (double) (nn <= 0) + (double) ((nn > 0) * nn);
	vr = (double) (vr <= 0.0) + (double) (vr > 0.0) * vr;
	vr = 1.0 / vr;
	#if __VOPENMP
	#pragma omp parallel for simd
	#endif
	for (i = 0; i < *n; i++) {
		hRes[i] *= vr;
	}
}

/**
 * @brief Computing the residuals for checking distribution tails of each columns by group
 * @param dta input dataset
 * @param dim size of the input dataset
 * @param gr  integer vector with group ids
 * @param ng  pointer to total number of groups
 * @param tRes pointer to the residual vector for tail outliers
 */
void tail_res(double *dta, int *dim, int *gr, int *ng, double *tRes) {
	int i, g;
	#if __VOPENMP
	#pragma omp parallel for default(shared) private(i, g) collapse(2)
	#endif
	for (i = 0; i < dim[1]; i++) {
		for (g = 1; g <= *ng; g++) {
			group_tail(&tRes[*dim * i], &dta[*dim * i], dim, gr, g);
		}
	}
}

/**
 * @brief Column residuals to check for relational outliers
 * @param E output matrix (scores for a given `s`)
 * @param A input matrix
 * @param dim size of input matrix
 * @param s skipping index
 */
void col_res(double *E, double *A, int *dim, int s) { /** FIXME: introduce new pointer in input */
	int i, j, k;
	double tmp, v, *Q, *qty;

	Q = (double *) malloc(dim[0] * (dim[1] - 1) * sizeof(double));
	qty = (double *) malloc((dim[1] - 1) * sizeof(double));
	if (Q && qty) {
		/* Compute only the matrix Q of the QR-decomposition */
		for (i = 0; i < dim[1] - 1; i++) {
			for (j = 0; j < dim[0]; j++)
				Q[*dim * i + j] = A[*dim * (i + (i >= s))+ j];
			for (k = 0; k < i; k++) {
				tmp = 0.0;
				v = 0.0;
				for (j = 0; j < dim[0]; j++) {
					tmp += Q[*dim * k + j] * A[*dim * (i + (i >= s))+ j];
					v += Q[*dim * k + j] * Q[*dim * k + j];
				}
				v = (double) (v <= 0.0) + (double) (v > 0.0) * v;
				tmp /= v;
				for (j = 0; j < dim[0]; j++)
					Q[*dim * i + j] -= tmp * Q[*dim * k + j];
			}
			/* Normalization of the column vector */
			tmp = 0.0;
			for (j = 0; j < dim[0]; j++) {
				v = Q[*dim * i + j];
				tmp += v * v;
			}
			tmp = (double) (tmp <= 0.0) + (double) (tmp > 0.0) * tmp;
			tmp = 1.0 / sqrt(tmp);
			for (j = 0; j < dim[0]; j++)
				Q[*dim * i + j] *= tmp;
		}
		/* Computing Q^t y */
		for (i = 0; i < dim[1] - 1; i++) {
			tmp = 0.0;
			for (j = 0; j < dim[0]; j++)
				tmp += Q[*dim * i + j] * A[*dim * s + j];
			qty[i] = tmp;
		}
		/* Computing the residuals (i.e. $y - Q(Q^t y)$) */
		v = 0.0;
		for (j = 0; j < dim[0]; j++) {
			tmp = 0.0;
			for (i = 0; i < dim[1] - 1; i++)
				tmp += Q[*dim * i + j] * qty[i];
			E[*dim * s + j] = fabs(A[*dim * s + j] - tmp);
			v += E[*dim * s + j] * E[*dim * s + j];
		}
		v = (double) (v <= 0.0) + (double) (v > 0.0) * v;
		v = sqrt(v / (double) ((dim[0] <= 1) + (dim[0] > 1) * (dim[0] - 1)));
		v = 1.0 / v;
		for (j = 0; j < dim[0]; j++) {
			E[*dim * s + j] *= v;
		}
	}
	if (Q) free(Q);
	if (qty) free(qty);
}

/**
 * @brief Residuals from linear models to check relational outliers
 * @param A input matrix
 * @param dim size of input matrix
 */
void relat_res(double *A, int *dim) {
	int i;
	double *E;

	E = (double *) malloc(dim[0] * dim[1] * sizeof(double));
	if (E) {
		#if __VOPENMP
		#pragma omp parallel for default(shared) private(i)
		#endif
		for (i = 0; i < dim[1]; i++) {
			col_res(E, A, dim, i);
		}
		#if __VOPENMP
		#pragma omp parallel for simd
		#endif
		for (i = 0; i < dim[0] * dim[1]; i++) {
			A[i] = E[i];
		}
	}
	if (E) free(E);
}
