#include <math.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <stdio.h>
#include "myomp.h"

#define EPS_TOLL 1e-10
#define RHO .9
#define EPSILON 1e-5
#define N_ITERATIONS 10

#define LEARNING_RATE 0.001
#define BETA_1 0.9
#define BETA_2 0.999
#define FACTOR_P 0.01

static int N_EPOCHS = 1000;
static char BAYES = 0;

void post_results(double *, int *, int *, double *, double *, double *);

/**
* A `mydata_str` is a structure with the following properties:
* @property {double *} E - A pointer to the matrix of residuals
* @property {double *) A - A pointer to the matrix of data
* @property {int} dimA - A pointer to the dimensions of the matrix `A`
*/
typedef struct {
      double *E;
      double *A;
      int *dimA;
} mydata_str;

static int cmp_dbl(const void *aa, const void *bb) {
    double a = *(double *) aa;
    double b = *(double *) bb;
    if (!isfinite(a) && isfinite(b)) return 1;
    if (isfinite(a) && !isfinite(b)) return -1;
    if (isfinite(a) && isfinite(b)) return 2 * (a > b) - 1;
    return 0;
}

static inline double median_wna(double *x, int n) {
    int i, nf = 0;
    double m = 0.0;
    double *y = (double *) malloc(n * sizeof(double));
    if (y) {
        for (i = 0; i < n; i++) {
            y[i] = x[i];
            nf += isfinite(x[i]);
        }
        if (nf > 0) {
            qsort(y, n, sizeof(double), cmp_dbl);
            i = nf >> 1;
            m = y[i];
            if (!(nf & 1) && i > 0) {/* Adjust if even */
                m += y[i - 1];
                m *= 0.5;
            }
        }
    }
    if (y) free(y);
    return m;
}

/**
* @brief Generic scoring function for tail and historical outliers
*
* @param scores vector of real numbers where to store the results of this algorithm
* @param dta matrix of (unsorted) data
* @param n number of records
* @param p number of variables
* @param zeroMedian {0, 1} flag, if zero computes the median, otherwise assumes median equal to zero
* @param dataUpdate {0, 1} flag, if zero does not update the data, otherwise update `dta` with "standardized residuals"
*/
static inline void scoring_tails(double *scores, double *dta, int n, int p, char zeroMedian, char dataUpdate) {
    int i, j, nm;
    double mae, tmp, m;
    #if __VOPENMP
    #pragma omp parallel for private(i, j, mae, nm, tmp, m)
    #endif
    for (j = 0; j < p; j++) {
        /* Compute the median */
        if (zeroMedian) {
            m = 0.0;
        }
        else {
            m = median_wna(&dta[n * j], n);
        }
        mae = 0.0;
        nm = 0;
        /* Compute sufficient stats for MAE */
        for (i = 0; i < n; i++) {
            tmp = dta[n * j + i] - m;
            if (isfinite(tmp)) {
                mae += fabs(tmp);
                nm++;
            }
        }
        /* Invert MAE */
        if (mae > 0.0 && nm > 0) {
            mae = (double) nm / mae;
        }
        else {
            mae = 1.0;
        }
        for (i = 0; i < n; i++) {
            /* Normalization*/
            tmp = (dta[n * j + i] - m) * mae;
            if (dataUpdate && isfinite(tmp)) dta[n * j + i] = tmp;
            /* Compute the scores */
            tmp = fabs(tmp);
            if (BAYES) {
                scores[n * j + i] = tmp;
            }
            else {
                if (tmp > 1.0) {
                    scores[n * j + i] = 1.0 / tmp;
                }
                else {
                    scores[n * j + i] = 1.0;
                }
            }
        }
    }
}

 
/**
* @brief Scoring system for zeros (i.e.,
* checking consistency with data format)
*
* @param zScore pointer to an empty vector (for the output of zeros or negative values)
* @param x pointer to the vector of current data (unsorted)
* @param len length of the vectors in input to this function
*/
static inline void format_check(double *zScore, double *x, int *len) {
      int i;
      int const n = *len;
      #if __VOPENMP
      #pragma omp parallel for private(i)
      #endif
      for (i = 0; i < n; i++) {
           zScore[i] = (double) (x[i] > 0.0); /* Scoring for no-sense data */
      }
}

/**
* @brief Scoring system for zeros and historical outliers
*
* @param hScore pointer to an empty matrix (for the output of historical outliers)
* @param x pointer to the matrix of current data (unsorted)
* @param w pointer to the matrix of previous data (unsorted)
* @param n number of records
* @param p number of variables
*/
static inline void history_check(double *hScore, double *x, double *w, int n, int p) {
    int i;
    #if __VOPENMP
    #pragma omp parallel for private(i)
    #endif
    for (i = 0; i < n * p; i++) {
        w[i] = fabs(log(x[i] / w[i])); /* Computing historical log-differences */
        if (!isfinite(w[i])) w[i] = 0.0; /* Fixing NAs or infinite values */
    }
    scoring_tails(hScore, w, n, p, 1, 0);
}
 
/**
* @brief Scoring system based on tail-outlier identification
*
* @param tScore matrix of real numbers where to store the results of this algorithm
* @param dta matrix of data (unsorted... they will be standardized at the end of this routine)
* @param n number of records
* @param p number of variables
*/
static inline void tail_check(double *tScore, double *dta, int n, int p) {
      int i;
      #if __VOPENMP
      #pragma omp parallel for private(i)
      #endif
      for (i = 0; i < n * p; i++) {
           dta[i] = log(dta[i]);
      }
      scoring_tails(tScore, dta, n, p, 0, 1);
}

/**
* @brief Double GEneral Matrix Multiplication
*
* @param res empty array where to store A %*% B (column-major format)
* @param A input matrix (column-major format) on the left of the product
* @param dimA number of rows and columns of the matrix A
* @param B input matrix (column-major format) on the right of the product
* @param dimB number of rows and columns of the matrix B
*/
static inline void dgemm(double *res, double *A, int *dimA, double *B, int *dimB) {
    int i, j, k;
    int b, c;
    if (dimA[1] == dimB[0]) {
        #if __VOPENMP
        #pragma omp parallel for private(i, j, k, b, c) collapse(2)
        #endif
        for (i = 0; i < dimA[0]; i++) {
            for (j = 0; j < dimB[1]; j++) {
                b = *dimB * j;
                c = *dimA * j + i;
                res[c] = 0.0;
                for (k = 0; k < dimA[1]; k++) {
                    if (isfinite(A[i + *dimA * k]) && isfinite(B[b + k]))
                        res[c] += A[i + *dimA * k] * B[b + k];
                }
            }
        }
    } 
}
 
/**
* @brief Normalize the matrix by variable
*
* @param E input matrix of data to normalize
* @param dim array containing the number of rows and columns
*/
static inline void normalize(double *E, int *dim) {
    int i, j;
    double mae;
    int nm;
    double tmp;
    #if __VOPENMP
    #pragma omp parallel for private(i, j, mae, nm, tmp)
    #endif
    for (j = 0; j < dim[1]; j++) {
        mae = 0.0;
        nm = 0;
        /* Compute the mean absolute error (MAE) */
        for (i = 0; i < *dim; i++) {
            tmp = E[*dim * j + i];
            if (isfinite(tmp)) {
                mae += fabs(tmp);
                nm++;
            }
        }
        /* Normalization */
        if (mae > 0.0 && nm > 0) {
            mae = (double) nm / mae;           
            for (i = 0; i < *dim; i++) {
                E[*dim * j + i] *= mae;
            }
        }
    }
} 

/**
* @brief Populate a parametric matrix for a linear decomposition of the data matrix
*
* @param mat an zero/empty (p x p) matrix (column-major format)
* @param dta a (n x p) matrix of data (stored by column, i.e. column-major format)
* @param dim an integer array storing the number of data points (n) and number of variables (p)
*/
static inline void init_param(double *mat, double *dta, int *dim) {
    int i, j, k, n;
    double tmp, x, y;
    int const p = dim[1];
    #if __VOPENMP
    #pragma omp parallel for private(i, j, k, n, tmp, x, y) collapse(2)
    #endif
    for (i = 0; i < p; i++) {
        for (j = 0; j < p; j++) {
            if (j > i) {
                n = 0;
                tmp = 0.0;
                for (k = 0; k < *dim; k++) {
                    x = dta[*dim * i + k];
                    y = dta[*dim * j + k];
                    if (isfinite(x) && isfinite(y)) {
                        x -= (double) (x > 1.0) * (x - 1.0);
                        x -= (double) (x < -1.0) * (x + 1.0);
                        y -= (double) (y > 1.0) * (y - 1.0);
                        y -= (double) (y < -1.0) * (y + 1.0);
                        tmp += x * y;
                        n++;
                    }
                }
                if (n > 0) mat[p * j + i] = tmp / (double) n;
                mat[p * i + j] = mat[p * j + i];
            }
        }
    }
    #if __VOPENMP
    #pragma omp parallel for
    #endif
    for (i = 0; i < p; i++) mat[p * i + i] = 0.0;
} 

/**
* @brief Computing the model residuals based on a linear algebra approach
*
* @param res Pointer to an empty matrix (to be populated with residuals)
* @param A Pointer to a matrix of data
* @param dimA Pointer to a vector of dimension for the matrix `A`
* @param R Pointer to a matrix of parameters
* @param dimR Pointer to a vector of dimension for the matrix `R`
*/
static inline void residuals(double *res, double *A, int *dimA, double *R, int *dimR) {
    int i, j, pos;
    dgemm(res, A, dimA, R, dimR); /* Matrix multiplication `A %*% R`*/
    #if __VOPENMP
    #pragma omp parallel for private(i, j, pos) collapse(2)
    #endif
    for (j = 0; j < dimA[1]; j++) {
        for (i = 0; i < *dimA; i++) {
            pos = *dimA * j + i;
            if (isfinite(res[pos]) && isfinite(A[pos]))
                res[pos] -= A[pos];
        }
    }
} 

/**
* @brief Compute the matrix-valued gradient of a robust objective function
*
* @param grd_v Pointer to the gradient matrix
* @param param Pointer to the parameter matrix
* @param len Pointer to the length of `param` and `grd_v`
* @param info Pointer to a structured data (passed as a void)
*/
static void mat_val_grad(double *grd_v, double *param, int *len, void *info) {
    mydata_str dta = *(mydata_str *) info;
    int const n = dta.dimA[0];
    int const p = dta.dimA[1];
    int i, j, k;
    int dimR[2];
    double tmp, slp;

    /* Computing the residuals */
    dimR[0] = dimR[1] = p;
    residuals(dta.E, dta.A, dta.dimA, param, dimR);
    memset(grd_v, 0, *len * sizeof(double));
    /* Computing the gradient */
    #if __VOPENMP
    #pragma omp parallel for private(i, j, k, tmp, slp) collapse(2)
    #endif
    for (i = 0; i < p; i++) {
        for (j = 0; j < p; j++) {
            slp = 0.0;
            if (i != j) for (k = 0; k < n; k++) {
                tmp = dta.E[n * i + k];
                tmp = (double) (tmp > 0.0) - (double) (tmp < 0.0);
                if (isfinite(dta.A[n * j + k]))
                    slp += dta.A[n * j + k] * tmp;
            } /* This assure that the diagonal is zero (by assumption/constraint) */
            grd_v[p * i + j] = slp;
        }
    }
    #if _VOPENMP
    #pragma omp parallel for
    #endif
    for (i = 0; i < p; i++) grd_v[(p + 1) * i] = 0.0;
}

/**
* It computes the gradient of the objective function, updates the momentum and the second order
* momentum, and then updates the parameters using the Lion Algorithm
*
* @param param the parameters to be optimized
* @param len the length of the parameter vector
* @param n_iter number of iterations 0.05, 10L)

* @param info a pointer to a structure that contains the data and other information
* @param grad a routine that computes the gradient of the objective function
*/
static inline void lion(double *param, int *len, int *n_iter, void *info,
          void (*grad)(double *, double *, int *, void *)) {
    int t, i, np = *len;
    double *grd_v;
    double *mom_m;
    double sgn;

    grd_v = (double *) malloc(np * sizeof(double));
    mom_m = (double *) calloc(np, sizeof(double));
    if (mom_m && grd_v) {
        for (t = 0; t < *n_iter; t++) {
            /* Update the gradient */
            (*grad)(grd_v, param, len, info);
            #if __VOPENMP
            #pragma omp parallel for simd private(sgn)
            #endif
            for (i = 0; i < np; i++) {
                /* Lion update of custom momentum */
                sgn = BETA_1 * mom_m[i] + (1.0 - BETA_1) * grd_v[i];
                sgn = (double) (sgn > 0.0) - (double) (sgn < 0.0);
                /* Update the momentum */
                mom_m[i] *= BETA_2;
                mom_m[i] += (1.0 - BETA_2) * grd_v[i];
                /* Computing the step */
                grd_v[i] = sgn + FACTOR_P * param[i];
                grd_v[i] *= LEARNING_RATE;
                param[i] -= grd_v[i];
            }
        }
    }
    if (mom_m) free(mom_m);
    if (grd_v) free(grd_v);
} 

/**
* @brief  Scoring system to identify relational outliers -- (interfaced with R, python...)
*
* @param A input matrix of data (including NAs)... also used to store the output
* @param dim array containing the number of rows and columns
*/
static inline void relat_check(double *rScore, double *A, int *dim) {
    mydata_str mydata;
    double *E, *R;
    double tmp;
    int i, lenR, dimR[2];
    int n_iter = N_ITERATIONS;

    E = (double *) malloc(dim[0] * dim[1] * sizeof(double));
    R = (double *) calloc(dim[1] * dim[1], sizeof(double));
    if (E && R) {
        mydata.E = E;
        mydata.A = A;
        mydata.dimA = dim;
        dimR[0] = dimR[1] = dim[1];
        lenR = dim[1] * dim[1];
    	/* Computes robust regression coefficients */
        init_param(R, A, dim);
        for (i = 0; i < N_EPOCHS; i++)
            lion(R, &lenR, &n_iter, (void *) &mydata, mat_val_grad);
    	/* Residual standardization */
	residuals(E, A, dim, R, dimR);
        normalize(E, dim);
        /* Compute the relational score */
        if (BAYES) {
            #if __VOPENMP
            #pragma omp parallel for private(i)
            #endif
            for (i = 0; i < dim[0] * dim[1]; i++) {
                rScore[i] = fabs(E[i]);
            }
        }
        else {
            #if __VOPENMP
            #pragma omp parallel for private(i, tmp)
            #endif
            for (i = 0; i < dim[0] * dim[1]; i++) {
                tmp = fabs(E[i]);
                rScore[i] = tmp > 1.0 ? 1.0 / tmp : 1.0;
            }
        }
    }
    if (E) free(E);
    if (R) free(R);
}
 
/**
* @brief Cellwise anomaly for state-level wide datasets
*
* @param s Pointer to an empty vector where to store the final scores
* @param z Pointer to an empty vector for data format anomaly scores
* @param h Pointer to an empty vector for historical anomaly scores
* @param r Pointer to an empty vector for relational anomaly scores
* @param t Pointer to an empty vector for tail anomaly scores
* @param Xc Pointer to a matrix of current data
* @param Xp Pointer to a matrix of previously reported data
* @param dimX Pointer to a vector with the dimensions of `Xc` and `Xp`
*/
extern void cellwise(double *s, double *z, double *h, double *r, double *t, double *Xc, double *Xp, int *dimX, int *epochs) {
    int i;
    int len = dimX[0] * dimX[1];
    N_EPOCHS = *epochs;
    BAYES = 0;
    format_check(z, Xc, &len);
    history_check(h, Xc, Xp, dimX[0], dimX[1]);
    tail_check(t, Xc, dimX[0], dimX[1]);
    relat_check(r, Xc, dimX);
    /* Combining the four scores using the product t-norm */
    #if __VOPENMP
    #pragma omp parallel for private(i)
    #endif
    for (i = 0; i < len; i++) {
        s[i] = z[i] * h[i] * r[i] * t[i];
    }
}

/**
* @brief Cellwise anomaly for state-level wide datasets using Bayesian testing
*
* @param s Pointer to an empty vector where to store the final scores
* @param z Pointer to an empty vector for data format anomaly scores
* @param h Pointer to an empty vector for historical anomaly scores
* @param r Pointer to an empty vector for relational anomaly scores
* @param t Pointer to an empty vector for tail anomaly scores
* @param Xc Pointer to a matrix of current data
* @param Xp Pointer to a matrix of previously reported data
* @param dimX Pointer to a vector with the dimensions of `Xc` and `Xp`
*/
#ifndef MY_TEST
extern void bayeswise(double *s, int *G, double *z, double *h, double *r, double *t, double *Xc, double *Xp, int *dimX, int *epochs) {
    int i;
    int len = dimX[0] * dimX[1];
    N_EPOCHS = *epochs;
    BAYES = 1;
    format_check(z, Xc, &len);
    history_check(h, Xc, Xp, dimX[0], dimX[1]);
    tail_check(t, Xc, dimX[0], dimX[1]);
    relat_check(r, Xc, dimX);
    /* Combining the four scores using the Bayesian testing */
    #if __VOPENMP
    #pragma omp parallel for private(i)
    #endif
    for (i = 0; i < len; i++) {
        s[i] *= z[i];
    }
    post_results(s, G, dimX, h, r, t);
}
#endif

#ifdef MY_TEST
int main() {
    int i, j, n;
    int dim4[] = {4, 7};
    int dim7[] = {7, 4};
    double a,  b, c;
/* Non-sense values */
    double x[] = {nan(""), 2.0, -4.0, 0.5, -1.0, nan(""), 0.0, \
                  -1.0, nan(""), 4.0, 0.5, nan(""), -1.2, 0.0, \
                  1.0, -1.3, -4.0, 0.5, 0.9, -1.2, 0.0, \
                  -1.1, 2.3, nan(""), 0.5, -5.0, 1.2, 0.0 };
    /* I cannot remember what w[] stands for */
    double w[] = {nan(""), nan(""), -1.0, 0.7, -0.7, nan(""), 0.3, \
                  0.75, nan(""), nan(""), 0.35, nan(""), -6.4, 2.0, \
                  0.0, -0.3, 14.0, -0.15, nan(""), nan(""), 20.0, \
                  -1.57, 111.3, nan(""), 0.4, nan(""), -0.2, 1.0 };
    double B[16] = {0.0};
    double G[16] = {0.0};
    double s[28];
    double z[28] = {0.0}, h[28] = {0.0}, r[28] = {0.0}, t[28] = {0.0};
    mydata_str info;
    a = 1.0;
    b = 2.0;
    c = 1.0;
    printf("Testing \e[1;31mcmp_dbl\e[0m:\n");
    printf("a >= b ? %d\n", cmp_dbl((void *) &a, (void *) &b));
    printf("a >= c ? %d\n", cmp_dbl((void *) &a, (void *) &c));
    printf("b >= c ? %d\n", cmp_dbl((void *) &b, (void *) &c));
    printf("\n");
    printf("Testing \e[1;31mmedian_wna\e[0m:\n");
    printf("Median: %f", median_wna(x, 7));
    printf("\n");
    printf("Testing \e[1;31mscoring_tain with median and no data update\e[0m:\n");
    memset(s, 0, sizeof(double) * 28);
    scoring_tails(s, x, 7, 4, 0, 0); /* Scores with median and no data update */
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 7; j++) {
            printf("%.2f ", s[i * 7 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Testing \e[1;31mscoring_tain with zero median and no data update\e[0m:\n");
    memset(s, 0, sizeof(double) * 28);
    scoring_tails(s, x, 7, 4, 1, 0); /* Scores without median and no data update */
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 7; j++) {
            printf("%.2f ", s[i * 7 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Testing \e[1;31mscoring_tain with median and data update\e[0m:\n");
    memset(s, 0, sizeof(double) * 28);
    scoring_tails(s, x, 7, 4, 0, 1); /* Scores with median and no data update */
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 7; j++) {
            printf("%.2f ", s[i * 7 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Testing \e[1;31mformat_check\e[0m:\n");
    memset(s, 0, sizeof(double) * 28);
    i = 28;
    format_check(s, x, &i); /* Scores for format check */
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 7; j++) {
            printf("%.2f ", s[i * 7 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Testing \e[1;31mhistory_check\e[0m:\n");
    memset(s, 0, sizeof(double) * 28);
    i = 28;
    history_check(s, x, w, 7, 4); /* Scores for history check */
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 7; j++) {
            printf("%.2f ", s[i * 7 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Testing \e[1;31mtail_check\e[0m:\n");
    memset(s, 0, sizeof(double) * 28);
    i = 28;
    tail_check(s, x, 7, 4); /* Scores for tail check */
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 7; j++) {
            printf("%.2f ", s[i * 7 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Testing \e[1;31mdgemm\e[0m:\n");
    dgemm(B, x, dim4, w, dim7);
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 4; j++) {
            printf("%.2f ", B[i * 4 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Testing \e[1;31mnormalize\e[0m:\n");
    normalize(w, dim7);
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 7; j++) {
            printf("%.2f ", w[i * 7 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Testing \e[1;31minit_param\e[0m:\n");
    init_param(B, w, dim7);
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 4; j++) {
            printf("%.2f ", B[i * 4 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Testing \e[1;31mresiduals\e[0m:\n");
    dim4[1] = 4;
    residuals(w, x, dim7, B, dim4);
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 7; j++) {
            printf("%.2f ", w[i * 7 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Testing \e[1;31mmat_val_grad\e[0m:\n");
    info.A = x;
    info.dimA = dim7;
    info.E = w;
    n = 16;
    mat_val_grad(G, B, &n, (void *) &info);
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 4; j++) {
            printf("%.2f ", G[i * 4 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Testing \e[1;31mlion\e[0m:\n");
    i = 10;
    lion(B, &n, &i, (void *) &info, mat_val_grad);
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 4; j++) {
            printf("%.2f ", B[i * 4 + j]);
        }
        printf("\n");
    }
    printf("\n");
    N_EPOCHS = 5;
    printf("Testing \e[1;31mrelat_check\e[0m:\n");
    memset(s, 0, sizeof(double) * 28);
    relat_check(s, x, dim7);
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 7; j++) {
            printf("%.2f ", w[i * 7 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Testing \e[1;31mcellwise\e[0m:\n");
    memset(s, 0, sizeof(double) * 28);
    int maxepoch = 10;
    cellwise(s, z, h, r, t, x, w, dim7, &maxepoch);
    for (i = 0; i < 4; i++) {
        for (j = 0; j < 7; j++) {
            printf("%.2f ", s[i * 7 + j]);
        }
        printf("\n");
    }
    printf("\n");
    printf("Test ended successfully\n\n");
    return 0;
}
#endif

/*
library(HRTnomaly)
data(toy)
res <- fuzzyHRT(toy)
bes <- bayesHRT(toy)
ces <- cellwise(toy, 0.05, 100L)
reb <- bayeswise(toy, 0.5, 100L)

cat("\n\nFuzzyHRT:\n")
class_check(res$outlier, res$anomaly_flag!="")
cat("\n\nBayesHRT:\n")
class_check(bes$outlier, bes$anomaly_flag!="")
cat("\n\nCellwise:\n")
class_check(ces$outlier, ces$anomaly_flag!="")
cat("\n\nBayeswise:\n")
class_check(reb$outlier, reb$anomaly_flag!="")
*/
