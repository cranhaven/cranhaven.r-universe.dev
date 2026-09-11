#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>
#include "myomp.h"

double const inv_thresh = 1.0 / 2.575829303548899940068; /* One percent on normal tails */

#define MAX_ANOM_TYPE 16
#define MAX_ITER 100
#define INTEG_INT 500

/**
 * @brief Empirical Likelihood
 * 
 * @param par A parameter value
 * @param dta Vector of data
 * @param K Length of the vector of data
 * @return double 
 */
static inline double EL(double par, double mx, int K) {
    double l = (double) K, p = 0.0, res = 0.0;
    uint32_t k, j = 0;
    double w = 1.0 / (double) K;

    /* Convergent loop to compute empirical likelihood */
    par = log(par);
    do {
        /* Update Lagrange multipliers */
        l -= 1;
        p -= par;
        l += w * (double) K;
        p += w * mx * (double) K;
        /* Update weights */
        w = l + p * (mx - par);
        w = (double) (w > 0.0) / ( \
            (double) (w <= 0.0) + \
            (double) (w > 0.0) * w);
    } while(j++ < MAX_ITER);
    res = 1.0;
    for (k = 0; k < K; k++)
        res *= w * (double) K;
    res *= (double) (res > 0.0);
    return res;
}

/**
 * @brief Integrate the product of EL and prior for a given hypothesis 
 * 
 * @param Hypo Binary value: 1 for regular and 0 for outlier
 * @param dta Vector of data
 * @param K Length of the vector of data
 * @param m Number of segments to perform Riemann integration
 * @return double 
 */
static inline double integ_prELik(int Hypo, double *dta, int K, int m) {
    double sm = 0.0;
    double mx = -1.0;
    double par = 0.5 * (double) Hypo + 1e-4;
    double const step = 0.9998 * 0.5 / (double) m;
    int k, i;

    if (dta && K <= MAX_ANOM_TYPE && m > 0) {
        /* Find the max "standardized" residual for the cell*/
        for (k = 0; k < K; k++) 
            mx += (double) (dta[k] > mx) * (dta[k] - mx);
        /* Initialize weights and "inverse distance" scores */
        mx = -log1p(fabs(mx));
        /* Integrate the likelihood */
        for (i = 0; i < m; i++) {
            sm += EL(par, mx, K);
            par += step;
        }
        sm /= (double) m;
        sm *= 2.0;
    }
    return sm;
}

/**
 * @brief Posterior computations over a single data entry
 * 
 * @param H_err Matrix of errors/residuals from historical analyses
 * @param R_err Matrix of errors/residuals from relational analyses
 * @param T_err Matrix of errors/residuals from distribution-tail analyses
 * @param prior_mat Matrix of prior probabilities for the regular cases (1-prior is used automatically for cellwise anomalies)
 * @param pos Position of the data entry within the database
 * @return int The highest posterior probability class
 */
static int post_calc(double *H_err, double *R_err, double *T_err, double *prior_mat, int pos) {
    int G = 0;
    double post[2];
    double x[3];
    double tmp;
    /* Transform the residuals for Empirical Likelihood computations */
    x[0] = inv_thresh * fabs(H_err[pos]);
    x[1] = inv_thresh * fabs(R_err[pos]);
    x[2] = inv_thresh * fabs(T_err[pos]);
    /* Fixing prior probabilities */
    if (isfinite(prior_mat[pos])) {
        prior_mat[pos] *= (double) (prior_mat[pos] > 0.0);
        prior_mat[pos] -= 1.0;
        prior_mat[pos] *= (double) (prior_mat[pos] < 1.0);
        prior_mat[pos] += 1.0;
        /* Penalized Empirical Likelihood Evaluation (PELE) */
        post[0] = integ_prELik(1, x, 3, INTEG_INT); /* Regular */
        post[1] = integ_prELik(0, x, 3, INTEG_INT); /* Outlier */
        /* Proportional to predictive posterior */
        post[0] *= prior_mat[pos];
        post[1] *= (1.0 - prior_mat[pos]);
        tmp = post[0] + post[1];
        tmp = tmp > 0.0 ? tmp : 1.0;
        tmp = 1.0 / tmp;
        post[0] *= tmp;
        post[1] *= tmp;
        /* Output for outliers */
        prior_mat[pos] = post[1];
        G = (int) (post[1] > post[0]); 
    }
    else {
        prior_mat[pos] = 0.0;
    }
    return G;
}

/**
 * @brief Posterior computations over the whole dataset
 * 
 * @param H_err Matrix of errors/residuals from historical analyses
 * @param R_err Matrix of errors/residuals from relational analyses
 * @param T_err Matrix of errors/residuals from distribution-tail analyses
 * @param prior_mat Matrix of prior probabilities for the regular cases 
 *                  (`1-prior` is automatically used for cellwise outliers)
 * @param G Empty matrix of integers where to store the results
 * @param dimX Vector of dimensions of the matrices listed above
 */
extern void post_results(double *prior_mat, int *G, int *dimX, double *H_err, double *R_err, double *T_err) {
    int i, j, pos;
    #if __VOPENMP
    #pragma omp parallel for default(shared) private(i, j, pos) collapse(2)
    #endif
    for (j = 0; j < dimX[1]; j++) {
        for (i = 0; i < dimX[0]; i++) {
            pos = *dimX * j + i;
            G[pos] = post_calc(H_err, R_err, T_err, prior_mat, pos);
        }
    }
}
