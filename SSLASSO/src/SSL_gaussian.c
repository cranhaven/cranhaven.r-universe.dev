#include <math.h>
#include <string.h>
#include "Rinternals.h"
#include "R_ext/Rdynload.h"
#include <R.h>
#include <R_ext/Applic.h>

double crossprod(double *X, double *y, int n, int j);
double sum(double *x, int n);
double update_sigma2(double *r, int n);
int checkConvergence(double *beta, double *beta_old, double eps, int l, int p);
double SSL(double z, double beta, double lambda0, double lambda1, double theta, double v, int n, double delta, double sigma2);
double pstar(double x, double theta, double lambda1, double lambda0);
double lambdastar(double x, double theta, double lambda1, double lambda0);
double expectation_approx(double *beta, double a, double b, int p, int l);
double threshold(double theta, double sigma2, double lambda1, double lambda0, int n);

// Memory handling, output formatting (Gaussian)

SEXP cleanupG(double *a, double *r, int *e1, int *e2, double *z, SEXP beta, SEXP loss, SEXP iter, SEXP thetas_export, SEXP sigmas_export) {

  Free(a);
  Free(r);
  Free(e1);
  Free(e2);
  Free(z);

  SEXP res;
  PROTECT(res = allocVector(VECSXP, 5));
  SET_VECTOR_ELT(res, 0, beta);
  SET_VECTOR_ELT(res, 1, loss);
  SET_VECTOR_ELT(res, 2, iter);
  SET_VECTOR_ELT(res, 3, thetas_export);
  SET_VECTOR_ELT(res, 4, sigmas_export);

  UNPROTECT(1);

  return(res);
}

// Gaussian loss
double gLoss(double *r, int n) {
  double l = 0;
  for (int i = 0; i < n; i++) l = l + pow(r[i], 2);
    return(l);
}

// Coordinate descent for Gaussian models

SEXP SSL_gaussian(SEXP X_, SEXP y_, SEXP penalty_, SEXP variance_, SEXP lambda1_, SEXP lambda0s_, SEXP theta_, SEXP sigma_, SEXP min_sigma2_, SEXP a_,  SEXP b_, SEXP eps_, SEXP max_iter_, SEXP counter_) {

  // Declarations
  int n = length(y_);
  int p = length(X_)/n;
  int L = length(lambda0s_);

  double *X = REAL(X_);
  double *y = REAL(y_);

  const char *penalty = CHAR(STRING_ELT(penalty_, 0));
  const char *variance = CHAR(STRING_ELT(variance_, 0));

  double lambda1 =REAL(lambda1_)[0];
  double *lambda0s = REAL(lambda0s_);
  double lambda0;
  double theta = REAL(theta_)[0];
  double sigma2 = pow(REAL(sigma_)[0], 2);
  double sigma2_init = pow(REAL(sigma_)[0], 2);
  double min_sigma2 = REAL(min_sigma2_)[0];
  double aa = REAL(a_)[0];
  double bb = REAL(b_)[0];
  double eps = REAL(eps_)[0];
  int max_iter = INTEGER(max_iter_)[0];
  int count_max = INTEGER(counter_)[0];

  // create containers for R output
  SEXP res, beta, loss, iter, thetas_export, sigmas_export;

  PROTECT(beta = allocVector(REALSXP, L*p));
  double *b = REAL(beta);
  for (int j=0; j< (L*p); j++) {
    b[j] = 0;
  }

  PROTECT(thetas_export = allocVector(REALSXP, L));
  double *thetas = REAL(thetas_export);

  PROTECT(sigmas_export = allocVector(REALSXP, L));
  double *sigmas = REAL(sigmas_export);

  for(int l = 0; l < L; l++) {
    thetas[l] = NAN;
    sigmas[l] = NAN;
  }

  PROTECT(loss = allocVector(REALSXP, L));
  PROTECT(iter = allocVector(INTSXP, L));

  for (int i = 0; i < L; i++) { 
    INTEGER(iter)[i] = 0;
  }

  double delta = 0;

  // Beta from previous iteration
  double *a = Calloc(p, double); 

  for (int j = 0; j < p; j++) {
    a[j] = 0;
  }

  // Residuals
  double *r = Calloc(n, double);
  
  for (int i=0; i<n; i++) {
    r[i] = y[i];
  }

  double *z = Calloc(p, double);

  for (int j=0; j<p; j++) {
    z[j] = crossprod(X, r, n, j);
  }

  // Index of an active set
  int *e1 = Calloc(p, int); 

  for (int j=0; j<p; j++) {
    e1[j] = 0;
  }

  // Index of an eligible set from the strong rule
  int *e2 = Calloc(p, int); 

  for (int j=0; j<p; j++) {
    e2[j] = 0;
  }

  // Thresholds for the strong screening rule
  double *thresholds = Calloc(L, double); 

  double cutoff;
  int converged = 0;
  int counter = 0;
  int violations = 0;
  int estimate_sigma = 0;

  // Regularization Path

  for (int l = 0; l < L; l++) {

    R_CheckUserInterrupt();

    lambda0 = lambda0s[l];

    if (l != 0) {

      if (strcmp(penalty, "adaptive") == 0){
        theta = expectation_approx(b, aa, bb, p, l - 1);
      }

      if (strcmp(variance, "unknown") == 0) {

        if (INTEGER(iter)[l - 1] < 100) {
          estimate_sigma = 1;
          sigma2 = update_sigma2(r, n);
          if(sigma2 < min_sigma2) {
            sigma2 = sigma2_init;
            estimate_sigma = 0;
          }
        } else {
          estimate_sigma = 0;
          if (INTEGER(iter)[l - 1] == max_iter) {
            sigma2 = sigma2_init;
          }
        }

      }

      thresholds[l] = threshold(theta, sigma2, lambda1, lambda0, n);

      // Determine eligible set

      cutoff = thresholds[l];

      for (int j=0; j < p; j++) {
        if (fabs(z[j]) > cutoff) {
          e2[j] = 1;
        }
      }

    } else {

      thresholds[l] = threshold(theta, sigma2, lambda1, lambda0, n);

      cutoff = thresholds[l];

      for (int j=0; j<p; j++) {
        if (fabs(z[j]) > cutoff) {
          e2[j] = 1;
        }
      }

    }

    delta = thresholds[l];

    while (INTEGER(iter)[l] < max_iter) { 

      while (INTEGER(iter)[l] < max_iter) {

        while (INTEGER(iter)[l] < max_iter) {

            // Solve over the active set

          INTEGER(iter)[l]++;

          for (int j=0; j<p; j++) {


            if (e1[j]) {

                // Update residuals zj
              z[j] = crossprod(X, r, n, j) + n * a[j];

                // Update beta_j

              b[l*p+j] = SSL(z[j], a[j], lambda0, lambda1, theta, 1, n, delta, sigma2);

                // Update r

              double shift = b[l*p+j] - a[j];

              if (shift !=0) {
                for (int i=0;i<n;i++) {
                  r[i] -= shift*X[j*n+i];
                }
              }
              counter++;
            }

              // Update theta every count_max iterations

            if (counter == count_max){

              if(strcmp(penalty, "adaptive")==0) {
                theta = expectation_approx(b, aa, bb,p,l);
                delta = threshold(theta, sigma2, lambda1, lambda0, n); 
              }

              if(strcmp(variance, "unknown") == 0 && estimate_sigma) {
                sigma2 = update_sigma2(r, n);
                if(sigma2 < min_sigma2) {
                  sigma2 = sigma2_init;
                }
              }

              counter = 0;
            }

          }

            // Check for convergence over the active set

          converged = checkConvergence(b, a, eps, l, p);

          for (int j = 0; j < p; j++) {
            a[j] = b[l * p + j];
          }

          if (converged) {
            break;
          }
        }

          // Scan for violations in strong set

        violations = 0;
        counter = 0;


        for (int j=0; j < p; j++) {

          if (e1[j] == 0 && e2[j] == 1) {

            z[j] = crossprod(X, r, n, j) + n * a[j];

            // Update beta_j

            b[l*p+j] = SSL(z[j], a[j], lambda0, lambda1, theta, 1, n, delta, sigma2);

            // If something enters the eligible set, update eligible set & residuals

            if (b[l*p+j] !=0) {

              e1[j] = e2[j] = 1;

              for (int i=0; i<n; i++) {
                r[i] -= b[l*p+j]*X[j*n+i];
              }

              a[j] = b[l*p+j];

              violations++;
              counter++;
            }
          }

          if (counter == count_max) {

            if(strcmp(penalty, "adaptive") == 0) {
              theta = expectation_approx(b, aa, bb,p,l);
              delta = threshold(theta, sigma2, lambda1, lambda0, n); 
            }

            if(strcmp(variance, "unknown") == 0 && estimate_sigma) {
              sigma2 = update_sigma2(r, n);
              if(sigma2 < min_sigma2) {
                sigma2 = sigma2_init;
              }
            }

            counter=0;
          }

        }

        if (violations==0) break;
      }

      // Scan for violations in rest

      int violations = 0;

      counter=0;

      for (int j=0; j<p; j++) {

        if (e2[j] == 0) {

          z[j] = crossprod(X, r, n, j) + n * a[j];

          // Update beta_j

          b[l*p+j] = SSL(z[j], a[j], lambda0, lambda1, theta, 1, n, delta, sigma2);

          // If something enters the eligible set, update eligible set & residuals

          if (b[l*p+j] !=0) {

            e1[j] = e2[j] = 1;

            for (int i=0; i<n; i++) {
              r[i] -= b[l*p + j] * X[j*n + i];
            }

            a[j] = b[l*p+j];

            violations++;
            counter++;

          }
        }

        if (counter == count_max){

          if(strcmp(penalty, "adaptive") == 0) {
            theta = expectation_approx(b, aa, bb,p,l);
            delta = threshold(theta, sigma2, lambda1, lambda0, n); 
          }
          if(strcmp(variance, "unknown") == 0 && estimate_sigma) {
            sigma2 = update_sigma2(r, n);
            if(sigma2 < min_sigma2) {
              sigma2 = sigma2_init;
            }
          }
          counter = 0;
        }

      }


      if (violations == 0) {

        REAL(loss)[l] = gLoss(r, n);
        thetas[l] = theta;
        sigmas[l] = sqrt(sigma2);

        break;
      }
    }
  }

  res = cleanupG(a, r, e1, e2, z, beta, loss, iter, thetas_export, sigmas_export);
  UNPROTECT(5);

  return(res);
}
