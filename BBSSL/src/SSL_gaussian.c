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
double SSL(double z, double beta, double lambda0, double lambda1, double theta, double v, double norm, double delta, double sigma2);
double pstar(double x, double theta, double lambda1, double lambda0);
double lambdastar(double x, double theta, double lambda1, double lambda0);
double expectation_approx(double *beta, double a, double b, int p, int l);
double threshold(double theta, double sigma2, double lambda1, double lambda0, double norm);

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

SEXP SSL_gaussian(SEXP X_, SEXP y_, SEXP initialbeta_, SEXP penalty_, SEXP variance_, SEXP lambda1_, SEXP lambda0s_, SEXP theta_, SEXP sigma_, SEXP min_sigma2_, SEXP a_,  SEXP b_, SEXP eps_, SEXP max_iter_, SEXP counter_) {

  // Declarations
  int n = length(y_);
  int p = length(X_)/n;
  int L = length(lambda0s_);

  double *X = REAL(X_);
  double *y = REAL(y_);
  double *initialbeta = REAL(initialbeta_);
  
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
  SEXP xnorm_;
  PROTECT(xnorm_ = allocVector(REALSXP, p));
  double *xnorm = REAL(xnorm_);

  // calculate norm of each column of X
  for (int j=0; j<p; j++) {
    xnorm[j] = 0;
    for (int i=0; i<n; i++) { // O(np)
      xnorm[j] += pow(X[j*n+i], 2);
    }
  }
  
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

  double *delta = Calloc(p, double); 

  // Beta from previous iteration
  double *a = Calloc(p, double); 
  double *newa = Calloc(p, double);
  
  for (int j = 0; j < p; j++) {
    a[j] = initialbeta[j];
    newa[j] = initialbeta[j];
  }


  // Index of an active set
  int *e1 = Calloc(p, int); 

  for (int j=0; j<p; j++) {
    e1[j] = 1-(a[j]==0);
  }

  // Index of an elible set from the strong rule
  int *e2 = Calloc(p, int); 

  for (int j=0; j<p; j++) {
    e2[j] = 1-(a[j]==0);
  }

  double *r = Calloc(n, double);
  double *XTY = Calloc(p, double);
  double *XTX = Calloc(p*p, double);
  
  for (int i=0; i<n; i++) {
    r[i] = y[i];
    for (int j=0; j<p; j++){
      r[i] -= X[j*n+i]*a[j]; //-X[i,j]*beta[j]
    }
  }
  
  double *z = Calloc(p, double);
  
  for (int j=0; j<p; j++) {
    z[j] = crossprod(X, r, n, j);							// O(np)
  }
  
  int thres = n; // thres = min(n, max_iter)
  if (thres > max_iter){
    thres = max_iter;
  }
  
  if (p < thres) { // when n is large, we utilize XTX and XTY to udpate beta instead of keeping track of residual r
    
    for (int j=0; j<p; j++){
      XTY[j] = 0;
      for (int i=1; i<n; i++){
        XTY[j] += X[j*n+i] * y[i];
      }
      //printf("the %d-th element in XTY is %f", j, XTY[j]);
    }
    
    for (int i=0; i<p; i++){
      for (int j=0; j<p; j++){
        XTX[j*p+i] = 0;
        for (int k=0; k<n; k++){
          XTX[j*p+i] += X[i*n+k] * X[j*n+k]; // O(p^2n)
        }
        //printf("the (%d,%d)-element in XTX is %f", i, j, XTX[j*p+i]);
      }
    }
    
  }
  
  
  // Thresholds for the strong screening rule
  double *thresholds = Calloc(p, double); 

  double *cutoff = Calloc(p, double);
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

      for (int j=0; j<p; j++) {
        thresholds[j] = threshold(theta, sigma2, lambda1, lambda0, xnorm[j]);
        cutoff[j] = thresholds[j];
        delta[j] = thresholds[j];
      }
      

      // Determine eligible set

      
      for (int j=0; j < p; j++) {
        if (fabs(z[j]) > cutoff[j]) {
          e2[j] = 1;
        }
      }

    } else {

      for (int j=0; j<p; j++) {
        thresholds[j] = threshold(theta, sigma2, lambda1, lambda0, xnorm[j]);
        cutoff[j] = thresholds[j];
        delta[j] = thresholds[j];
      }

      
      
      for (int j=0; j<p; j++) {
        if (fabs(z[j]) > cutoff[j]) {
          e2[j] = 1;
        }
      }

    }

    
    while (INTEGER(iter)[l] < max_iter) { 

      while (INTEGER(iter)[l] < max_iter) {

        while (INTEGER(iter)[l] < max_iter) {

            // Solve over the active set

          INTEGER(iter)[l]++;

          for (int j=0; j<p; j++) {


            if (e1[j]) {								// only for currently active predictors

                // Update residuals zj
              if (p >= thres){
                z[j] = crossprod(X, r, n, j) + xnorm[j] * a[j]; 					// O(n)
              }else{
                z[j] = XTY[j] - crossprod(XTX, newa, p, j) + xnorm[j] * a[j]; // O(p)
              }
              

                // Update beta_j

              b[l*p+j] = SSL(z[j], a[j], lambda0, lambda1, theta, 1, xnorm[j], delta[j], sigma2); // O(1)
                // Update r

              
              if (p >= thres) {
                double shift = b[l*p+j] - a[j];
                if (shift !=0) {
                  //printf("updated beta[%d]=%f\t", j, b[l*p+j]);
                  for (int i=0;i<n;i++) {
                    r[i] -= shift*X[j*n+i];
                  }
                }										// O(n)
              } else {
                newa[j] = SSL(z[j], a[j], lambda0, lambda1, theta, 1, xnorm[j], delta[j], sigma2);
              }
              
              counter++;
            }

              // Update theta every count_max iterations				// only for every count_max iterations

            if (counter == count_max){

              if(strcmp(penalty, "adaptive")==0) {
                theta = expectation_approx(b, aa, bb,p,l);				// O(p)
                for (int j=0; j<p; j++){
                  delta[j] = threshold(theta, sigma2, lambda1, lambda0, xnorm[j]); 			// O(1)
                }
              }

              if(strcmp(variance, "unknown") == 0 && estimate_sigma) {
                sigma2 = update_sigma2(r, n);						// O(n)
                if(sigma2 < min_sigma2) {
                  sigma2 = sigma2_init;
                }
              }

              counter = 0;
            }

          }

            // Check for convergence over the active set

          converged = checkConvergence(b, a, eps, l, p);				// O(p)

          for (int j = 0; j < p; j++) {
            a[j] = b[l * p + j];
            //printf("beta[%d]=%f\t",j, a[j]);
          }										// O(p)

          if (converged) {
            break;
          }
        }

          // Scan for violations in strong set

        violations = 0;
        counter = 0;


        for (int j=0; j < p; j++) {

          if (e1[j] == 0 && e2[j] == 1) {						// only for those strong predictors

            if (p >= thres){
              z[j] = crossprod(X, r, n, j) + xnorm[j] * a[j]; 					// O(n)
            }else{
              z[j] = XTY[j] - crossprod(XTX, a, p, j) + xnorm[j] * a[j]; // O(p)
            }
            
            // Update beta_j

            b[l*p+j] = SSL(z[j], a[j], lambda0, lambda1, theta, 1, xnorm[j], delta[j], sigma2);
            
            // If something enters the eligible set, update eligible set & residuals

            if (b[l*p+j] !=0) {

              e1[j] = e2[j] = 1;

              if (p >= thres){
                for (int i=0; i<n; i++) {							// O(n)
                  r[i] -= b[l*p+j]*X[j*n+i];
                }
              } 
              
              a[j] = b[l*p+j];

              violations++;
              counter++;
            }
          }

          if (counter == count_max) {

            if(strcmp(penalty, "adaptive") == 0) {
              theta = expectation_approx(b, aa, bb,p,l);				// O(p)
              for (int j=0; j<p; j++){
                delta[j] = threshold(theta, sigma2, lambda1, lambda0, xnorm[j]); 
              }
            }

            if(strcmp(variance, "unknown") == 0 && estimate_sigma) {
              sigma2 = update_sigma2(r, n);						// O(n)
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

        if (e2[j] == 0) {								// only for inactive ones

          if (p >= thres){
            z[j] = crossprod(X, r, n, j) + xnorm[j] * a[j]; 					// O(n)
          }else{
            z[j] = XTY[j] - crossprod(XTX, a, p, j) + xnorm[j] * a[j]; // O(p)
          }
          
          // Update beta_j

          b[l*p+j] = SSL(z[j], a[j], lambda0, lambda1, theta, 1, xnorm[j], delta[j], sigma2);
          
          // If something enters the eligible set, update eligible set & residuals

          if (b[l*p+j] !=0) {

            e1[j] = e2[j] = 1;

            if (p >= thres){
              for (int i=0; i<n; i++) {
                r[i] -= b[l*p + j] * X[j*n + i];						// O(n)
              }
            }
            
            a[j] = b[l*p+j];

            violations++;
            counter++;

          }
        }

        if (counter == count_max){

          if(strcmp(penalty, "adaptive") == 0) {
            theta = expectation_approx(b, aa, bb,p,l);					// O(p)
            for(int j=0; j<p; j++){
              delta[j] = threshold(theta, sigma2, lambda1, lambda0, xnorm[j]); 
            }
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
  UNPROTECT(6);

  return(res);
}
