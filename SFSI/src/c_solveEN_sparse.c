#include "SFSI.h"
//#include "utils.c"

//====================================================================
// Calculate betas for a set of decreasing lambdas: l[0]>l[1]>...>l[k]
// The procedure is iteratively done until convergence, i.e., the
// difference between two consecutive solutions is smaller than a threshold.
// It uses the covariance matrix XtX and the covariance vector Xty
// XtX is assumed to be scaled with diagonal equal to one
//
//     p:         Number of beta parameters (dim of XtX)
//     XtX:       Crossprod matrix X'X
//     Xty:       Crossprod vector X'y
//     q:         Number of lambdas
//     lambda:    Vector of lambdas for which the betas will be calculated
//     alpha:     Alpha value in the Elastic-net problem
//     tol:       Maximum value between two consecutive solutions for convergence
//     maxiter:   Number of iterations to run before the updating stops
//
// ---------------------------------------------------------------------
// The fitted values yHatNoj[i] excluding the contribution from X[,j] is
//                     yHatNoj[i] = sum{k!=j}X[i,k]b[k]
// and can be written for the whole vector yHatNoj = {yHatNoj[i]} as
//                     yHatNoj = Xb - X[,j]b[j]
//
// Fitting the partial residual r[i] = y[i] - yHatNoj[i] to X[i,j] has
// an OLS estimator equal to
//             bOLS[j] = (1/n)sum{i=1:n}[X[i,j](y[i] - yHatNoj[i])]
// which can be written as
//             bOLS[j] = X[,j]'y - XtyHatNoj[j]
// where
//                XtyHatNoj[j] = X[,j]'Xb - X[,j]'X[,j]b[j]
//
// After soft-thresholding the OLS estimator for variable j:
//                       bNew[j] <- S(bOLS[j], L1)/(1+L2)
// the terms XtyHatNoj[k] are updated for all k (different from j) if
// there is a change in delta = bNew[j]-b[j]:
//         XtyHatNoj[k] <- XtyHatNoj[k] + X[,j]'X[,k]*(bNew[j]-b[j])
//
// This will replace the contribution of the current value b[j] by the
// contribution of the updated value bNew[j] in XtyHatNoj
//====================================================================
SEXP R_updatebeta_sparse(SEXP XtX_, SEXP Xty_,
                         SEXP lambda_, SEXP alpha_, SEXP b0_,
                         SEXP tol_, SEXP maxiter_, SEXP dfmax_,
                         SEXP scale_, SEXP sd_,
                         SEXP eps_, SEXP filename_,
                         SEXP doubleprecision_, SEXP verbose_)
{
    double L1, L2, maxerror, delta, bOLS, bNew;
    long long i, j;
    int k, iter;
    int varsize, vartype;
    int inc1 = 1;
    double *B;
    float valuefloat;
    int nprotect = 10;
    FILE *f = NULL;

    int p = Rf_length(Xty_);
    int nlambda = Rf_length(lambda_);
    int maxiter = INTEGER_VALUE(maxiter_);
    int dfmax = INTEGER_VALUE(dfmax_);
    int verbose = asLogical(verbose_);
    int scale = asLogical(scale_);
    double alpha = NUMERIC_VALUE(alpha_);
    double tol = NUMERIC_VALUE(tol_);
    double eps = NUMERIC_VALUE(eps_);
    int doubleprecision = asLogical(doubleprecision_);
    int save = !Rf_isNull(filename_);

    PROTECT(lambda_ = AS_NUMERIC(lambda_));
    double *lambda = NUMERIC_POINTER(lambda_);

    PROTECT(sd_ = AS_NUMERIC(sd_));
    double *sd = NUMERIC_POINTER(sd_);

    PROTECT(XtX_ = AS_NUMERIC(XtX_));
    double *XtX = NUMERIC_POINTER(XtX_);

    PROTECT(Xty_ = AS_NUMERIC(Xty_));
    double *Xty = NUMERIC_POINTER(Xty_);

    int *df = (int *) R_alloc(nlambda, sizeof(int));
    int *niter = (int *) R_alloc(nlambda, sizeof(int));    // Niter at each lambda
    double *error = (double *) R_alloc(nlambda, sizeof(double)); // Max error b0-bnew at each lambda
    double *b = (double *) R_alloc(p, sizeof(double));     // Current b[j] values
    double *bout = (double *) R_alloc(p, sizeof(double));  // Output
    double *XtyHatNoj = (double *) R_alloc(p, sizeof(double)); // XtyHatNoj[j] = {XtX[,j]'b - XtX[j,j]b[j]}

    for(k=0; k<nlambda; k++){
      df[k] = p;
    }

    if(Rf_isNull(b0_)){
      memset(b, 0, sizeof(double)*p);          // Initialize all b[j] to zero
      memset(XtyHatNoj, 0, sizeof(double)*p);  // All XtyHatNoj[j] are also zero
    }else{
      PROTECT(b0_ = AS_NUMERIC(b0_));
      memcpy(b, NUMERIC_POINTER(b0_), sizeof(double)*p);
      nprotect++;

      // XtyHatNoj[j] = {XtX[,j]'b - XtX[j,j]b[j]}; here XtX[j,j]=1
      for(j=0; j<p; j++){
        XtyHatNoj[j] = F77_NAME(ddot)(&p, XtX + p*j, &inc1, b, &inc1) - b[j];
      }
    }

    if(save){
      //Rprintf(" Saving binary file info for B ...\n");
      varsize = doubleprecision ? sizeof(double) : sizeof(float);
      vartype = 3;
      f = fopen(CHAR(STRING_ELT(filename_,0)),"wb");
      fwrite(&p, sizeof(int), 1, f);
      fwrite(&nlambda, sizeof(int), 1, f);
      fwrite(&vartype, sizeof(int), 1, f);
      fwrite(&varsize, sizeof(int), 1, f);
    }else{
      //Rprintf(" Allocating memory for B ...\n");
      // Allocated memory is set to zero (as in calloc)
      B = (double *) R_Calloc(p*nlambda, double);
    }

    // Get indices of nonzero
    long long nnonzero = (long long)p*p/2;
    int *inonzero = (int *) R_Calloc(nnonzero, int);
    int *nnonzeroj = (int *) R_alloc(p, sizeof(int));
    long long cont = 0;

    for(j=0; j<p; j++){
      nnonzeroj[j] = 0;
      for(i=0; i<p; i++){
        if(fabs(XtX[p*j + i])>eps){
          inonzero[cont++] = i;
          nnonzeroj[j]++;
        }
      }
      if((nnonzero < (cont+p)) && j<(p-1)){  // Enlarge memory if needed
        nnonzero += p;
        inonzero = R_Realloc(inonzero, nnonzero, int);
      }
    }

    //Rprintf(" Starting beta updating ...\n");
    for(k=0; k<nlambda; k++)
    {
        L1 = alpha*lambda[k];
        L2 = (1-alpha)*lambda[k];
        iter = 0;              // Set iter < maxiter to enter the WHILE
        maxerror = tol + 1.0;  // Set a error > tol to enter the WHILE
        while(iter<maxiter && maxerror>tol)
        {
           iter++;
           maxerror = 0;
           cont = 0; // for the number of nonzero
           for(j=0; j<p; j++)
           {
              // varj = XtX[p*j + j];  // Variance of predictor j
              // bOLS = (Xty[j] - XtyHatNoj[j])/varj;
              bOLS = Xty[j] - XtyHatNoj[j];
              bNew = soft_threshold(bOLS, L1)/(1+L2);

              delta = bNew-b[j];
              //Rprintf(" j=%d. XtyHatNoj=%1.8f bOLS=%1.8f  bNew=%1.8f  delta=%f\n",j+1,XtyHatNoj[j], bOLS, bNew, delta);
              if(fabs(delta)>0){ // Update only if there is a change
                // Update: XtyHatNoj[k] <- XtyHatNoj[k] + XtX[k,j]*(bNew[j]-b[j])
                //F77_NAME(daxpy)(&p, &delta, XtX + p*j, &inc1, XtyHatNoj, &inc1);
                daxpy_set(nnonzeroj[j], &delta, XtX + p*j, inonzero + cont, XtyHatNoj, inonzero +  cont);

                XtyHatNoj[j] -= delta; // except for k=j. delta*varj
                if(fabs(delta)>maxerror){
                  maxerror = fabs(delta);
                }
                b[j] = bNew;
              }
              cont += nnonzeroj[j]; // cumulate the counter
           }
        }
        niter[k] = iter;
        error[k] = maxerror;
        F77_NAME(dcopy)(&p, b, &inc1, bout, &inc1);

        if(scale){
          for(j=0; j<p; j++){
            bout[j] = bout[j]/sd[j];
          }
        }

        df[k] = 0;
        for(j=0; j<p; j++){
          if(fabs(bout[j])>0) df[k]++;
        }

        if(verbose){
            Rprintf(" lambda[%d]=%1.8f. nsup=%5d. niter=%5d. Error=%G\n",k+1,lambda[k],df[k],iter,maxerror);
            if(maxerror>tol){
              Rprintf(" Warning: The process did not converge after %d iterations for lambda[%d]=%f\n",maxiter,k+1,lambda[k]);
            }
        }

        if(save){
          if(doubleprecision){
            fwrite(bout, varsize, p, f);
          }else{  // Cast to float one by one
            for(j=0; j<p; j++){
              valuefloat = bout[j];
              fwrite(&valuefloat, varsize, 1, f);
            }
          }
        }else{
          F77_NAME(dcopy)(&p, bout, &inc1, B + p*k, &inc1);
        }

        if(dfmax<p && df[k]>=dfmax){
          break;
        }
    }

    //Rprintf(" Writting results: lambda, nsup, B ...\n");
    SEXP lambda2_ = PROTECT(Rf_allocVector(REALSXP, k));
    memcpy(NUMERIC_POINTER(lambda2_), lambda, k*sizeof(double));

    SEXP df_ = PROTECT(Rf_allocVector(INTSXP, k));
    memcpy(INTEGER_POINTER(df_), df, k*sizeof(int));

    SEXP niter_ = PROTECT(Rf_allocVector(INTSXP, k));
    memcpy(INTEGER_POINTER(niter_), niter, k*sizeof(int));

    SEXP error_ = PROTECT(Rf_allocVector(REALSXP, k));
    memcpy(NUMERIC_POINTER(error_), error, k*sizeof(double));

    SEXP B_ = NULL;
    if(save){
      fseek(f, sizeof(int), SEEK_SET); // Save the final number of solutions
      fwrite(&k, sizeof(int), 1, f);
      fclose(f);
      B_ = R_NilValue;
    }else{
      B_ = PROTECT(Rf_allocMatrix(REALSXP, p, k));
      memcpy(NUMERIC_POINTER(B_), B, p*k*sizeof(double));
      nprotect++;
    }

    SEXP list_ = PROTECT(Rf_allocVector(VECSXP, 5));
    SET_VECTOR_ELT(list_, 0, B_);
    SET_VECTOR_ELT(list_, 1, lambda2_);
    SET_VECTOR_ELT(list_, 2, df_);
    SET_VECTOR_ELT(list_, 3, niter_);
    SET_VECTOR_ELT(list_, 4, error_);

    // Set dimnames for outputs
    SEXP names_ = PROTECT(Rf_allocVector(VECSXP, 5));
    SET_VECTOR_ELT(names_, 0, mkChar("beta"));
    SET_VECTOR_ELT(names_, 1, mkChar("lambda"));
    SET_VECTOR_ELT(names_, 2, mkChar("nsup"));
    SET_VECTOR_ELT(names_, 3, mkChar("niter"));
    SET_VECTOR_ELT(names_, 4, mkChar("error"));
    setAttrib(list_, R_NamesSymbol, names_);

    UNPROTECT(nprotect);

    return(list_);
}
