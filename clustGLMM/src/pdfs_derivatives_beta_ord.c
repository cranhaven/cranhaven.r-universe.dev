/*
 * Functions for logs of full conditional pdfs and their derivatives
 */

#include <R.h>
#include <Rmath.h>
#include <stdio.h>

#include "structures.h"
#include "my_math.h"

/*
 * BETA_ORD
 */

void logpYord(struct str_state* last,  // IN last known values of generated parameters
              double* Y,                // IN [*N]
              double* X,                // IN [N*(#regr)]    regressors
              int* spec,            // [5]                class-specific parameters
              // order:   [0] prec_num, 
              //          [1] c_ord,
              //          [2] InvSigma, 
              //          [3] InvQ, 
              //          [4] naY
              int* dims,            // [33]:              what parameters are saved
              // order:   [0-1]   beta_num_fix, beta_num,
              //          [2-4]   prec_num, sd_num, var_num,
              //          [5-6]   beta_poi_fix, beta_poi,
              //          [7-8]   beta_bin_fix, beta_bin,
              //          [9-10]  beta_ord_fix, beta_ord,
              //          [11-13] c_ord, a_ord, pi_ord,
              //          [14-15] beta_cat_fix, beta_cat,
              //          [16-20] InvSigma, Sigma, sdSigma, corSigma, detInvSigma,
              //          [21-23] InvQ, Q, detInvQ,
              //          [24]    b,
              //          [25-26] w, ng,
              //          [27-29] loglik, pUig, U,
              //          [30,31] Gplus, e0,
              //          [32]    naY
              double* predictor_num,
              double* predictor_poi,
              double* predictor_bin,
              double* predictor_ord,
              double* predictor_cat,
              int* pred_skip,         // IN [1] Which part of predictor is to be skipped
              int* N,                 // IN [1] number of observations
              int* n,                 // IN [1] number of subjects
              int* g,                 // IN [1] *G will be passed for FIXED, g for group-specific data
              int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
              int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
              int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
              //        FALSE = all levels of cat var have one set of bi - compares last category with others
              int* nfix,              // IN [1] number of FIXED  regressors or each response
              int* cumnfix,           // IN [1] where to start in FormulaF
              int* totnfix,           // IN [1] total dimension of fixed parameter (useless)
              int* FormulaF,          // IN [nfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
              int* n_i,               // IN [n] number of observations dedicated to j-th subject
              int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
              int* nUg,               // IN [G] number of subjects in classes 1, ..., G
              int* listUi,            // IN [n*G] row by row indices of subjects in group g
              double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
              double* value           // OUT [1]
){
  // Can be used for either beta_ord_fix or beta_ord
  // Difference is given by pred_skip and g
  // if beta ord (group-specific coefficients) then nfix, cumnfix, FormulaF are actually grp counterparts
  
  // Auxiliary variables
  int i, j, k, l;
  int coord;
  double qk;
  double etak;
  // double etak_1;
  // double pk, pk_1;
  int add = *totnfix; // not totnfix, but corresponding cumKord is sent through this pointer
  int gi = *g;
  double* pc;
  
  *value = 0.0;
  
  for(l = 0; l < nUg[*g]; l++){
    // just those in g-th cluster (if *g == G), it is across all of them
    i = listUi[l + *n * *g];
    // i belongs to cluster gi, which is the same as given *g if *g < *G
    if(*g == dims[25]){ // (*g = G) we use it for all data
      gi = (*last).U[i];
    }
    if(spec[1]){
      // c_ord is group-specific
      pc = (*last).c_ord + add + gi * dims[11];
    }else{
      pc = (*last).c_ord + add;
    }
    for(j = 0; j < n_i[i]; j++){
      coord = i_j[i + *n * j]; // index of j-th observation of i-th individual within g-th cluster
      // New fixed/group-specific predictor part using theta
      etak = 0.0;
      //if(l < 3){
      //  printf("\nlogpYord: i = %d, j = %d, nfix = %d, etak = %f", i, j, *nfix, etak);
      //}
      for(k = 0; k < *nfix; k++){
        etak += theta[k] * X[coord + *N * FormulaF[k + *cumnfix]];
        //if(l < 3){
        //  printf("\nlogpYord: i = %d, j = %d, k = %d, etak = %f, theta=%f, X=%f", 
        //         i, j, k, etak, theta[k], X[coord + *N * FormulaF[k + *cumnfix]]);
        //}
      }
      // Add other parts: G/F, R, O
      for(k = 0; k < 4; k++){
        if(*pred_skip != k){
          etak += predictor_ord[k + 4*coord];
        }
        //if(l < 3){
        //  printf("\nlogpYord: i = %d, j = %d, k = %d, etak = %f", i, j, k, etak);
        //}
      }
      //fflush(stdout);
      
      k = Y[coord]; // double to int, requires values in {0, ..., Kord[y]}
      // Subtract the intercept for the category within
      /*
      if(k == 0){
        pk_1 = 1.0;
      }else{
        etak_1 = etak - pc[k-1];
        pk_1 = logit_inv(etak_1);
      }
      if(k == *Kord){
        pk = 0.0;
      }else{
        etak -= pc[k];
        pk = logit_inv(etak);
      }
      qk = pk_1 - pk + 0.00000001;
      *value += log(qk);
      */
      
      // Numerically stable version
      /*
      if(k == 0){
        pk_1 = 0.0;                      // complementary probability
        etak -= pc[k];
        pk = 1.0 / (1.0 + exp(etak));    // complementary probability
        qk = pk - pk_1;
      }else{
        if(k == *Kord){
          pk = 0.0;
          etak_1 = etak - pc[k-1];
          pk_1 = 1.0 / (1.0 + exp(-etak_1));
          qk = pk_1 - pk;
        }else{
          etak_1 = etak - pc[k-1];
          etak -= pc[k];
          
          if(etak > 0){
            pk = 1.0 / (1.0 + exp(etak));     // complementary probability
            pk_1 = 1.0 / (1.0 + exp(etak_1)); // complementary probability
            qk = pk - pk_1;
          }else{
            pk = 1.0 / (1.0 + exp(-etak));
            pk_1 = 1.0 / (1.0 + exp(-etak_1));
            qk = pk_1 - pk;
          }
        }
      }
      */
      qk = ord_qk(k, etak, pc, Kord);
      *value += log(qk);
    }
  }
  
}

void logpbeta_ord(struct str_state* last,  // IN last known values of generated parameters
                  double* sd_beta,          // hyperparameter - std. dev. of prior for beta
                  double* Y,                // IN [*N]
                  double* X,                // IN [N*(#regr)]    regressors
                  int* spec,            // [5]                class-specific parameters
                  // order:   [0] prec_num, 
                  //          [1] c_ord,
                  //          [2] InvSigma, 
                  //          [3] InvQ, 
                  //          [4] naY
                  int* dims,            // [33]:              what parameters are saved
                  // order:   [0-1]   beta_num_fix, beta_num,
                  //          [2-4]   prec_num, sd_num, var_num,
                  //          [5-6]   beta_poi_fix, beta_poi,
                  //          [7-8]   beta_bin_fix, beta_bin,
                  //          [9-10]  beta_ord_fix, beta_ord,
                  //          [11-13] c_ord, a_ord, pi_ord,
                  //          [14-15] beta_cat_fix, beta_cat,
                  //          [16-20] InvSigma, Sigma, sdSigma, corSigma, detInvSigma,
                  //          [21-23] InvQ, Q, detInvQ,
                  //          [24]    b,
                  //          [25-26] w, ng,
                  //          [27-29] loglik, pUig, U,
                  //          [30,31] Gplus, e0,
                  //          [32]    naY
                  double* predictor_num,
                  double* predictor_poi,
                  double* predictor_bin,
                  double* predictor_ord,
                  double* predictor_cat,
                  int* pred_skip,         // IN [1] Which part of predictor is to be skipped
                  int* N,                 // IN [1] number of observations
                  int* n,                 // IN [1] number of subjects
                  int* g,                 // IN [1] *G will be passed for FIXED, g for group-specific data
                  int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
                  int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
                  int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
                  //        FALSE = all levels of cat var have one set of bi - compares last category with others
                  int* nfix,              // IN [1] number of FIXED  regressors or each response
                  int* cumnfix,           // IN [1] where to start in FormulaF
                  int* totnfix,           // IN [1] total dimension of fixed parameter (useless)
                  int* FormulaF,          // IN [nfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
                  int* n_i,               // IN [n] number of observations dedicated to j-th subject
                  int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
                  int* nUg,               // IN [G] number of subjects in classes 1, ..., G
                  int* listUi,            // IN [n*G] row by row indices of subjects in group g
                  double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
                  double* value           // OUT [1]
){
  // Auxiliary variables
  //double loglik;
  double x;
  int k;
  
  logpYord(last, Y, X, spec, dims, 
           predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, pred_skip,
           N, n, g, Kord, Kcat, kspec_bi_cat, 
           nfix, cumnfix, totnfix, FormulaF, n_i, i_j, nUg, listUi, theta, value);
  
  
  //*value = loglik;
  for(k = 0; k < *nfix; k++){
    x = theta[k] / (*sd_beta);
    *value -= 0.5 * x * x;
  }
  
}

void d_d2_logpbeta_ord(struct str_state* last,  // IN last known values of generated parameters
                       double* sd_beta,          // hyperparameter - std. dev. of prior for beta
                       double* Y,                // IN [*N]
                       double* X,                // IN [N*(#regr)]    regressors
                       int* spec,            // [5]                class-specific parameters
                       // order:   [0] prec_num, 
                       //          [1] c_ord,
                       //          [2] InvSigma, 
                       //          [3] InvQ, 
                       //          [4] naY
                       int* dims,            // [33]:              what parameters are saved
                       // order:   [0-1]   beta_num_fix, beta_num,
                       //          [2-4]   prec_num, sd_num, var_num,
                       //          [5-6]   beta_poi_fix, beta_poi,
                       //          [7-8]   beta_bin_fix, beta_bin,
                       //          [9-10]  beta_ord_fix, beta_ord,
                       //          [11-13] c_ord, a_ord, pi_ord,
                       //          [14-15] beta_cat_fix, beta_cat,
                       //          [16-20] InvSigma, Sigma, sdSigma, corSigma, detInvSigma,
                       //          [21-23] InvQ, Q, detInvQ,
                       //          [24]    b,
                       //          [25-26] w, ng,
                       //          [27-29] loglik, pUig, U,
                       //          [30,31] Gplus, e0,
                       //          [32]    naY
                       double* predictor_num,
                       double* predictor_poi,
                       double* predictor_bin,
                       double* predictor_ord,
                       double* predictor_cat,
                       int* pred_skip,         // IN [1] Which part of predictor is to be skipped
                       int* N,                 // IN [1] number of observations
                       int* n,                 // IN [1] number of subjects
                       int* g,                 // IN [1] *G will be passed for FIXED, g for group-specific data
                       int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
                       int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
                       int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
                       //        FALSE = all levels of cat var have one set of bi - compares last category with others
                       int* nfix,              // IN [1] number of FIXED  regressors or each response
                       int* cumnfix,           // IN [1] where to start in FormulaF
                       int* totnfix,           // IN [1] total dimension of fixed parameter (useless)
                       int* FormulaF,          // IN [nfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
                       int* n_i,               // IN [n] number of observations dedicated to j-th subject
                       int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
                       int* nUg,               // IN [G] number of subjects in classes 1, ..., G
                       int* listUi,            // IN [n*G] row by row indices of subjects in group g
                       double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
                       double* grad,           // OUT [dim(theta)]
                       double* hess            // OUT [dim(theta) * (dim(theta)+1)/2]
){
  // Auxiliary variables
  int i, j, k, k1, k2, l;
  int coord;
  double etak, etak_1;
  double pk, pk_1;
  double ppk, ppk_1;
  double pp, pp1;
  int add = *totnfix; // not totnfix, but corresponding cumKord is sent through this pointer
  int gi = *g;
  double* pc;
  double auxX;
  
  // start with prior distribution contribution
  for(k1 = 0; k1 < *nfix; k1++){
    grad[k1] = -theta[k1] / (*sd_beta * *sd_beta );
    k = k1 * (k1+3) /2;
    hess[k] = 1.0 / (*sd_beta * *sd_beta );
    for(k2 = k1+1; k2 < *nfix; k2++){
      k = k1 + k2*(k2+1)/2;
      hess[k] = 0.0;
    }
    
  }
  
  for(l = 0; l < nUg[*g]; l++){
    // just those in g-th cluster (if g == G*), it is across all of them
    i = listUi[l + *n * *g];
    // i belongs to cluster gi, which is the same as given *g if *g < *G
    if(*g == dims[25]){ // (*g = G) we use it for all data
      gi = (*last).U[i];
    }
    if(spec[1]){
      // c_ord is group-specific
      pc = (*last).c_ord + add + gi * dims[11];
    }else{
      pc = (*last).c_ord + add;
    }
    for(j = 0; j < n_i[i]; j++){
      coord = i_j[i + *n * j]; // index of l-th observation of j-th individual within g-th cluster
      
      // New fixed predictor part using theta
      etak = 0.0;
      //if(l < 3){
      //  printf("\nd_d2_logpbeta_ord: i = %d, j = %d, nfix = %d, etak = %f", i, j, *nfix, etak);
      //}
      for(k = 0; k < *nfix; k++){
        etak += theta[k] * X[coord + *N * FormulaF[k + *cumnfix]];
      }
      //if(l < 3){
      //  printf("\nd_d2_logpbeta_ord: i = %d, j = %d, etak = %f", i, j, etak);
      //}
      // Add other parts: G/F, R, O
      for(k = 0; k < 4; k++){
        if(*pred_skip != k){
          etak += predictor_ord[k + 4*coord];
        }
        //if(l < 3){
        //  printf("\nd_d2_logpbeta_ord: i = %d, j = %d, k = %d, etak = %f", i, j, k, etak);
        //}
      }
      
      k = Y[coord]; // double to int, requires values in {0, ..., Kord[y]}
      // Subtract the intercept for the category within
      if(k == 0){
        pk_1 = 1.0;
        ppk_1 = 0.0;
      }else{
        etak_1 = etak - pc[k-1];
        pk_1 = logit_inv(etak_1);
        ppk_1 = pk_1 * (1-pk_1);
      }
      if(k == *Kord){
        pk = 0.0;
        ppk = 0.0;
      }else{
        etak -= pc[k];
        pk = logit_inv(etak);
        ppk = pk * (1-pk);
      }
      pp = ppk + ppk_1;
      pp1 = 1.0 - pk_1 - pk;
      
      //if(l < 3){
      //  printf("\nd_d2_logpbeta_ord: i = %d, j = %d, nfix = %d, eta = %f,
      //         i, j, *nfix, eta);
      //}
      
      for(k1 = 0; k1 < *nfix; k1++){
        auxX = X[coord + *N * FormulaF[k1 + *cumnfix]];
        grad[k1] += pp1 * auxX;
        for(k2 = k1; k2 < *nfix; k2++){
          k = k1 + k2*(k2+1)/2;
          hess[k] += pp * auxX * X[coord + *N * FormulaF[k2 + *cumnfix]];
        }
      }
    }
  }
}
