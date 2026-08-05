/*
 * Functions for logs of full conditional pdfs and their derivatives
 */

#include <R.h>
#include <Rmath.h>
#include <stdio.h>

#include "structures.h"
#include "matrmult.h"
#include "my_math.h"

/*
 * BETA_BIN
 */

void logpYi(struct str_state* last,  // IN last known values of generated parameters
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
            int* nY,                // IN [5] counts of outcomes
            int* N,                 // IN [1] number of observations
            int* n,                 // IN [1] number of subjects
            int* i,                 // IN [1] index of a subject
            int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
            int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
            int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
            //        FALSE = all levels of cat var have one set of bi - compares last category with others
            int* nran,              // IN [1] number of RANDOM  regressors or each response
            int* cumnran,           // IN [1] where to start in FormulaR
            int* totnran,           // IN [1] total dimension of fixed parameter (useless)
            int* FormulaR,          // IN [totnran]  numbers of columns of X that should be used for RANDOM  effects of modelled responses
            int* n_i,               // IN [n] number of observations dedicated to j-th subject
            int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
            int* nUg,               // IN [G] number of subjects in classes 1, ..., G
            int* listUi,            // IN [n*G] row by row indices of subjects in group g
            double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
            double* value           // OUT [1]
){
  // Auxiliary variables
  int j, k, l, g, y, t, tstart;
  int coord, auxcoord;
  double eta, auxY;
  int shift_nY = 0;
  double* ptau;
  double* pc;
  double qk;
  // double etak, etak_1;
  // double pk, pk_1;
  int add_to_c;
  int maxKcat = 0;
  for(y = 0; y < nY[4]; y++){
    if(maxKcat < Kcat[y]){
      maxKcat = Kcat[y];
    }
  }
  int cumKcat;
  double* etas;
  etas = (double*)malloc(maxKcat* sizeof(double));
  //double etas[maxKcat];
  double maxeta;
  double log_sum_exp;
  
  *value = 0.0;
  g = (*last).U[*i]; // group to which i-th subject belongs
  //printf("\nlogpYi: n = %d,  n_j[%d] = %d, g = %d", *n, *i, n_j[*i], g);
  //fflush(stdout);
  
  if(spec[0]){
    // prec_num is g-specific
    ptau = (*last).prec_num + g*dims[2];
  }else{
    ptau = (*last).prec_num;
  }
  
  if(spec[1]){
    // a_ord and c_ord are g-specific
    pc = (*last).c_ord + g*dims[11];
  }else{
    pc = (*last).c_ord;
  }
  
  for(j = 0; j < n_i[*i]; j++){
    coord = i_j[*i + *n * j]; // index of l-th observation of j-th individual within g-th cluster
    // Numeric variables
    
    shift_nY = 0;   // index within outcomes Y
    t = 0;          // index within b_i
    for(y = 0; y < nY[0]; y++){
      // Update predictor
      auxcoord = 4*(coord + *N * y);
      eta  = predictor_num[auxcoord];     // fixed part of predictor, which is stable
      eta += predictor_num[1 + auxcoord]; // group-specific part
      eta += predictor_num[3 + auxcoord]; // offset part
      
      for(l = 0; l < nran[shift_nY]; l++){
        // random part of predictor, that needs to be updated wrt new b_i
        eta += theta[t] * X[coord + *N * FormulaR[t]]; 
        t++;
      }
      
      auxY = Y[coord + *N * shift_nY] - eta;
      //if(*i < 5){printf("logpYi: i = %d, j = %d, y = %d", *i, j, y);}
      //if(*i < 5){printf("logpYi: i = %d, j = %d, y = %d, auxY = %f", *i, j, y, auxY);}
      //auxY = ptau[y] * auxY * auxY;
      //if(*i < 5){printf("\nlogpYi: i = %d, j = %d, y = %d, auxY = %f", *i, j, y, auxY);}
      *value -= 0.5 * ptau[y] * auxY * auxY; // subtracting the square
      
      shift_nY++;
    }
    
    //printf("\nvalue = %f", *value);
    //fflush(stdout);
    
    // Poisson outcomes
    for(y = 0; y < nY[1]; y++){
      // Update predictor
      auxcoord = 4*(coord + *N * y);
      eta  = predictor_poi[auxcoord];     // fixed part of predictor, which is stable
      eta += predictor_poi[1 + auxcoord]; // group-specific part
      eta += predictor_poi[3 + auxcoord]; // offset part
      
      for(l = 0; l < nran[shift_nY]; l++){
        // random part of predictor, that needs to be updated wrt new b_i
        eta += theta[t] * X[coord + *N * FormulaR[t]]; 
        t++;
      }
      
      *value += Y[coord + *N * shift_nY]*eta - exp(eta);
      
      shift_nY++;
    }
    
    //printf("\nvalue = %f", *value);
    //fflush(stdout);
    
    // Binary variables
    for(y = 0; y < nY[2]; y++){
      // Update predictor
      auxcoord = 4*(coord + *N * y);
      eta  = predictor_bin[auxcoord];     // fixed part of predictor, which is stable
      eta += predictor_bin[1 + auxcoord]; // group-specific part
      eta += predictor_bin[3 + auxcoord]; // offset part
      
      for(l = 0; l < nran[shift_nY]; l++){
        // random part of predictor, that needs to be updated wrt new b_i
        eta += theta[t] * X[coord + *N * FormulaR[t]]; 
        t++;
      }
      
      *value += Y[coord + *N * shift_nY] * eta - log(1+exp(eta));
      shift_nY++;
    }
    
    //printf("\nvalue = %f", *value);
    //fflush(stdout);
    
    // Ordinal variables
    add_to_c = 0;
    for(y = 0; y < nY[3]; y++){
      // Update predictor
      auxcoord = 4*(coord + *N * y);
      eta  = predictor_ord[auxcoord];     // fixed part of predictor, which is stable
      eta += predictor_ord[1 + auxcoord]; // group-specific part
      eta += predictor_ord[3 + auxcoord]; // offset part
      
      for(l = 0; l < nran[shift_nY]; l++){
        // random part of predictor, that needs to be updated wrt new b_i
        eta += theta[t] * X[coord + *N * FormulaR[t]]; 
        t++;
      }
      
      k = Y[coord + *N * shift_nY]; // double to int, requires values in {0, ..., Kord[y]}
      // Subtract the intercept for the category within
      //if(*i < 5){printf("\nlogpYi Ord: i=%d, j=%d, y=%d, k=%d, eta=%f", 
      //   *i, j, y, k, eta);}
      /*
      if(k == 0){
        pk_1 = 1.0;
      }else{
        etak_1 = eta - pc[add_to_c + k-1];
        pk_1 = logit_inv(etak_1);
        //if(*i < 5){printf("\nlogpYi Ord: i=%d, j=%d, y=%d, k=%d, ck_1=%f, etak_1=%f, pk_1=%f", 
        //   *i, j, y, k, pc[add_to_c + k-1], etak_1, pk_1);}
      }
      if(k == Kord[y]){
        pk = 0.0;
      }else{
        etak = eta - pc[add_to_c + k];
        pk = logit_inv(etak);
        //if(*i < 5){printf("\nlogpYi Ord: i=%d, j=%d, y=%d, k=%d, ck=%f, etak=%f, pk=%f", 
        //   *i, j, y, k, pc[add_to_c + k], etak, pk);}
      }
      //if(*i < 5){printf("\nlogpYi Ord: i=%d, j=%d, y=%d, k=%d, etak=%f, etak_1=%f, pk=%f, pk_1=%f", 
      //   *i, j, y, k, etak, etak_1, pk, pk_1);}
      qk = pk_1 - pk + 0.00000001;
      //if(*i < 5){printf("\nlogpYi Ord: i=%d, j=%d, y=%d, k=%d, qk=%f, logqk=%f", 
      //   *i, j, y, k, qk, log(qk));}
      *value += log(qk);
      **/
      
      // Numerically stable version
      /*
      if(k == 0){
        pk_1 = 0.0;                      // complementary probability
        etak = eta - pc[add_to_c + k];
        pk = 1.0 / (1.0 + exp(etak));    // complementary probability
        qk = pk - pk_1;
      }else{
        if(k == Kord[y]){
          pk = 0.0;
          etak_1 = eta - pc[add_to_c + k-1];
          pk_1 = 1.0 / (1.0 + exp(-etak_1));
          qk = pk_1 - pk;
        }else{
          etak = eta - pc[add_to_c + k];
          etak_1 = eta - pc[add_to_c + k-1];
          
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
      qk = ord_qk(k, eta, pc + add_to_c, Kord + y);
      *value += log(qk);
      
      shift_nY++;
      add_to_c += Kord[y];
    }
    
    //printf("\nvalue = %f", *value);
    //fflush(stdout);
    
    // Categorical variables
    cumKcat = 0;
    for(y = 0; y < nY[4]; y++){
      // Update predictor
      maxeta = 0.0;
      tstart = t;
      for(k = 0; k < Kcat[y]; k++){
        auxcoord = 4*(coord + *N * cumKcat);
        etas[k]  = predictor_cat[auxcoord ];     // fixed part of predictor, which is stable
        etas[k] += predictor_cat[1 + auxcoord ]; // group-specific part
        etas[k] += predictor_cat[3 + auxcoord ]; // offset part
        
        if(*kspec_bi_cat){ //k-specific bi
          // continue in increasing t
        }else{
          // not k-specific bi
          t = tstart; // get back where starts bi for this y
        } 
        for(l = 0; l < nran[shift_nY]; l++){
          etas[k] += theta[t] * X[coord + *N * FormulaR[l + cumnran[shift_nY]]];
          t++;
        }
        
        if(maxeta < etas[k]){
          maxeta = etas[k];
        }
        
        cumKcat++; // updates by 1 to another set of predictors
      }
      log_sum_exp = exp(-maxeta);
      for(k = 0; k < Kcat[y]; k++){
        if(Y[coord + *N * shift_nY] == k){
          *value += etas[k]; // if one of first K-1 categories, add corresponding predictor
        }
        // subtract the maximal value before exponentiating
        log_sum_exp += exp(etas[k] - maxeta);
      }
      log_sum_exp = log(log_sum_exp);
      log_sum_exp += maxeta;
      
      // in ALL cases, even Y==K-1 (last category), subtract log_sum_exp
      *value -= log_sum_exp;

      shift_nY++;
    } // end of categorical variables
    
    //printf("\nvalue = %f", *value);
    //fflush(stdout);
    
  } // end of for j in 1:n_i
  
  free(etas);
}

void logpbi(struct str_state* last,  // IN last known values of generated parameters
            double* hyperparameter,   // not needed pointer to potential hyperparameter
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
            int* nY,                // IN [5] counts of outcomes
            int* N,                 // IN [1] number of observations
            int* n,                 // IN [1] number of subjects
            int* i,                 // IN [1] index of a subject
            int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
            int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
            int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
            //        FALSE = all levels of cat var have one set of bi - compares last category with others
            int* nran,              // IN [1] number of RANDOM  regressors or each response
            int* cumnran,           // IN [1] where to start in FormulaR
            int* totnran,           // IN [1] total dimension of fixed parameter (useless)
            int* FormulaR,          // IN [totnran]  numbers of columns of X that should be used for RANDOM  effects of modelled responses
            int* n_i,               // IN [n] number of observations dedicated to j-th subject
            int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
            int* nUg,               // IN [G] number of subjects in classes 1, ..., G
            int* listUi,            // IN [n*G] row by row indices of subjects in group g
            double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
            double* value           // OUT [1]
){
  *value = 0.0;
  
  // Auxiliary variables
  //double loglik;
  double bISb;
  int g;
  double* pInvSigma;
  
  g = (*last).U[*i];
  if(spec[2]){
    pInvSigma = (*last).InvSigma + g*dims[16];
  }else{
    pInvSigma = (*last).InvSigma;
  }
  
  //printf("\nlogpbi: n = %d,  n_j[%d] = %d", *n, *i, n_j[*i]);
  logpYi(last, Y, X, spec, dims, 
         predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, 
         nY, N, n, i, Kord, Kcat, kspec_bi_cat, 
         nran, cumnran, totnran, FormulaR, n_i, i_j, nUg, listUi, theta, value);
  
  
  // Add the square of prior distribution -1/2 b_i * InvSigma * b_i
  aBa(theta, pInvSigma, &bISb, totnran);
  *value -= 0.5 * bISb;
  
  
}

void d_d2_logpbi(struct str_state* last,  // IN last known values of generated parameters
                 double* hyperparameter,   // not needed pointer to potential hyperparameter
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
                 int* nY,                // IN [5] counts of outcomes
                 int* N,                 // IN [1] number of observations
                 int* n,                 // IN [1] number of subjects
                 int* i,                 // IN [1] index of a subject
                 int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
                 int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
                 int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
                 //        FALSE = all levels of cat var have one set of bi - compares last category with others
                 int* nran,              // IN [1] number of RANDOM  regressors or each response
                 int* cumnran,           // IN [1] where to start in FormulaR
                 int* totnran,           // IN [1] total dimension of fixed parameter (useless)
                 int* FormulaR,          // IN [totnran]  numbers of columns of X that should be used for RANDOM  effects of modelled responses
                 int* n_i,               // IN [n] number of observations dedicated to j-th subject
                 int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
                 int* nUg,               // IN [G] number of subjects in classes 1, ..., G
                 int* listUi,            // IN [n*G] row by row indices of subjects in group g
                 double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
                 double* grad,           // OUT [totnran]
                 double* hess            // OUT [totnran*(totnran+1)/2]
){
  // Auxiliary variables
  int j, k, k1, k2, l1, l2, g, y, t, t2, tstart;
  int coord, auxcoord, row, col;
  double eta, auxY, auxZ;
  double expeta, Yminee;
  double logitinv_eta, dli_eta;
  int shift_nY = 0;
  double* ptau;
  double* pc;
  double* pInvSigma;
  double etak, etak_1, pp1, pp;
  double pk, pk_1, ppk, ppk_1;
  int add_to_c;
  int maxKcat = 0;
  for(y = 0; y < nY[4]; y++){
    if(maxKcat < Kcat[y]){
      maxKcat = Kcat[y];
    }
  }
  int cumKcat;
  double* etas;
  etas = (double*)malloc(maxKcat * sizeof(double));
  //double etas[maxKcat];
  double maxeta;
  double* exp_eta;
  exp_eta = (double*)malloc(maxKcat* sizeof(double));
  //double exp_eta[maxKcat];
  double* softmax;
  softmax = (double*)malloc(maxKcat* sizeof(double));
  //double softmax[maxKcat];
  double sum_exp, mult_coef_grad, mult_coef_hess, coef_K_1;
  
  g = (*last).U[*i]; // group to which i-th subject belongs
  if(spec[0]){
    // prec_num is g-specific
    ptau = (*last).prec_num + g*dims[2];
  }else{
    ptau = (*last).prec_num;
  }
  
  if(spec[1]){
    // a_ord and c_ord are g-specific
    pc = (*last).c_ord + g*dims[11];
  }else{
    pc = (*last).c_ord;
  }
  
  if(spec[2]){
    // InvSigma is g-specific
    pInvSigma = (*last).InvSigma + g*dims[16];
  }else{
    pInvSigma = (*last).InvSigma;
  }
  
  // prior distribution as a starting point
  // gradient
  minusBa(theta, pInvSigma, grad, totnran);
  // hessian
  for(l1 = 0; l1 < dims[16]; l1++){
    hess[l1] = pInvSigma[l1];
  }
  
  //printf("\nd_d2_logpbi: priors are set");
  
  for(j = 0; j < n_i[*i]; j++){
    coord = i_j[*i + *n * j]; // index of l-th observation of j-th individual within g-th cluster
    //printf("\nd_d2_logpbi: coord[%d] = %d", j, coord);
    
    // Numeric variables
    shift_nY = 0;   // index within outcomes Y
    t = 0;          // index within b_i
    for(y = 0; y < nY[0]; y++){
      tstart = t2 = t;
      //printf("\nd_d2_logpbi: Nums[%d]", y);
      // Update predictor
      auxcoord = 4*(coord + *N * y);
      eta  = predictor_num[auxcoord];     // fixed part of predictor, which is stable
      eta += predictor_num[1 + auxcoord]; // group-specific part
      eta += predictor_num[3 + auxcoord]; // offset part
      
      for(l1 = 0; l1 < nran[shift_nY]; l1++){
        // random part of predictor, that needs to be updated wrt new b_i
        eta += theta[t] * X[coord + *N * FormulaR[t]]; 
        t++;
      }
      auxY = Y[coord + *N * shift_nY] - eta;
      
      for(l1 = 0; l1 < nran[shift_nY]; l1++){
        // gradient update
        auxZ = X[coord + *N * FormulaR[t2]];
        grad[t2] += ptau[y] * auxY * auxZ;
        // hessian update
        for(l2 = l1; l2 < nran[shift_nY]; l2++){
          col = (tstart+l2)*(tstart+l2+1)/2;
          hess[t2 + col] += ptau[y] * auxZ * X[coord + *N * FormulaR[l2 + cumnran[shift_nY]]];
          //printf("\nd_d2_logpbi: j = %d, t = %d, col = %d, hess[%d] = %f", j, t, col, t+col, hess[t+col]);
        }
        t2++;
      }
      //printf("\nd_d2_logpbi: j = %d, t = %d", j, t);
      shift_nY++;
    }
    //printf("\nd_d2_logpbi: Nums are added");
    
    // Poisson outcomes
    for(y = 0; y < nY[1]; y++){
      tstart = t2 = t;
      // Update predictor
      auxcoord = 4*(coord + *N * y);
      eta  = predictor_poi[auxcoord];     // fixed part of predictor, which is stable
      eta += predictor_poi[1 + auxcoord]; // group-specific part
      eta += predictor_poi[3 + auxcoord]; // offset part
      
      for(l1 = 0; l1 < nran[shift_nY]; l1++){
        // random part of predictor, that needs to be updated wrt new b_i
        eta += theta[t] * X[coord + *N * FormulaR[t]]; 
        t++;
      }
      expeta = exp(eta);
      Yminee = Y[coord + *N * shift_nY] - expeta;
      
      for(l1 = 0; l1 < nran[shift_nY]; l1++){
        auxZ = X[coord + *N * FormulaR[t2]];
        grad[t2] += Yminee * auxZ;
        for(l2 = l1; l2 < nran[shift_nY]; l2++){
          col = (tstart+l2)*(tstart+l2+1)/2;
          hess[t2+col] += expeta * auxZ * X[coord + *N * FormulaR[l2 + cumnran[shift_nY]]];
        }
        t2++;
      }
      
      shift_nY++;
    }
    
    // Binary variables
    for(y = 0; y < nY[2]; y++){
      tstart = t2 = t;
      //printf("\nd_d2_logpbi: Bins[%d]", y);
      // Update predictor
      auxcoord = 4*(coord + *N * y);
      eta  = predictor_bin[auxcoord];     // fixed part of predictor, which is stable
      eta += predictor_bin[1 + auxcoord]; // group-specific part
      eta += predictor_bin[3 + auxcoord]; // offset part
      
      for(l1 = 0; l1 < nran[shift_nY]; l1++){
        // random part of predictor, that needs to be updated wrt new b_i
        eta += theta[t] * X[coord + *N * FormulaR[t]]; 
        t++;
      }
      
      logitinv_eta = logit_inv(eta);
      dli_eta = logitinv_eta * (1.0-logitinv_eta);
      
      for(l1 = 0; l1 < nran[shift_nY]; l1++){
        // gradient
        auxZ = X[coord + *N * FormulaR[t2]];
        grad[t2] += (Y[coord + *N * shift_nY] - logitinv_eta) * auxZ;
        // hessian
        for(l2 = l1; l2 < nran[shift_nY]; l2++){
          col = (tstart+l2)*(tstart+l2+1)/2;
          hess[t2 + col] += dli_eta * auxZ * X[coord + *N * FormulaR[l2 + cumnran[shift_nY]]];
          //printf("\nd_d2_logpbi: j = %d, t = %d, t2 = %d, col = %d, hess[%d] = %f", j, t, t2, col, t2+col, hess[t2+col]);
        }
        t2++;
      }
      //printf("\nd_d2_logpbi: j = %d, t = %d, t2 = %d", j, t, t2);
      shift_nY++;
    }
    //printf("\nd_d2_logpbi: Bins are added");
    
    // Ordinal variables
    add_to_c = 0;
    for(y = 0; y < nY[3]; y++){
      tstart = t2 = t;
      //printf("\nd_d2_logpbi: Ords[%d]", y);
      // Update predictor
      auxcoord = 4*(coord + *N * y);
      eta  = predictor_ord[auxcoord];     // fixed part of predictor, which is stable
      eta += predictor_ord[1 + auxcoord]; // group-specific part
      eta += predictor_ord[3 + auxcoord]; // offset part
      
      for(l1 = 0; l1 < nran[shift_nY]; l1++){
        // random part of predictor, that needs to be updated wrt new b_i
        eta += theta[t] * X[coord + *N * FormulaR[t]]; 
        t++;
      }
      
      k = Y[coord + *N * shift_nY]; // double to int, requires values in {0, ..., Kord[y]}
      // Subtract the intercept for the category within
      if(k == 0){
        pk_1 = 1.0;
        ppk_1 = 0.0;
      }else{
        etak_1 = eta - pc[add_to_c + k-1];
        pk_1 = logit_inv(etak_1);
        ppk_1 = pk_1 * (1-pk_1);
      }
      if(k == Kord[y]){
        pk = 0.0;
        ppk = 0.0;
      }else{
        etak = eta - pc[add_to_c + k];
        pk = logit_inv(etak);
        ppk = pk * (1-pk);
      }
      pp1 = 1.0 - pk_1 - pk;
      pp = ppk + ppk_1;
      
      for(l1 = 0; l1 < nran[shift_nY]; l1++){
        // gradient
        auxZ = X[coord + *N * FormulaR[t2]];
        grad[t2] += pp1 * auxZ;
        // hessian
        for(l2 = l1; l2 < nran[shift_nY]; l2++){
          col = (tstart+l2)*(tstart+l2+1)/2;
          hess[t2 + col] += pp * auxZ * X[coord + *N * FormulaR[l2 + cumnran[shift_nY]]];
          //printf("\nd_d2_logpbi: j = %d, t = %d, t2 = %d, col = %d, hess[%d] = %f", j, t, t2, col, t2+col, hess[t2+col]);
        }
        t2++;
      }
      
      //printf("\nd_d2_logpbi: j = %d, t = %d, t2 = %d", j, t, t2);
      shift_nY++;
      add_to_c += Kord[y];
    }
    //printf("\nd_d2_logpbi: Ords are added");
    
    // Categorical variables
    cumKcat = 0;
    for(y = 0; y < nY[4]; y++){
      tstart = t2 = t;
      //printf("\nd_d2_logpbi: Cats[%d]", y);
      // Update predictor
      maxeta = 0.0;
      for(k = 0; k < Kcat[y]; k++){
        auxcoord = 4*(coord + *N * cumKcat);
        etas[k]  = predictor_cat[auxcoord];     // fixed part of predictor, which is stable
        etas[k] += predictor_cat[1 + auxcoord]; // group-specific part
        etas[k] += predictor_cat[3 + auxcoord]; // offset part
        
        if(!(*kspec_bi_cat)){ // not k-specific bi
          t = tstart; // get back where starts bi for this y
        } // else continue in increasing t
        for(l1 = 0; l1 < nran[shift_nY]; l1++){
          etas[k] += theta[t] * X[coord + *N * FormulaR[l1 + cumnran[shift_nY]]];
          t++;
        }
        if(maxeta < etas[k]){
          maxeta = etas[k];
        }
        cumKcat++; // updates by 1 to another set of predictors
      }
      sum_exp = exp(-maxeta);
      for(k = 0; k < Kcat[y]; k++){
        exp_eta[k] = exp(etas[k] - maxeta);
        sum_exp += exp_eta[k];
      }
      
      // softmax computation
      for(k = 0; k < Kcat[y]; k++){
        softmax[k] = exp_eta[k]/sum_exp;
      }
      
      if(*kspec_bi_cat){
        // each category has its own set of bi parameters - block matrix
        for(k1 = 0; k1 < Kcat[y]; k1++){
          tstart = t2; // move start to the beginning of next category
          // do diagonal k1=k2 first
          mult_coef_grad = (Y[coord + *N * shift_nY] == k1) - softmax[k1];
          mult_coef_hess = softmax[k1] * (1-softmax[k1]);
          for(l1 = 0; l1 < nran[shift_nY]; l1++){
            // gradient
            auxZ = X[coord + *N * FormulaR[l1 + cumnran[shift_nY]]];
            grad[t2] += mult_coef_grad * auxZ;
            // hessian
            for(l2 = l1; l2 < nran[shift_nY]; l2++){
              //printf("\nd_d2_logpbi: j = %d, k1 = %d, l1 = %d, l2 = %d", j, k1, l1, l2);
              col = (tstart+l2)*(tstart+l2+1)/2;
              hess[t2 + col] += mult_coef_hess * auxZ * X[coord + *N * FormulaR[l2 + cumnran[shift_nY]]];
              //printf("\nd_d2_logpbi: tstart = %d, t = %d, t2 = %d, col = %d, hess[%d] = %f", 
              //       tstart, t, t2, col, t2+col, hess[t2+col]);
            }
            t2++;
          }
          
          // now off-diagonal --> only hess, grad is already complete
          for(k2 = k1+1; k2 < Kcat[y]; k2++){
            mult_coef_hess = softmax[k1] * softmax[k2];
            //printf("\nd_d2_logpbi: softmax[%d] = %f, softmax[%d] = %f, mult_coef_hess = %f",
            //       k1, softmax[k1], k2, softmax[k2], mult_coef_hess);
            for(l1 = 0; l1 < nran[shift_nY]; l1++){
              auxZ = X[coord + *N * FormulaR[l1 + cumnran[shift_nY]]];
              row = l1 + tstart; // tstart is already where k1 starts
                
              for(l2 = 0; l2 < nran[shift_nY]; l2++){
                //printf("\nd_d2_logpbi: j = %d, k1 = %d, k2 = %d, l1 = %d, l2 = %d", j, k1, k2, l1, l2);
                // m2 from 0, because we do rectangle, not upper triangle
                col = nran[shift_nY] * (k2-k1) + l2 + tstart; // tstart points to diagonal--> shift by multiple of Kcat[y]
                k = row + col*(col+1)/2;
                hess[k] -= mult_coef_hess * auxZ * X[coord + *N * FormulaR[l2 + cumnran[shift_nY]]];
                // its negative off the block-diagonal --> -
                //printf("\nd_d2_logpbi: tstart = %d, t = %d, t2 = %d, row = %d, col = %d, hess[%d] = %f", 
                //       tstart, t, t2, row, col, k, hess[k]);
              }
            }
          }
          // end of off-diagonal 
        }
      }else{
        // there is just one bi per categorical outcome --> only one upper triangular block of Sigma to be added
        // do only k1=k2=K-1
        coef_K_1 = (sum_exp-exp(-maxeta))/sum_exp;
        mult_coef_grad = (Y[coord + *N * shift_nY] != Kcat[y]) - coef_K_1; // subtract p(Y=K-1) from indicator of last category K-1
        mult_coef_hess = coef_K_1*exp(-maxeta)/sum_exp;
        for(l1 = 0; l1 < nran[shift_nY]; l1++){
          // gradient
          auxZ = X[coord + *N * FormulaR[l1 + cumnran[shift_nY]]];
          grad[t2] += mult_coef_grad * auxZ;
          // hessian
          for(l2 = l1; l2 < nran[shift_nY]; l2++){
            //printf("\nd_d2_logpbi: j = %d, k1 = %d, l1 = %d, l2 = %d", j, Kcat[y], l1, l2);
            col = (tstart+l2)*(tstart+l2+1)/2;
            hess[t2 + col] += mult_coef_hess * auxZ * X[coord + *N * FormulaR[l2 + cumnran[shift_nY]]];
            //printf("\nd_d2_logpbi: tstart = %d, t = %d, t2 = %d, col = %d, hess[%d] = %f", 
            //       tstart, t, t2, col, t2+col, hess[t2+col]);
          }
          t2++;
        }

      }
      
      
      shift_nY++;
    } // end of categorical variables
    //printf("\nd_d2_logpbi: Cats are added");
    
  } // end of for j in 1:n_i 
  
  
  free(etas);
  free(exp_eta);
  free(softmax);
  
}
