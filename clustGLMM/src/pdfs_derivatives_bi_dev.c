/*
 * Functions for logs of full conditional pdfs and their derivatives
 */

#include <R.h>
#include <Rmath.h>
#include <stdio.h>

#include "matrmult.h"
#include "my_math.h"

void logpYi_dev(double* Y,                // IN [totnY*N]
                int* isYna,               // [N*sum(nY)]  0 = Y value present, 1 = Y value is NA
                double* X,                // IN [N*(#regr)]    regressors
                double* pbeta_num_fix,     // pointer to beta_num_fix
                double* pbeta_num,         // pointer to beta_num
                double* pprec_num,         // pointer to prec_num
                double* pbeta_poi_fix,     // pointer to beta_poi_fix
                double* pbeta_poi,         // pointer to beta_poi
                double* pbeta_bin_fix,     // pointer to beta_bin_fix
                double* pbeta_bin,         // pointer to beta_bin
                double* pbeta_ord_fix,     // pointer to beta_ord_fix
                double* pbeta_ord,         // pointer to beta_ord
                double* pc_ord,            // pointer to c_ord
                double* pbeta_cat_fix,     // pointer to beta_cat_fix
                double* pbeta_cat,         // pointer to beta_cat
                double* pInvSigma,         // pointer to InvSigma
                int* N,                   // IN [1] number of observations
                int* n,                   // IN [1] number of subjects
                int* nY,                  // IN [4]
                int* i,                   // IN [1] number of subject 
                int* Kord,                // IN [nY[2]] total number of categories of Ord outcomes -1
                int* Kcat,                // IN [nY[3]] total number of categories of Cat outcomes -1
                int* nfix,                 // IN [totnY] number of FIXED  regressors or each response
                int* cumnfix,              // IN [totnY] where to start in FormulaF
                int* FormulaF,             // IN [totnfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
                int* ngrp,                 // IN [totnY] number of GROUP-SPECIFIC  regressors or each response
                int* cumngrp,              // IN [totnY] where to start in FormulaG
                int* FormulaG,             // IN [totnfix]  numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
                int* nran,                 // IN [totnY] number of RANDOM  regressors or each response
                int* cumnran,              // IN [totnY] where to start in FormulaR
                int* totnran,              // IN [1] total dimension of b_i
                int* FormulaR,             // IN [totnran]  numbers of columns of X that should be used for RANDOM  effects of modelled responses
                int* noff,                 // IN [totnY] number of OFFSET  regressors or each response
                int* cumnoff,              // IN [totnY] where to start in FormulaO
                int* FormulaO,             // IN [totnran]  numbers of columns of X that should be used for OFFSET  effects of modelled responses
                int* ij,                  // IN [1] number of first ij observations of j-th subject to be used
                int* i_j,                 // IN [n * max_n_j] indeces dedicated to j-th subject 
                int* kspec_bi_cat,        // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
                //        FALSE = all levels of cat var have one set of bi - compares last category with others
                int* print,
                double* theta,            // IN [dims[.]] the parameter in which the function is evaluated
                double* value             // OUT [1]
){
  // Auxiliary variables
  int j, k, l, y, t, tstart, tf, tg;
  int coord;
  double eta, auxY;
  int shift_nY = 0;
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
  etas = (double*)malloc(maxKcat * sizeof(double));
  //double etas[maxKcat];
  double* expetas;
  expetas = (double*)malloc(maxKcat * sizeof(double));
  //double expetas[maxKcat];
  double log_sum_exp;
  double maxeta;
  
  double* half_log_prec;
  half_log_prec = (double *)malloc(nY[0] * sizeof(double));
  
  for(y = 0; y < nY[0]; y++){
    half_log_prec[y] = 0.5 * log(pprec_num[y]);
  }
  
  
  *value = 0.0;
  
  for(j = 0; j < *ij; j++){
    coord = i_j[*i + *n * j]; // index of l-th observation of j-th individual within g-th cluster
    // Numeric variables
    
    shift_nY = 0;   // index within outcomes Y
    t = 0;          // index within b_i
    tf = tg = 0;
    for(y = 0; y < nY[0]; y++){
      if(isYna[coord + *N * shift_nY]){
        // add nothing, but update indices
        tf += nfix[shift_nY];
        tg += ngrp[shift_nY];
        t += nran[shift_nY];
      }else{
        // log-normalizing constant 
        *value -= HALF_LOG_2PI;
        
        // Do not forget on log tau!!!
        *value += half_log_prec[y];
        
        // Update predictor
        eta = 0.0; 
        
        for(l = 0; l < nfix[shift_nY]; l++){
          // fixed part of predictor created by beta_num_fix and covariates
          eta += pbeta_num_fix[tf] * X[coord + *N * FormulaF[l + cumnfix[shift_nY]]]; 
          tf++;
        }
        
        for(l = 0; l < ngrp[shift_nY]; l++){
          // group-specific part of predictor created by beta_num and covariates
          eta += pbeta_num[tg] * X[coord + *N * FormulaG[l + cumngrp[shift_nY]]]; 
          tg++;
        }
        
        for(l = 0; l < nran[shift_nY]; l++){
          // random part of predictor, that needs to be updated wrt new b_i
          eta += theta[t] * X[coord + *N * FormulaR[t]]; 
          t++;
        }
        
        for(l = 0; l < noff[shift_nY]; l++){
          // offset part of predictor
          eta += X[coord + *N * FormulaO[l + cumnoff[shift_nY]]]; 
        }
        
        auxY = Y[coord + *N * shift_nY] - eta;
        //auxY = ptau[y] * auxY * auxY;
        *value -= 0.5 * pprec_num[y] * auxY * auxY; // subtracting the square
        //if(*print){printf("\nlogpYi Num: i = %d, j = %d, y = %d, eta = %f, auxY = %f, value = %f", *i, j, y, eta, auxY, *value);}
        
      }
      shift_nY++;
    }
    
    //printf("\nlogpYi_dev: After Nums: j = %d, value = %f", j, *value);
    
    // Poisson outcomes
    tf = tg = 0;
    for(y = 0; y < nY[1]; y++){
      if(isYna[coord + *N * shift_nY]){
        // add nothing, but update indices
        tf += nfix[shift_nY];
        tg += ngrp[shift_nY];
        t += nran[shift_nY];
      }else{
        // Update predictor
        eta = 0.0; 
        
        for(l = 0; l < nfix[shift_nY]; l++){
          // fixed part of predictor created by beta_poi_fix and covariates
          eta += pbeta_poi_fix[tf] * X[coord + *N * FormulaF[l + cumnfix[shift_nY]]]; 
          tf++;
        }
        
        for(l = 0; l < ngrp[shift_nY]; l++){
          // group-specific part of predictor created by beta_poi and covariates
          eta += pbeta_poi[tg] * X[coord + *N * FormulaG[l + cumngrp[shift_nY]]]; 
          tg++;
        }
        
        for(l = 0; l < nran[shift_nY]; l++){
          // random part of predictor, that needs to be updated wrt new b_i
          eta += theta[t] * X[coord + *N * FormulaR[t]]; 
          t++;
        }
        
        for(l = 0; l < noff[shift_nY]; l++){
          // offset part of predictor
          eta += X[coord + *N * FormulaO[l + cumnoff[shift_nY]]]; 
        }
        
        *value += Y[coord + *N * shift_nY]*eta - exp(eta); 
        // normalizing constant -log(y!)
        *value -= lgamma(Y[coord + *N * shift_nY] + 1);
        //if(*print){printf("\nlogpYi Poi: i = %d, j = %d, y = %d, eta = %f, value = %f", *i, j, y, eta, *value);}
      }
      shift_nY++;
    }
    
    tf = tg = 0;
    // Binary variables
    for(y = 0; y < nY[2]; y++){
      if(isYna[coord + *N * shift_nY]){
        // add nothing, but update indices
        tf += nfix[shift_nY];
        tg += ngrp[shift_nY];
        t += nran[shift_nY];
      }else{
        // Update predictor
        eta = 0.0; 
        
        for(l = 0; l < nfix[shift_nY]; l++){
          // fixed part of predictor created by beta_bin_fix and covariates
          eta += pbeta_bin_fix[tf] * X[coord + *N * FormulaF[l + cumnfix[shift_nY]]]; 
          tf++;
        }
        
        for(l = 0; l < ngrp[shift_nY]; l++){
          // group-specific part of predictor created by beta_bin and covariates
          eta += pbeta_bin[tg] * X[coord + *N * FormulaG[l + cumngrp[shift_nY]]]; 
          tg++;
        }
        
        for(l = 0; l < nran[shift_nY]; l++){
          // random part of predictor, that needs to be updated wrt new b_i
          eta += theta[t] * X[coord + *N * FormulaR[t]]; 
          t++;
        }
        
        for(l = 0; l < noff[shift_nY]; l++){
          // offset part of predictor
          eta += X[coord + *N * FormulaO[l + cumnoff[shift_nY]]]; 
        }
        
        *value += Y[coord + *N * shift_nY] * eta - safe_log1pexp(eta); // log(1+exp(eta));
        //if(*print){printf("\nlogpYi Bin: i = %d, j = %d, y = %d, eta = %f, value = %f", *i, j, y, eta, *value);}
      }
      shift_nY++;
    }
    //printf("\nlogpYi_dev: After Bins: j = %d, value = %f", j, *value);
             
    // Ordinal variables
    add_to_c = 0;
    tf = tg = 0;
    for(y = 0; y < nY[3]; y++){
      if(isYna[coord + *N * shift_nY]){
        // add nothing, but update indices
        tf += nfix[shift_nY];
        tg += ngrp[shift_nY];
        t += nran[shift_nY];
      }else{
        // Update predictor
        eta = 0.0; 
        
        for(l = 0; l < nfix[shift_nY]; l++){
          // fixed part of predictor created by beta_ord_fix and covariates
          eta += pbeta_ord_fix[tf] * X[coord + *N * FormulaF[l + cumnfix[shift_nY]]]; 
          tf++;
        }
        if(*print){
          //printf("\nlogpYi Ord fix: i = %d, j = %d, y = %d, eta = %f",
          //       *i, j, y, eta);
        }
        
        for(l = 0; l < ngrp[shift_nY]; l++){
          // group-specific part of predictor created by beta_ord and covariates
          eta += pbeta_ord[tg] * X[coord + *N * FormulaG[l + cumngrp[shift_nY]]]; 
          tg++;
        }
        if(*print){
          //printf("\nlogpYi Ord grp: i = %d, j = %d, y = %d, eta = %f",
          //       *i, j, y, eta);
        }
        
        for(l = 0; l < nran[shift_nY]; l++){
          // random part of predictor, that needs to be updated wrt new b_i
          eta += theta[t] * X[coord + *N * FormulaR[t]]; 
          if(*print){
            //printf("\nlogpYi Ord ran: i = %d, j = %d, y = %d, eta = %f, theta[%d] = %f, X[%d] = %f",
            //       *i, j, y, eta, 
            //       t, theta[t],
            //       coord + *N * FormulaR[t], 
            //       X[coord + *N * FormulaR[t]]);
          }
          t++;
        }
        if(*print){
          //printf("\nlogpYi Ord ran: i = %d, j = %d, y = %d, eta = %f",
          //       *i, j, y, eta);
        }
        
        for(l = 0; l < noff[shift_nY]; l++){
          // offset part of predictor
          eta += X[coord + *N * FormulaO[l + cumnoff[shift_nY]]]; 
        }
        
        //printf("\nlogpYi_dev: Ords fixed+random: y = %d, eta = %f", y, eta);
        k = Y[coord + *N * shift_nY]; // double to int, requires values in {0, ..., Kord[y]}
        // Subtract the intercept for the category within
        //printf("\nlogpYi_dev: k = %d, add_to_c = %d, pc_ord k-1 = %f, pc_ord k = %f", 
        //       k, add_to_c, pc_ord[add_to_c + k-1], pc_ord[add_to_c + k]);
        /*
        if(k == 0){
          pk_1 = 1.0;
        }else{
          etak_1 = eta - pc_ord[add_to_c + k-1];
          pk_1 = logit_inv(etak_1);
        }
        if(k == Kord[y]){
          pk = 0.0;
        }else{
          etak = eta - pc_ord[add_to_c + k];
          pk = logit_inv(etak);
        }
        //printf("\nlogpYi_dev: Ords: y = %d, pk_1 = %f, pk = %f, qk = %f", y, pk_1, pk, qk);
        qk = pk_1 - pk + 0.00000001;
        *value += log(qk);
        */
        
        // Numerically stable version
        /*
        if(k == 0){
          pk_1 = 0.0;                      // complementary probability
          etak = eta - pc_ord[add_to_c + k];
          pk = 1.0 / (1.0 + exp(etak));    // complementary probability
          qk = pk - pk_1;
        }else{
          if(k == Kord[y]){
            pk = 0.0;
            etak_1 = eta - pc_ord[add_to_c + k-1];
            pk_1 = 1.0 / (1.0 + exp(-etak_1));
            qk = pk_1 - pk;
          }else{
            etak = eta - pc_ord[add_to_c + k];
            etak_1 = eta - pc_ord[add_to_c + k-1];
            
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
        qk = ord_qk(k, eta, pc_ord, Kord + y);
        *value += log(qk);
        //if(*print){printf("\nlogpYi Ord: i = %d, j = %d, y = %d, eta = %f, pk_1 = %f, pk = %f, qk = %f, value = %f", 
        //   *i, j, y, eta, pk_1, pk, qk, *value);}
      }
      
      shift_nY++;
      add_to_c += Kord[y];
    }
    //printf("\nlogpYi_dev: After Ords: j = %d, value = %f", j, *value);
    
    // Categorical variables
    cumKcat = 0;
    tf = tg = 0;
    for(y = 0; y < nY[4]; y++){
      if(isYna[coord + *N * shift_nY]){
        // add nothing, but update indices
        tf += nfix[shift_nY] * Kcat[y];
        tg += ngrp[shift_nY] * Kcat[y];
        if(*kspec_bi_cat){ //k-specific bi
          t += nran[shift_nY] * Kcat[y];
        }else{
          // not k-specific bi
          t += nran[shift_nY]; 
        } 
      }else{
        // Update predictor
        tstart = t;
        maxeta = 0.0;
        for(k = 0; k < Kcat[y]; k++){
          etas[k] = 0.0; 
          
          for(l = 0; l < nfix[shift_nY]; l++){
            // fixed part of predictor created by beta_cat_fix and covariates
            etas[k] += pbeta_cat_fix[tf] * X[coord + *N * FormulaF[l + cumnfix[shift_nY]]]; 
            tf++;
          }
          if(*print){
            //printf("\nlogpYi Cat fix: i = %d, j = %d, y = %d, etas[%d] = %f",
            //       *i, j, y, k, etas[k]);
          }
          
          for(l = 0; l < ngrp[shift_nY]; l++){
            // group-specific part of predictor created by beta_cat and covariates
            etas[k] += pbeta_cat[tg] * X[coord + *N * FormulaG[l + cumngrp[shift_nY]]];
            if(*print){
              //printf("\nlogpYi Cat grp: i = %d, j = %d, y = %d, etas[%d] = %f, beta[%d] = %f, X[%d] = %f",
              //       *i, j, y, k, etas[k], 
              //       tg, pbeta_cat[tg],
              //       coord + *N * FormulaG[l + cumngrp[shift_nY]], 
              //       X[coord + *N * FormulaG[l + cumngrp[shift_nY]]]);
            }
            tg++;
          }
          if(*print){
            //printf("\nlogpYi Cat grp: i = %d, j = %d, y = %d, etas[%d] = %f",
            //       *i, j, y, k, etas[k]);
          }
          
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
          if(*print){
            //printf("\nlogpYi Cat ran: i = %d, j = %d, y = %d, etas[%d] = %f",
            //       *i, j, y, k, etas[k]);
          }
          
          for(l = 0; l < noff[shift_nY]; l++){
            // offset part of predictor
            etas[k] += X[coord + *N * FormulaO[l + cumnoff[shift_nY]]]; 
          }
          
          if(*print){
            //printf("\nlogpYi Cat: i = %d, j = %d, y = %d, etas[%d] = %f, value = %f", 
            // *i, j, y, k, etas[k], *value);
            }
          
          if(maxeta < etas[k]){
            maxeta = etas[k];
          }
          
          //log_sum_exp += exp(etas[k]);
          cumKcat++; // updates by 1 to another set of predictors
        }
        
        log_sum_exp = exp(-maxeta);
        for(k = 0; k < Kcat[y]; k++){
          if(Y[coord + *N * shift_nY] == k){
            *value += etas[k]; // if one of first K-1 categories, add corresponding predictor
          }
          // subtract the maximal value before exponentiating
          expetas[k] = exp(etas[k] - maxeta);
          log_sum_exp += expetas[k];
        }
        log_sum_exp = log(log_sum_exp);
        log_sum_exp += maxeta;
        
        // in ALL cases, even Y==K-1 (last category), subtract log_sum_exp
        *value -= log_sum_exp;
        if(*print){
          //printf("\nlogpYi Cat: i = %d, j = %d, y = %d, log_sum_exp = %f, value = %f", 
          // *i, j, y, log_sum_exp, *value);
          }
      }
      
      shift_nY++;
    } // end of categorical variables
    //printf("\nlogpYi_dev: After Cats: j = %d, value = %f", j, *value);
    
  } // end of for j in 1:n_i
  
  free(etas);
  free(expetas);
  free(half_log_prec);
}

void logpbi_dev(double* Y,              // IN [totnY*N]
                int* isYna,             // [N*sum(nY)]  0 = Y value present, 1 = Y value is NA
                double* X,              // IN [N*(#regr)]    regressors
                double* pbeta_num_fix,     // pointer to beta_num_fix
                double* pbeta_num,         // pointer to beta_num
                double* pprec_num,         // pointer to prec_num
                double* pbeta_poi_fix,     // pointer to beta_poi_fix
                double* pbeta_poi,         // pointer to beta_poi
                double* pbeta_bin_fix,     // pointer to beta_bin_fix
                double* pbeta_bin,         // pointer to beta_bin
                double* pbeta_ord_fix,     // pointer to beta_ord_fix
                double* pbeta_ord,         // pointer to beta_ord
                double* pc_ord,            // pointer to c_ord
                double* pbeta_cat_fix,     // pointer to beta_cat_fix
                double* pbeta_cat,         // pointer to beta_cat
                double* pInvSigma,         // pointer to InvSigma
                int* N,                 // IN [1] number of observations
                int* n,                 // IN [1] number of subjects
                int* nY,                // IN [4]
                int* i,                 // IN [1] number of subject 
                int* Kord,              // IN [nY[2]] total number of categories of Ord outcomes -1
                int* Kcat,              // IN [nY[3]] total number of categories of Cat outcomes -1
                int* nfix,                 // IN [totnY] number of FIXED  regressors or each response
                int* cumnfix,              // IN [totnY] where to start in FormulaF
                int* FormulaF,             // IN [totnfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
                int* ngrp,                 // IN [totnY] number of GROUP-SPECIFIC  regressors or each response
                int* cumngrp,              // IN [totnY] where to start in FormulaG
                int* FormulaG,             // IN [totnfix]  numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
                int* nran,                 // IN [totnY] number of RANDOM  regressors or each response
                int* cumnran,              // IN [totnY] where to start in FormulaR
                int* totnran,              // IN [1] total dimension of b_i
                int* FormulaR,             // IN [totnran]  numbers of columns of X that should be used for RANDOM  effects of modelled responses
                int* noff,                 // IN [totnY] number of OFFSET  regressors or each response
                int* cumnoff,              // IN [totnY] where to start in FormulaO
                int* FormulaO,             // IN [totnran]  numbers of columns of X that should be used for OFFSET  effects of modelled responses
                int* ij,                // IN [1] number of first ij observations of j-th subject to be used
                int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
                int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
                //        FALSE = all levels of cat var have one set of bi - compares last category with others
                int* print,
                double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
                double* value           // OUT [1]
){
  *value = 0.0;
  
  // Auxiliary variables
  //double loglik;
  double bISb;
  
  //printf("\nlogpbi: n = %d,  ij[%d] = %d", *n, *i, *ij);
  logpYi_dev(Y, isYna, X,  
             pbeta_num_fix, pbeta_num, pprec_num, 
             pbeta_poi_fix, pbeta_poi, 
             pbeta_bin_fix, pbeta_bin, 
             pbeta_ord_fix, pbeta_ord, pc_ord, 
             pbeta_cat_fix, pbeta_cat, 
             pInvSigma,
             N, n, nY, i, Kord, Kcat, 
             nfix, cumnfix, FormulaF,
             ngrp, cumngrp, FormulaG,
             nran, cumnran, totnran, FormulaR,
             noff, cumnoff, FormulaO,
             ij, i_j, 
             kspec_bi_cat, print, theta, value);
  //printf("\nlogpbi_dev: value = %f", *value);
  
  // Add the square of prior distribution -1/2 b_i * InvSigma * b_i
  aBa(theta, pInvSigma, &bISb, totnran);
  *value -= 0.5 * bISb;
  *value -= *totnran * HALF_LOG_2PI; // normalizing constant
  //printf("\nlogpbi_dev: value = %f", *value);
  
}

void d_d2_logpbi_dev(double* Y,             // IN [totnY*N]
                     int* isYna,            // [N*sum(nY)]  0 = Y value present, 1 = Y value is NA
                     double* X,             // IN [N*(#regr)]    regressors
                     double* pbeta_num_fix,     // pointer to beta_num_fix
                     double* pbeta_num,         // pointer to beta_num
                     double* pprec_num,         // pointer to prec_num
                     double* pbeta_poi_fix,     // pointer to beta_poi_fix
                     double* pbeta_poi,         // pointer to beta_poi
                     double* pbeta_bin_fix,     // pointer to beta_bin_fix
                     double* pbeta_bin,         // pointer to beta_bin
                     double* pbeta_ord_fix,     // pointer to beta_ord_fix
                     double* pbeta_ord,         // pointer to beta_ord
                     double* pc_ord,            // pointer to c_ord
                     double* pbeta_cat_fix,     // pointer to beta_cat_fix
                     double* pbeta_cat,         // pointer to beta_cat
                     double* pInvSigma,         // pointer to InvSigma
                     int* N,                // IN [1] number of observations
                     int* n,                // IN [1] number of subjects
                     int* nY,               // IN [4]
                     int* i,                // IN [1] number of subject 
                     int* Kord,             // IN [nY[2]] total number of categories of Ord outcomes -1
                     int* Kcat,             // IN [nY[3]] total number of categories of Cat outcomes -1
                     int* nfix,                 // IN [totnY] number of FIXED  regressors or each response
                     int* cumnfix,              // IN [totnY] where to start in FormulaF
                     int* FormulaF,             // IN [totnfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
                     int* ngrp,                 // IN [totnY] number of GROUP-SPECIFIC  regressors or each response
                     int* cumngrp,              // IN [totnY] where to start in FormulaG
                     int* FormulaG,             // IN [totnfix]  numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
                     int* nran,                 // IN [totnY] number of RANDOM  regressors or each response
                     int* cumnran,              // IN [totnY] where to start in FormulaR
                     int* totnran,              // IN [1] total dimension of b_i
                     int* FormulaR,             // IN [totnran]  numbers of columns of X that should be used for RANDOM  effects of modelled responses
                     int* noff,                 // IN [totnY] number of OFFSET  regressors or each response
                     int* cumnoff,              // IN [totnY] where to start in FormulaO
                     int* FormulaO,             // IN [totnran]  numbers of columns of X that should be used for OFFSET  effects of modelled responses
                     int* ij,               // IN [1] number of first ij observations of j-th subject to be used
                     int* i_j,              // IN [n * max_n_j] indeces dedicated to j-th subject 
                     int* kspec_bi_cat,     // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
                     //        FALSE = all levels of cat var have one set of bi - compares last category with others
                     double* theta,         // IN [dims[.]] the parameter in which the function is evaluated
                     double* grad,          // OUT [totnran]
                     double* hess           // OUT [totnran*(totnran+1)/2]
){
  // Auxiliary variables
  int j, k, k1, k2, l1, l2, y, t, t2, tf, tg, tstart;
  int coord, row, col;
  double eta, auxY, auxZ;
  double expeta, Yminee;
  double logitinv_eta, dli_eta;
  int shift_nY = 0;
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
  exp_eta = (double*)malloc(maxKcat * sizeof(double));
  //double exp_eta[maxKcat];
  double* softmax;
  softmax = (double*)malloc(maxKcat * sizeof(double));
  //double softmax[maxKcat];
  double sum_exp, mult_coef_grad, mult_coef_hess, coef_K_1;
  
  // prior distribution as a starting point
  // gradient
  minusBa(theta, pInvSigma, grad, totnran);
  // hessian
  for(l1 = 0; l1 < *totnran * (*totnran+1)/2; l1++){
    hess[l1] = pInvSigma[l1];
  }
  
  //printf("\nd_d2_logpbi: priors are set");
  
  for(j = 0; j < *ij; j++){
    coord = i_j[*i + *n * j]; // index of l-th observation of j-th individual within g-th cluster
    //printf("\nd_d2_logpbi: coord[%d] = %d", j, coord);
    // Numeric variables
    shift_nY = 0;   // index within outcomes Y
    t = 0;          // index within b_i
    tf = tg = 0;         // index within beta
    for(y = 0; y < nY[0]; y++){
      
      if(isYna[coord + *N * shift_nY]){
        // add nothing, but update indices
        tf += nfix[shift_nY];
        tg += ngrp[shift_nY];
        t += nran[shift_nY];
      }else{
        tstart = t2 = t;
        // Update predictor
        eta = 0.0; 
        
        for(l1 = 0; l1 < nfix[shift_nY]; l1++){
          // fixed part of predictor created by beta_num_fix and covariates
          eta += pbeta_num_fix[tf] * X[coord + *N * FormulaF[l1 + cumnfix[shift_nY]]]; 
          tf++;
        }
        
        for(l1 = 0; l1 < ngrp[shift_nY]; l1++){
          // group-specific part of predictor created by beta_num and covariates
          eta += pbeta_num[tg] * X[coord + *N * FormulaG[l1 + cumngrp[shift_nY]]]; 
          tg++;
        }
        
        for(l1 = 0; l1 < nran[shift_nY]; l1++){
          // random part of predictor, that needs to be updated wrt new b_i
          eta += theta[t] * X[coord + *N * FormulaR[t]]; 
          t++;
        }
        
        for(l1 = 0; l1 < noff[shift_nY]; l1++){
          // offset part of predictor
          eta += X[coord + *N * FormulaO[l1 + cumnoff[shift_nY]]]; 
        }
        
        auxY = Y[coord + *N * shift_nY] - eta;
        
        for(l1 = 0; l1 < nran[shift_nY]; l1++){
          // gradient update
          auxZ = X[coord + *N * FormulaR[t2]];
          grad[t2] += pprec_num[y] * auxY * auxZ;
          // log tau not needed (constant differentiated to zero)
          // hessian update
          for(l2 = l1; l2 < nran[shift_nY]; l2++){
            col = (tstart+l2)*(tstart+l2+1)/2;
            hess[t2 + col] += pprec_num[y] * auxZ * X[coord + *N * FormulaR[l2 + cumnran[shift_nY]]];
            //printf("\nd_d2_logpbi: j = %d, t = %d, col = %d, hess[%d] = %f", j, t, col, t+col, hess[t+col]);
          }
          t2++;
        }
      }
      //printf("\nd_d2_logpbi: j = %d, t = %d", j, t);
      shift_nY++;
    }
    //printf("\nd_d2_logpbi: Nums are added");
    
    // Poisson outcomes
    tf = tg = 0;         // index within beta
    for(y = 0; y < nY[1]; y++){
      if(isYna[coord + *N * shift_nY]){
        // add nothing, but update indices
        tf += nfix[shift_nY];
        tg += ngrp[shift_nY];
        t += nran[shift_nY];
      }else{
        tstart = t2 = t;
        // Update predictor
        eta = 0.0; 
        
        for(l1 = 0; l1 < nfix[shift_nY]; l1++){
          // fixed part of predictor created by beta_poi_fix and covariates
          eta += pbeta_poi_fix[tf] * X[coord + *N * FormulaF[l1 + cumnfix[shift_nY]]]; 
          tf++;
        }
        
        for(l1 = 0; l1 < ngrp[shift_nY]; l1++){
          // group-specific part of predictor created by beta_poi and covariates
          eta += pbeta_poi[tg] * X[coord + *N * FormulaG[l1 + cumngrp[shift_nY]]]; 
          tg++;
        }
        
        for(l1 = 0; l1 < nran[shift_nY]; l1++){
          // random part of predictor, that needs to be updated wrt new b_i
          eta += theta[t] * X[coord + *N * FormulaR[t]]; 
          t++;
        }
        
        for(l1 = 0; l1 < noff[shift_nY]; l1++){
          // offset part of predictor
          eta += X[coord + *N * FormulaO[l1 + cumnoff[shift_nY]]]; 
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
      }
      //printf("\nd_d2_logpbi: j = %d, t = %d", j, t);
      shift_nY++;
    }
    
    // Binary variables
    tf = tg = 0; 
    for(y = 0; y < nY[2]; y++){
      if(isYna[coord + *N * shift_nY]){
        // add nothing, but update indices
        tf += nfix[shift_nY];
        tg += ngrp[shift_nY];
        t += nran[shift_nY];
      }else{
        tstart = t2 = t;
        // Update predictor
        eta = 0.0; 
        
        for(l1 = 0; l1 < nfix[shift_nY]; l1++){
          // fixed part of predictor created by beta_bin_fix and covariates
          eta += pbeta_bin_fix[tf] * X[coord + *N * FormulaF[l1 + cumnfix[shift_nY]]]; 
          tf++;
        }
        
        for(l1 = 0; l1 < ngrp[shift_nY]; l1++){
          // group-specific part of predictor created by beta_bin and covariates
          eta += pbeta_bin[tg] * X[coord + *N * FormulaG[l1 + cumngrp[shift_nY]]]; 
          tg++;
        }
        
        for(l1 = 0; l1 < nran[shift_nY]; l1++){
          // random part of predictor, that needs to be updated wrt new b_i
          eta += theta[t] * X[coord + *N * FormulaR[t]]; 
          t++;
        }
        
        for(l1 = 0; l1 < noff[shift_nY]; l1++){
          // offset part of predictor
          eta += X[coord + *N * FormulaO[l1 + cumnoff[shift_nY]]]; 
        }
        
        logitinv_eta = logit_inv(eta);
        dli_eta = logitinv_eta * (1-logitinv_eta);
        
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
      }
      //printf("\nd_d2_logpbi: j = %d, t = %d, t2 = %d", j, t, t2);
      shift_nY++;
    }
    //printf("\nd_d2_logpbi: Bins are added");
    
    // Ordinal variables
    add_to_c = 0;
    tf = tg = 0; 
    for(y = 0; y < nY[3]; y++){
      if(isYna[coord + *N * shift_nY]){
        // add nothing, but update indices
        tf += nfix[shift_nY];
        tg += ngrp[shift_nY];
        t += nran[shift_nY];
      }else{
        tstart = t2 = t;
        // Update predictor
        eta = 0.0; 
        
        for(l1 = 0; l1 < nfix[shift_nY]; l1++){
          // fixed part of predictor created by beta_ord_fix and covariates
          eta += pbeta_ord_fix[tf] * X[coord + *N * FormulaF[l1 + cumnfix[shift_nY]]]; 
          tf++;
        }
        
        for(l1 = 0; l1 < ngrp[shift_nY]; l1++){
          // group-specific part of predictor created by beta_ord and covariates
          eta += pbeta_ord[tg] * X[coord + *N * FormulaG[l1 + cumngrp[shift_nY]]]; 
          tg++;
        }
        
        for(l1 = 0; l1 < nran[shift_nY]; l1++){
          // random part of predictor, that needs to be updated wrt new b_i
          eta += theta[t] * X[coord + *N * FormulaR[t]]; 
          t++;
        }
        
        for(l1 = 0; l1 < noff[shift_nY]; l1++){
          // offset part of predictor
          eta += X[coord + *N * FormulaO[l1 + cumnoff[shift_nY]]]; 
        }
        
        k = Y[coord + *N * shift_nY]; // double to int, requires values in {0, ..., Kord[y]}
        // Subtract the intercept for the category within
        if(k == 0){
          pk_1 = 1.0;
          ppk_1 = 0.0;
        }else{
          etak_1 = eta - pc_ord[add_to_c + k-1];
          pk_1 = logit_inv(etak_1);
          ppk_1 = pk_1 * (1-pk_1);
        }
        if(k == Kord[y]){
          pk = 0.0;
          ppk = 0.0;
        }else{
          etak = eta - pc_ord[add_to_c + k];
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
      }  
      //printf("\nd_d2_logpbi: j = %d, t = %d, t2 = %d", j, t, t2);
      shift_nY++;
      add_to_c += Kord[y];
    }
    //printf("\nd_d2_logpbi: Ords are added");
    
    // Categorical variables
    cumKcat = 0;
    tf = tg = 0;
    for(y = 0; y < nY[4]; y++){
      if(isYna[coord + *N * shift_nY]){
        // add nothing, but update indices
        tf += nfix[shift_nY] * Kcat[y];
        tg += ngrp[shift_nY] * Kcat[y];
        if(*kspec_bi_cat){ //k-specific bi
          t += nran[shift_nY] * Kcat[y];
        }else{
          // not k-specific bi
          t += nran[shift_nY]; 
        } 
      }else{
        tstart = t2 = t;
        //printf("\nd_d2_logpbi: Cats[%d]", y);
        // Update predictor
        maxeta = 0.0;
        for(k = 0; k < Kcat[y]; k++){
          etas[k] = 0.0; 
          for(l1 = 0; l1 < nfix[shift_nY]; l1++){
            // fixed part of predictor created by beta_cat_fix and covariates
            etas[k] += pbeta_cat_fix[tf] * X[coord + *N * FormulaF[l1 + cumnfix[shift_nY]]]; 
            tf++;
          }
          
          for(l1 = 0; l1 < ngrp[shift_nY]; l1++){
            // group-specific part of predictor created by beta_cat and covariates
            etas[k] += pbeta_cat[tg] * X[coord + *N * FormulaG[l1 + cumngrp[shift_nY]]]; 
            tg++;
          }
          
          if(*kspec_bi_cat){ //k-specific bi
            // continue in increasing t
          }else{
            // not k-specific bi
            t = tstart; // get back where starts bi for this y
          } 
          for(l1 = 0; l1 < nran[shift_nY]; l1++){
            etas[k] += theta[t] * X[coord + *N * FormulaR[l1 + cumnran[shift_nY]]];
            t++;
          }
          
          for(l1 = 0; l1 < noff[shift_nY]; l1++){
            // offset part of predictor
            etas[k] += X[coord + *N * FormulaO[l1 + cumnoff[shift_nY]]]; 
          }
          if(maxeta < etas[k]){
            maxeta = etas[k];
          }
          cumKcat++; // updates by 1 to another set of predictors
        }
        // subtracting maximal value
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
      }
      
      shift_nY++;
    } // end of categorical variables
    //printf("\nd_d2_logpbi: Cats are added");
    
    
  } // end of for j in 1:n_i
  
  free(etas);
  free(exp_eta);
  free(softmax);
  
}
