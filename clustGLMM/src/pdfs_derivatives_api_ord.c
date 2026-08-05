/*
 * Functions for logs of full conditional pdfs and their derivatives
 */

#include <R.h>
#include <Rmath.h>
#include <stdio.h>

#include "structures.h"
#include "my_math.h"

/*
 * A_ORD
 */

void logpYapi_ord(struct str_state* last,  // IN last known values of generated parameters
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
  int i, j, k, l;
  int coord;
  double qk;
  double etak;
  // double etak, etak_1;
  // double pk, pk_1;
  double pcum, mcum;
  double* ea;
  ea = (double*)malloc(*Kord * sizeof(double));
  //double ea[*Kord];
  double* c;
  c = (double*)malloc(*Kord * sizeof(double));
  //double c[*Kord]; // *nfix = *Kord
  
  // convert api_ord into c
  pcum = 0.0;
  mcum = 1.0;
  for(j = 0; j < *Kord; j++){
    ea[j] = exp(theta[j]);
    mcum += ea[j];
  }
  for(j = 0; j < *Kord; j++){
    pcum += ea[j];
    mcum -= ea[j];
    c[j] = log(pcum) - log(mcum);
  }
  
  *value = 0.0;
  
  for(l = 0; l < nUg[*g]; l++){
    // just those in g-th cluster (if g == G*), it is across all of them
    i = listUi[l + *n * *g];
    for(j = 0; j < n_i[i]; j++){
      coord = i_j[i + *n * j]; // index of l-th observation of j-th individual within g-th cluster
      
      // All 4 parts of predictor have already been updated
      etak = 0.0;
      for(k = 0; k < 4; k++){
        // nothing to be skipped from summation
        etak += predictor_ord[k + 4*coord];
        //if(l < 3){
        //  printf("\nlogpYord: i = %d, j = %d, k = %d, etak = %f", i, j, k, etak);
        //}
      }
      // category of Y_ij
      k = Y[coord]; // double to int, requires values in {0, ..., Kord[y]}
      
      // Subtract the intercept for the category within
      /*
      if(k == 0){
        pk_1 = 1.0;
      }else{
        etak_1 = etak - c[k-1];
        pk_1 = logit_inv(etak_1);
      }
      if(k == *Kord){
        pk = 0.0;
      }else{
        etak -= c[k];
        pk = logit_inv(etak);
      }
      qk = pk_1 - pk + 0.00000001;
      //printf("[%d]: k=%d, c=%f, pk_1=%f, pk=%f, qk=%f, ", coord, k, c[k], pk_1, pk, qk);
      *value += log(qk);
      */
      
      // Numerically stable version
      /*
      if(k == 0){
        pk_1 = 0.0;                      // complementary probability
        etak -= c[k];
        pk = 1.0 / (1.0 + exp(etak));    // complementary probability
        qk = pk - pk_1;
      }else{
        if(k == *Kord){
          pk = 0.0;
          etak_1 = etak - c[k-1];
          pk_1 = 1.0 / (1.0 + exp(-etak_1));
          qk = pk_1 - pk;
        }else{
          etak_1 = etak - c[k-1];
          etak -= c[k];
          
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
      qk = ord_qk(k, etak, c, Kord);
      *value += log(qk);
    }
  }
  
  free(ea);
  free(c);
  
  
}

void logpapi_ord(struct str_state* last,  // IN last known values of generated parameters
                 double* api_prior,        // hyperparameter - api_prior for Dirichlet prior of pi
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
  double pcum, sum_api_prior;
  int k;
  
  logpYapi_ord(last, Y, X, spec, dims, 
               predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat, pred_skip,
               N, n, g, Kord, Kcat, kspec_bi_cat, 
               nfix, cumnfix, totnfix, FormulaF, n_i, i_j, nUg, listUi, theta, value);
  
  pcum = 1.0;
  //sum_api_prior = api_prior[*Kord];
  for(k = 0; k < *Kord; k++){
    //*value += api_prior[k] * theta[k];
    *value += *api_prior * theta[k];
    pcum += exp(theta[k]);
    //sum_api_prior += api_prior[k]; 
  }
  //*value += api_prior[*Kord] * 0.0; // for the last category
  
  sum_api_prior = (*Kord+1) * (*api_prior);
  
  *value -= sum_api_prior * log(pcum);
  
}

void d_d2_logpapi_ord(struct str_state* last,  // IN last known values of generated parameters
                      double* api_prior,        // hyperparameter - api_prior for Dirichlet prior of pi
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
  int i, j, k, l1, l2, l, ll;
  int coord, d2coord, dcoord;
  double* c;
  c = (double*)malloc(*Kord * sizeof(double));
  //double c[*Kord]; // *nfix = *Kord
  double aux, auxk, auxk_1;
  double etak, etak_1;
  double pk, pk_1;
  double ppk, ppk_1, qk;
  double ppk_qk, ppk_1_qk;
  double pcum, mcum, sum_api_prior;
  double* ea;
  ea = (double*)malloc(*Kord * sizeof(double));
  //double ea[*Kord];
  double* ea_pcum;
  ea_pcum = (double*)malloc(*Kord * sizeof(double));
  //double ea_pcum[*Kord];
  double* dcda;
  dcda = (double*)malloc((*Kord+2) * *Kord * sizeof(double));
  //double dcda[(*Kord+2) * *Kord]; // derivative of c with respect to a
  // [c_0 / da], [c_1 / da], ... , [c_K / da]
  // first and last set will be set to 0.0
  double* d2cd2a;
  d2cd2a = (double*)malloc((*Kord+2) * *Kord * (*Kord+1)/2 * sizeof(double));
  //double d2cd2a[(*Kord+2) * *Kord * (*Kord+1)/2]; // derivative of c with respect to a
  // [c_0 / dada], [c_1 / dada], ... , [c_K / dada]
  // first and last set will be set to 0.0
  
  // convert api_ord into c
  dcoord = 0;
  d2coord = 0;
  pcum = 0.0;
  mcum = 1.0;
  //sum_api_prior = (*param).api_prior[*Kord];
  for(k = 0; k < *Kord; k++){
    //sum_api_prior += (*param).api_prior[k]; 
    ea[k] = exp(theta[k]);
    mcum += ea[k];
    dcda[dcoord] = 0.0; // c_0 does not contain any a
    dcoord++;
    for(l = k; l < *Kord; l++){
      d2cd2a[d2coord] = 0.0;
      d2coord++;
    }
  }
  sum_api_prior = (*Kord+1) * (*api_prior);
  
  for(k = 0; k < *Kord; k++){
    pcum += ea[k];
    mcum -= ea[k];
    // ordered intercepts
    c[k] = log(pcum / mcum);
    
    // first derivatives
    for(l = 0; l <*Kord; l++){
      if(l <= k){
        dcda[dcoord + l] = ea[l] / pcum;
      }else{
        dcda[dcoord + l] = -ea[l] / mcum;
      }
    }
    
    // second derivatives
    for(l1 = 0; l1 <*Kord; l1++){
      
      // diagonal case l2 == l1
      aux = dcda[dcoord + l1];
      if(l1 <= k){
        d2cd2a[d2coord + l1*(l1+3)/2] = aux * (1.0 - aux);
      }else{
        d2cd2a[d2coord + l1*(l1+3)/2] = aux * (1.0 + aux);
      }
      
      // case l1 < l2
      for(l2 = l1+1; l2 <*Kord; l2++){
        if(l1 <= k){
          if(l2 <= k){
            d2cd2a[d2coord + l1 + l2*(l2+1)/2] = - dcda[dcoord + l1] * dcda[dcoord + l2];
          }else{
            d2cd2a[d2coord + l1 + l2*(l2+1)/2] = 0.0;
          }
        }else{
          d2cd2a[d2coord + l1 + l2*(l2+1)/2] = dcda[dcoord + l1] * dcda[dcoord + l2];
        }
        
        //if( (l1 <= k) & (k < l2) ){
        //  d2cd2a[d2coord + l1 + l2*(l2+1)/2] = 0.0;
        //}else{
        //  d2cd2a[d2coord + l1 + l2*(l2+1)/2] = - dcda[dcoord + l1] * dcda[dcoord + l2];
        //}
      }
    }
    
    dcoord += *Kord;
    d2coord += *Kord * (*Kord + 1) / 2;
  }
  
  // terms for c_K = Inf, should disappear anyway, but for the purpose of programming set to zero
  for(l1 = 0; l1 < *Kord; l1++){
    dcda[dcoord] = 0.0; // c_K does not contain any a
    dcoord++;
    for(l2 = l1; l2 <*Kord; l2++){
      d2cd2a[d2coord] = 0.0; // c_K does not contain any a
      d2coord++;
    }
  }
  
  //Printing derivatives
  //printf("\nFirstderivative:");
  //for(k = 0; k<(*Kord+2); k++){
  //  printf("\n [k]=%d\ndcda: ",k);
  //  for(l = 0; l<*Kord; l++){
  //    printf("[%d] = %f, ", l, dcda[*Kord * k + l]);
  //  }
  //}
  //fflush(stdout);
  //printf("\nSecondderivative:");
  //for(k = 0; k<(*Kord+2); k++){
  //  printf("\n [k]=%d\ndcda: ",k);
  //  for(l1 = 0; l1<*Kord; l1++){
  //    printf("\n");
  //    for(l2 = l1; l2<*Kord; l2++){
  //      printf("%f, ", d2cd2a[*Kord * (*Kord + 1)/2 * k + l1 + l2*(l2+1)/2]);
  //    }
  //  }
  //}
  //fflush(stdout);
  
  
  pcum += 1.0;
  for(k = 0; k < *Kord; k++){
    ea_pcum[k] = ea[k]/pcum;
  }
  
  // start with zeros in gradient 
  for(k = 0; k < *Kord; k++){
    grad[k] = 0.0;
  }
  
  // start with zeros in hess matrix
  for(l1 = 0; l1 < *Kord; l1++){
    for(l2 = 0; l2 < *Kord; l2++){
      l = l1 + l2*(l2+1)/2;
      hess[l] = 0.0;
    }
  }
  
  for(ll = 0; ll < nUg[*g]; ll++){
    // just those in g-th cluster (if g == G*), it is across all of them
    i = listUi[ll + *n * *g];
    for(j = 0; j < n_i[i]; j++){
      coord = i_j[i + *n * j]; // index of l-th observation of j-th individual within g-th cluster
      
      // All 4 parts of predictor have already been updated
      etak = 0.0;
      for(k = 0; k < 4; k++){
        // nothing to be skipped from summation
        etak += predictor_ord[k + 4*coord];
        //if(l < 3){
        //  printf("\nlogpYord: i = %d, j = %d, k = %d, etak = %f", i, j, k, etak);
        //}
      }
      
      // category of Y_ij
      k = Y[coord]; // double to int, requires values in {0, ..., Kord[y]}
      
      // Subtract the intercept for the category within
      if(k == 0){
        pk_1 = 1.0;
        ppk_1 = 0.0;
      }else{
        etak_1 = etak - c[ k-1];
        pk_1 = logit_inv(etak_1);
        ppk_1 = pk_1 * (1.0 - pk_1);
      }
      if(k == *Kord){
        pk = 0.0;
        ppk = 0.0;
      }else{
        etak -= c[k];
        pk = logit_inv(etak);
        ppk = pk * (1.0 - pk);
      }
      
      qk = pk_1 - pk;
      ppk_qk = ppk/qk;
      ppk_1_qk = ppk_1/qk;
      auxk = (pk*pk + pk_1*(1-2*pk))/qk;
      auxk_1 = (pk_1*pk_1 + pk*(1-2*pk_1))/qk;
      
      for(l1 = 0; l1 < *Kord; l1++){
        // gradient
        grad[l1] -= ppk_1/qk * dcda[*Kord * k + l1];
        grad[l1] += ppk/qk * dcda[*Kord * (k+1) + l1];
        
        // Hess diagonal
        l = l1*(l1+3)/2;
        hess[l] += d2cd2a[*Kord * (*Kord + 1)/2 * k + l] * ppk_1_qk;
        hess[l] -= d2cd2a[*Kord * (*Kord + 1)/2 * (k+1) + l] * ppk_qk;
        hess[l] += dcda[*Kord * k + l1] * dcda[*Kord * k + l1] * ppk_1_qk * auxk_1;
        hess[l] += dcda[*Kord * (k+1) + l1] * dcda[*Kord * (k+1) + l1] * ppk_qk * auxk;
        hess[l] -= 2.0 * dcda[*Kord * k + l1] * dcda[*Kord * (k+1) + l1] * ppk_1_qk * ppk_qk;
        
        // Hess off-diagonal
        for(l2 = l1+1; l2 < *Kord; l2++){
          l = l1 + l2*(l2+1)/2;
          hess[l] += d2cd2a[*Kord * (*Kord + 1)/2 * k + l] * ppk_1_qk;
          hess[l] -= d2cd2a[*Kord * (*Kord + 1)/2 * (k+1) + l] * ppk_qk;
          
          hess[l] += dcda[*Kord * k + l1] * dcda[*Kord * k + l2] * ppk_1_qk * auxk_1;
          hess[l] -= dcda[*Kord * k + l1] * dcda[*Kord * (k+1) + l2] * ppk_1_qk * ppk_qk;
          
          hess[l] += dcda[*Kord * (k+1) + l1] * dcda[*Kord * (k+1) + l2] * ppk_qk * auxk;
          hess[l] -= dcda[*Kord * (k+1) + l1] * dcda[*Kord * k + l2] * ppk_1_qk * ppk_qk;
          
        }
      }
      
      
    }
  }
  
  // end with addition of prior distribution
  for(l1 = 0; l1 < *Kord; l1++){
    // gradient
    //value[l1] += api_prior[l1];
    grad[l1] += *api_prior;
    grad[l1] -= sum_api_prior * ea_pcum[l1];
    
    // Hess diagonal
    k = l1*(l1+3)/2;
    hess[k] += sum_api_prior * ea_pcum[l1] * (1.0 - ea_pcum[l1]); 
    
    for(l2 = l1+1; l2 < *Kord; l2++){
      // Hess off-diagonal
      k = l1 + l2*(l2+1)/2;
      hess[k] -= sum_api_prior * ea_pcum[l1] * ea_pcum[l2];
    }
  }
  //printf("after_prior_distribution: ");
  //for(k = 0; k < *Kord; k++) printf("grad[%d] = %f,", k, grad[k]);
  //printf("\n");
  
  
  free(ea);
  free(c);
  free(ea_pcum);
  free(dcda);
  free(d2cd2a);
}
