//
//  PURPOSE:   Metropolis within Gibbs algorithm 
//             for Model-Based Clustering in joint modelling
//             of numeric, count, binary, ordinal and categorical variables
//             while assuming logit models for bin, ord and cat variables
//                            Poisson log-linear model for count variables
//                            LME for numeric variables
//
//  AUTHOR:    Jan Vavra
//             vavraj[AT]karlin.mff.cuni.cz
//
//  LOG:       20210413  created
//
// ================================================================
//
// To obtain a dll file in Windows, run
//
//    library("callr")
//    rcmd(cmd = "SHLIB", cmdargs = "class_num_ord_bin.c")
//
//    To be precise, you need to compile all in one:
// rcmd(cmd = "SHLIB", cmdargs = c("class_num_ord_bin.c",
//                                  "calculate_predictor.c",
//                                  "cholesky.c",
//                                  "matrmult.c",
//                                  "myrdirichlet.c",
//                                  "myrtruncnorm.c",
//                                  "myrwishart.c",
//                                  "gibbs_b.c",
//                                  "gibbs_beta.c",
//                                  "gibbs_InvQ.c",
//                                  "gibbs_InvSigma.c",
//                                  "gibbs_latent.c",
//                                  "gibbs_pUik.c",
//                                  "gibbs_pUik_nonb.c",
//                                  "gibbs_prec.c"))
//
// To use declared functions in R use
//
//    dyn.load("./class_num_ord_bin.dll")
//


/*** These are headers available within the R source tree      ***/
/*** that provide mathematical and also many statistical       ***/
/*** functions (densities, cdf's, random number generators)    ***/
/*** available in R itself.                                    ***/

#include <R.h>
#include <Rmath.h>
#include <stdio.h>
#include <stdlib.h>

#include "structures.h"
#include "my_math.h"

#include "calculate_predictor.h"
#include "matrmult.h"
#include "cholesky.h"
#include "myrdirichlet.h"
#include "newton_raphson.h"
#include "metropolis.h"
#include "newton_raphson_le0.h"
#include "metropolis_le0.h"

#include "gibbs_naY.h"
#include "gibbs_InvQ.h"
#include "gibbs_InvSigma.h"
#include "gibbs_pUig.h"

#include "gibbs_beta_num.h"
#include "gibbs_prec_num.h"

#include "metrgibbs_beta_poi.h"
#include "metrgibbs_beta_bin.h"
#include "metrgibbs_beta_ord.h"
#include "metrgibbs_a_ord.h"
#include "metrgibbs_beta_cat.h"
#include "metrgibbs_bi.h"
#include "metrgibbs_e0.h"

#include "pdfs_derivatives_beta_poi.h"
#include "pdfs_derivatives_beta_bin.h"
#include "pdfs_derivatives_beta_ord.h"
#include "pdfs_derivatives_api_ord.h"
#include "pdfs_derivatives_beta_cat.h"
#include "pdfs_derivatives_bi.h"
#include "pdfs_derivatives_le0.h"
#include "pdfs_last.h"

/*** ================================================================================ ***/
/*** THE KEY PART OF THE CODE ***/
/*** Gibbs algorithm ***/
/*** ================================================================================ ***/

void Metropolis_within_Gibbs_MBC_NumPoiBinOrdCat( 
    /** IN parameters **/
    int* Id,              // [N] IDs
    double* Y,            // [N*sum(nY)]  responses
    int* isYna,           // [N*sum(nY)]  0 = Y value present, 1 = Y value is NA
    double* X,            // [N*#regr]    regressors
    int* varying,         // [5]          class-specific parameters
                            // order:   [0] prec_num, 
                            //          [1] c_ord,
                            //          [2] InvSigma, 
                            //          [3] InvQ, 
                            //          [4] naY
    int* save,            // [33]:        what parameters are saved
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
    double* vecparam,     // vector of parameters --> str_param
    double* vecinits,     // vector of initial values (except U) --> str_state 
    /** OUT last state **/
    double* veclast,      // vector of last values (except U) <-- str_state 
    /** Parameters describing dimensions **/
    int* chain,           // [1]        number of generated chain (just for printing)
    int* G,               // [1]        number of groups=classes=clusters
    int* iter,            // [1]        total number of generated states
    int* N,               // [1]        total number of observations
    int* n,               // [1]        total number of subjects (different ids in the dataset)
    int* nY,              // [5]        counts of Nums, Pois, Bins, Ords, Cats variables
    int* FormulaF,        // [sum(nfix)]  numbers of columns of X that should be used for FIXED  effects of modelled responses
    int* FormulaG,        // [sum(ngrp)]  numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
    int* FormulaR,        // [sum(nran)]  numbers of columns of X that should be used for RANDOM effects of modelled responses
    int* FormulaO,        // [sum(noff)]  numbers of columns of X that should be used for OFFSET effects of modelled responses
    int* nfix,            // [sum(nY)]  number of FIXED  regressors for each response
    int* ngrp,            // [sum(nY)]  number of GROUP-SPECIFIC  regressors for each response
    int* nran,            // [sum(nY)]  number of RANDOM regressors for each response
    int* noff,            // [sum(nY)]  number of OFFSET regressors for each response
    int* Kord,            // [nY[Ords]]  the counts of categories of ordinal variables -1
    int* Kcat,            // [nY[Cats]]  the counts of categories of ordinal variables -1
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
    int* dimswithG,       // [33]:              what parameters are saved
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
    // keep in mind that these counts for Cats are just for single predictor, not all of them
    /** Arrays to store generated states **/
    /** Some of them might be NULL (see save and calc) **/
    double* beta_num_fix, // [(B+M)*sum(nfix[Nums])]      Beta parameters for fixed effects of numerical variables
    double* beta_num,     // [(B+M)*sum(ngrp[Nums]) *G]   Beta parameters for group-specific fixed effects of numerical variables
    double* prec_num,     // [(B+M)*nY[0]*(G)]            Precision parameter of numerical variables
    double* sd_num,       // [(B+M)*nY[0]*(G)]            Standard deviation of numerical variables
    double* var_num,      // [(B+M)*nY[0]*(G)]            Variance of numerical variables
    double* beta_poi_fix, // [(B+M)*sum(nfix[Pois])]      Beta parameters for fixed effects of count variables
    double* beta_poi,     // [(B+M)*sum(ngrp[Pois]) *G]   Beta parameters for group-specific fixed effects of count variables
    double* beta_bin_fix, // [(B+M)*sum(nfix[Bins])]      Beta parameters for fixed effects of binary variables
    double* beta_bin,     // [(B+M)*sum(ngrp[Bins]) *G]   Beta parameters for group-specific effects of binary variables
    double* beta_ord_fix, // [(B+M)*sum(nfix[Ords])]      Beta parameters for fixed effects of ordinal variables
    double* beta_ord,     // [(B+M)*sum(ngrp[Ords]) *G]   Beta parameters for group-specific fixed effects of ordinal variables
    double* a_ord,        // [(B+M)*sum(Kord)(* G)]       Non-constrained transformed intercepts for ordinal variables
    double* c_ord,        // [(B+M)*sum(Kord)(* G)]       Ordered intercepts for ordinal variables
    double* pi_ord,       // [(B+M)*sum(Kord+1)(* G)]     Ordered intercepts for ordinal variables
    double* beta_cat_fix, // [(B+M)*sum(nfix[Cats]*Kcat)] Beta parameters for fixed effects of categorical variables
    double* beta_cat,     // [(B+M)*sum(ngrp[Cats]*Kcat) *G] Beta parameters for group-specific effects of categorical variables
    double* InvSigma,     // [(B+M)*totnran*(totnran+1)/2(* G)] Precision matrix of random effects
    double* Sigma,        // [(B+M)*totnran*(totnran+1)/2(* G)] Variance matrix of random effects
    double* sdSigma,      // [(B+M)*totnran(* G)]         standard deviations of random effects
    double* corSigma,     // [(B+M)*totnran*(totnran-1)/2(* G)] correlations of random effects
    double* detInvSigma,  // [(B+M)(* G)]                 determinant of InvSigma
    double* InvQ,         // [(B+M)*totnran*(totnran+1)/2(* G)] Prior inverse scale matrix of InvSigma
    double* Q,            // [(B+M)*totnran*(totnran+1)/2(* G)] Prior scale matrix of InvSigma
    double* detInvQ,      // [(B+M)(* G)]                 determinant of InvQ
    double* b,            // [(B+M)*totnran * n]          Random effects
    double* w,            // [(B+M)*G]                    Class probabilities
    int*    ng,           // [(B+M)*G]                    counts of units within clusters
    double* loglik,       // [(B+M)]                      Full-conditional model likelihood (evaluated when all latent known)
    double* pUig,         // [(B+M)*n * G]                Probability of i-th subject belong to each class
    int*    U,            // [(B+M)*n]                    To which class does i-th subject belong to
    int*    Gplus,        // [B+M]                        number of non-empty clusters
    double* e0,           // [B+M]                        precision parameter for prior of w
    double* naY,          // [(B+M)*sum(isYna)(* G)]      imputed missing outcome values
    // Tuning parameters
    double* vectuningdouble,  // [11]
    int* vectuninginteger     // [5]
  )
{
  //// Declarations ////
  //// ------------ ////
  int i, j, m, y;          // looping indeces
  int shiftnY;
  //int row, col;             // looping indeces for matrix dimensions
  
  //for(i = 0; i < 33; i++){
  //  printf("\nC: n = %d, N = %d, dims[%d] = %d, dimswithG[%d] = %d",
  //         *n, *N, i, dims[i], i, dimswithG[i]);
  //}
  //fflush(stdout);
  
  //// Tuning parameters into given structure ////
  double* p = vectuningdouble; // pointer for elements of vecparam
  int* pi = vectuninginteger;
  struct str_tuning tuning;     // structure where hyperparameters will be stored
  
  tuning.freq_proposal_update         = pi;    pi++;
  tuning.times_proposal               = pi;    pi++;
  tuning.const_proposal_beta_poi_fix  = p;     p++;
  tuning.const_proposal_beta_poi      = p;     p++;
  tuning.const_proposal_beta_bin_fix  = p;     p++;
  tuning.const_proposal_beta_bin      = p;     p++;
  tuning.const_proposal_beta_ord_fix  = p;     p++;
  tuning.const_proposal_beta_ord      = p;     p++;
  tuning.const_proposal_beta_cat_fix  = p;     p++;
  tuning.const_proposal_beta_cat      = p;     p++;
  tuning.const_proposal_a_ord         = p;     p++;
  tuning.const_proposal_b             = p;     p++;
  tuning.const_proposal_e0            = p;     p++;
  tuning.tolerance                    = p;     p++;
  tuning.maxiter                      = pi;    pi++;
  tuning.maxnrep                      = pi;    pi++;
  tuning.kspec_bi_cat                 = pi;  
  
  //printf("\ntuning.freq_proposal_update = %d", *(tuning.freq_proposal_update));
  //printf("\ntuning.times_proposal = %d", *(tuning.times_proposal));
  //printf("\ntuning.const_proposal_beta_poi_fix = %f", *(tuning.const_proposal_beta_poi_fix));
  //printf("\ntuning.const_proposal_beta_poi = %f", *(tuning.const_proposal_beta_poi));
  //printf("\ntuning.const_proposal_beta_bin_fix = %f", *(tuning.const_proposal_beta_bin_fix));
  //printf("\ntuning.const_proposal_beta_bin = %f", *(tuning.const_proposal_beta_bin));
  //printf("\ntuning.const_proposal_beta_ord_fix = %f", *(tuning.const_proposal_beta_ord_fix));
  //printf("\ntuning.const_proposal_beta_ord = %f", *(tuning.const_proposal_beta_ord));
  //printf("\ntuning.const_proposal_beta_cat_fix = %f", *(tuning.const_proposal_beta_cat_fix));
  //printf("\ntuning.const_proposal_beta_cat = %f", *(tuning.const_proposal_beta_cat));
  //printf("\ntuning.const_proposal_a_ord = %f", *(tuning.const_proposal_a_ord));
  //printf("\ntuning.const_proposal_b = %f", *(tuning.const_proposal_b));
  //printf("\ntuning.const_proposal_e0 = %f", *(tuning.const_proposal_e0));
  //printf("\ntuning.tolerance = %f", *(tuning.tolerance));
  //printf("\ntuning.maxiter = %d", *(tuning.maxiter)); 
  //printf("\ntuning.maxnrep = %d", *(tuning.maxnrep));
  //printf("\ntuning.kspec_bi_cat = %d", *(tuning.kspec_bi_cat));
  //if(*(tuning.kspec_bi_cat)){
  //  printf("\ndouble kspec_bi_cat is TRUE");
  //}else{
  //  printf("\ndouble kspec_bi_cat is FALSE");
  //}
  
  //Rprintf("\nC: Tuning parameters are ready");
  //fflush(stdout);
  
  
  //// Calculation of useful dimensions, etc. ////
  //// -------------------------------------- ////
  // Total number of responses
  int totnY;            // total number of responses
  totnY = 0;
  for(i = 0; i < 5; i++)
    totnY += nY[i];
  int curSize = totnY + 1;
  
  // Total and cummulative numbers of fixed and random regressors
  // Will be needed separately for Nums, Bins, Ords and Cats
  
  //int totnfix_num = 0;      // total number of fixed regressors of Nums
  //int totnfix_poi = 0;      // total number of fixed regressors of Pois
  //int totnfix_bin = 0;      // total number of fixed regressors of Bins
  //int totnfix_ord = 0;      // total number of fixed regressors of Ords
  //int totnfix_cat = 0;      // total number of fixed regressors of Cats
  
  //int totngrp_num = 0;      // total number of group-specific regressors of Nums
  //int totngrp_poi = 0;      // total number of group-specific regressors of Pois
  //int totngrp_bin = 0;      // total number of group-specific regressors of Bins
  //int totngrp_ord = 0;      // total number of group-specific regressors of Ords
  //int totngrp_cat = 0;      // total number of group-specific regressors of Cats
  
  //int totnran_num = 0;      // total number of random regressors of Nums
  //int totnran_poi = 0;      // total number of random regressors of Pois
  //int totnran_bin = 0;      // total number of random regressors of Bins
  //int totnran_ord = 0;      // total number of random regressors of Ords
  //int totnran_cat = 0;      // total number of random regressors of Cats
  int totnran = 0;          // total number of all random regressors
  
  //int totnoff_num = 0;      // total number of offset regressors of Nums
  //int totnoff_poi = 0;      // total number of offset regressors of Pois
  //int totnoff_bin = 0;      // total number of offset regressors of Bins
  //int totnoff_ord = 0;      // total number of offset regressors of Ords
  //int totnoff_cat = 0;      // total number of offset regressors of Cats
  
  //int cumnfix_num [nY[0] + 1];  // cummulative number of fixed regressors of Nums
  //int cumnfix_poi [nY[1] + 1];  // cummulative number of fixed regressors of Pois
  //int cumnfix_bin [nY[2] + 1];  // cummulative number of fixed regressors of Bins
  //int cumnfix_ord [nY[3] + 1];  // cummulative number of fixed regressors of Ords
  //int cumnfix_cat [nY[4] + 1];  // cummulative number of fixed regressors of Cats
  int* cumnfix;
  cumnfix = (int *)R_alloc(curSize, sizeof(int));
  //int cumnfix [totnY + 1];      // cummulative number of fixed regressors of alltogether
  
  //int cumngrp_num [nY[0] + 1];  // cummulative number of group-specific regressors of Nums
  //int cumngrp_poi [nY[1] + 1];  // cummulative number of group-specific regressors of Pois
  //int cumngrp_bin [nY[2] + 1];  // cummulative number of group-specific regressors of Bins
  //int cumngrp_ord [nY[3] + 1];  // cummulative number of group-specific regressors of Ords
  //int cumngrp_cat [nY[4] + 1];  // cummulative number of group-specific regressors of Cats
  int* cumngrp;
  cumngrp = (int *)R_alloc(curSize, sizeof(int));
  //int cumngrp [totnY + 1];      // cummulative number of group-specific regressors of alltogether
  
  //int cumnran_num [nY[0] + 1];  // cummulative number of random regressors of Nums
  //int cumnran_poi [nY[1] + 1];  // cummulative number of random regressors of Pois
  //int cumnran_bin [nY[2] + 1];  // cummulative number of random regressors of Bins
  //int cumnran_ord [nY[3] + 1];  // cummulative number of random regressors of Ords
  //int cumnran_cat [nY[4] + 1];  // cummulative number of random regressors of Cats
  int* cumnran;
  cumnran = (int *)R_alloc(curSize, sizeof(int));
  //int cumnran [totnY + 1];      // cummulative number of random regressors of alltogether
  
  //int cumnoff_num [nY[0] + 1];  // cummulative number of offset regressors of Nums
  //int cumnoff_poi [nY[1] + 1];  // cummulative number of offset regressors of Pois
  //int cumnoff_bin [nY[2] + 1];  // cummulative number of offset regressors of Bins
  //int cumnoff_ord [nY[3] + 1];  // cummulative number of offset regressors of Ords
  //int cumnoff_cat [nY[4] + 1];  // cummulative number of offset regressors of Cats
  int* cumnoff;
  cumnoff = (int *)R_alloc(curSize, sizeof(int));
  //int cumnoff [totnY + 1];      // cummulative number of offset regressors of alltogether
  
  //cumnfix_num[0] = cumngrp_num[0] = cumnran_num[0] = cumnoff_num[0] = 0;
  cumnfix[0] = cumngrp[0] = cumnran[0] = cumnoff[0] = 0;
  
  //j = 0;
  //for(i = 0; i < nY[0]; i++){
  //  totnfix_num += nfix[j];
  //  totngrp_num += ngrp[j];
  //  totnran_num += nran[j];
  //  totnoff_num += noff[j];
  //  cumnfix_num[i+1] = cumnfix[j+1] = totnfix_num;
  //  cumngrp_num[i+1] = cumngrp[j+1] = totngrp_num;
  //  cumnran_num[i+1] = cumnran[j+1] = totnran_num;
  //  cumnoff_num[i+1] = cumnoff[j+1] = totnoff_num;
  //  j++;
  //}
  
  //cumnfix_poi[0] = cumngrp_poi[0] = cumnran_poi[0] = cumnoff_poi[0] = 0;
  //for(i = 0; i < nY[1]; i++){
  //  totnfix_poi += nfix[j];
  //  totngrp_poi += ngrp[j];
  //  totnran_poi += nran[j];
  //  totnoff_poi += noff[j];
  //  cumnfix_poi[i+1] = totnfix_poi;
  //  cumngrp_poi[i+1] = totngrp_poi;
  //  cumnran_poi[i+1] = totnran_poi;
  //  cumnoff_poi[i+1] = totnoff_poi;
  //  cumnfix[j+1] = cumnfix[j] + nfix[j];
  //  cumngrp[j+1] = cumngrp[j] + ngrp[j];
  //  cumnran[j+1] = cumnran[j] + nran[j];
  //  cumnoff[j+1] = cumnoff[j] + noff[j];
  //  j++;
  //}
  
  //cumnfix_bin[0] = cumngrp_bin[0] = cumnran_bin[0] = cumnoff_bin[0] = 0;
  //for(i = 0; i < nY[2]; i++){
  //  totnfix_bin += nfix[j];
  //  totngrp_bin += ngrp[j];
  //  totnran_bin += nran[j];
  //  totnoff_bin += noff[j];
  //  cumnfix_bin[i+1] = totnfix_bin;
  //  cumngrp_bin[i+1] = totngrp_bin;
  //  cumnran_bin[i+1] = totnran_bin;
  //  cumnoff_bin[i+1] = totnoff_bin;
  //  cumnfix[j+1] = cumnfix[j] + nfix[j];
  //  cumngrp[j+1] = cumngrp[j] + ngrp[j];
  //  cumnran[j+1] = cumnran[j] + nran[j];
  //  cumnoff[j+1] = cumnoff[j] + noff[j];
  //  j++;
  //}
  
  //cumnfix_ord[0] = cumngrp_ord[0] = cumnran_ord[0] = cumnoff_ord[0] = 0;
  //for(i = 0; i < nY[3]; i++){
  //  totnfix_ord += nfix[j];
  //  totngrp_ord += ngrp[j];
  //  totnran_ord += nran[j];
  //  totnoff_ord += noff[j];
  //  cumnfix_ord[i+1] = totnfix_ord;
  //  cumngrp_ord[i+1] = totngrp_ord;
  //  cumnran_ord[i+1] = totnran_ord;
  //  cumnoff_ord[i+1] = totnoff_ord;
  //  cumnfix[j+1] = cumnfix[j] + nfix[j];
  //  cumngrp[j+1] = cumngrp[j] + ngrp[j];
  //  cumnran[j+1] = cumnran[j] + nran[j];
  //  cumnoff[j+1] = cumnoff[j] + noff[j];
  //  j++;
  //}
  
  //cumnfix_cat[0] = cumngrp_cat[0] = cumnran_cat[0] = cumnoff_cat[0] = 0;
  //for(i = 0; i < nY[4]; i++){
  //  totnfix_cat += nfix[j]*Kcat[i]; // *Kcat for several predictors per categorical outcome
  //  totngrp_cat += ngrp[j]*Kcat[i]; // *Kcat for several predictors per categorical outcome
  //  if(*(tuning.kspec_bi_cat)){
  //    totnran_cat += nran[j]*Kcat[i]; // *Kcat for several predictors per categorical outcome
  //  }else{
  //    totnran_cat += nran[j];         // There is only one set of random effects for each cat outcome
  //  }
  //  totnoff_cat += nfix[j]; // There is only one offset for each category
  //  
  //  cumnfix_cat[i+1] = totnfix_cat;
  //  cumngrp_cat[i+1] = totngrp_cat;
  //  cumnran_cat[i+1] = totnran_cat;
  //  cumnoff_cat[i+1] = totnoff_cat;
  //  cumnfix[j+1] = cumnfix[j] + nfix[j]; // not *Kcat, because it is meant to be for orientation in FormulaF
  //  cumngrp[j+1] = cumngrp[j] + ngrp[j]; // not *Kcat, because it is meant to be for orientation in FormulaG
  //  cumnran[j+1] = cumnran[j] + nran[j]; // not *Kcat, because it is meant to be for orientation in FormulaR
  //  cumnoff[j+1] = cumnoff[j] + noff[j]; // not *Kcat
  //  j++;
  //}
  
  for(y = 0; y < totnY; y++){
    cumnfix[y+1] = cumnfix[y] + nfix[y];
    cumngrp[y+1] = cumngrp[y] + ngrp[y];
    cumnran[y+1] = cumnran[y] + nran[y];
    cumnoff[y+1] = cumnoff[y] + noff[y];
  }
  
  totnran = cumnran[totnY];
  
  //for(i = 0; i <= totnY; i++){
  //  printf("\nC: cumnfix[%d] = %d, cumngrp[%d] = %d, cumnran[%d] = %d, cumnoff[%d] = %d,", 
  //         i, cumnfix[i], i, cumngrp[i], i, cumnran[i], i, cumnoff[i]);
  //}
  //fflush(stdout);
  
  // Subjects 
  int* n_i;
  n_i = (int *)R_alloc(*n, sizeof(int));
  //int n_i [*n];       // number of observations dedicated to j-th subject
  int max_n_i = 0;    // maximal value of n_i numbers
  int id;             // ID of the subject
  
  for(i = 0; i < *n; i++){
    n_i[i] = 0;
  }
  
  for(i = 0; i < *N; i++){
    id = Id[i]; 
    n_i[id]++; 
    if(n_i[id] > max_n_i){
      max_n_i = n_i[id];
    }
  }
  
  //printf("\nC: (max_n_i=%d) * (n=%d) = %d", max_n_i, *n, *n * max_n_i);
  
  int* i_j;
  i_j = (int *)R_alloc(*n * max_n_i, sizeof(int));
  //int i_j [*n * max_n_i];      // matrix of indeces to j-th subject
  int* nn_ii;
  nn_ii = (int *)R_alloc(*n, sizeof(int));
  //int nn_ii [*n];             // counts of each subject
  
  for(i = 0; i < *n; i++){
    nn_ii[i] = 0;
  }
  for(i = 0; i < *N; i++){
    id = Id[i];
    i_j[id + *n * nn_ii[id]] = i;
    ++nn_ii[id];
  }
  
  //printf("\nC: Id[%d] = %d, i_j[(%d+%d*%d) = %d] = %d", 
  //       *N, Id[*N-1], id, *n, nn_ii[id]-1, id + *n * (nn_ii[id]-1), i_j[id + *n * (nn_ii[id]-1)]);
  
  int sumKord = 0;
  int* cumKord;
  cumKord = (int *)R_alloc(nY[3] + 1, sizeof(int));
  //int cumKord[nY[3] + 1];
  cumKord[0] = 0;
  for(i = 0; i < nY[3]; i++){
    sumKord += Kord[i];
    cumKord[i+1] = sumKord;
  }
  
  int sumKcat = 0;
  int* cumKcat;
  cumKcat = (int *)R_alloc(nY[4] + 1, sizeof(int));
  //int cumKcat[nY[4] + 1];
  cumKcat[0] = 0;
  for(i = 0; i < nY[4]; i++){
    sumKcat += Kcat[i];
    cumKcat[i+1] = sumKcat;
  }
  
  //printf("\ncumKcat[0]=%d, cumKcat[1]=%d, sumKcat=%d, Kcat[0]=%d, KCat[1]=%d",
  //       cumKcat[0], cumKcat[1], sumKcat, Kcat[0], Kcat[1]);
  //fflush(stdout);
  
  //printf("\nC: Dimensions calculated");
  //fflush(stdout);
  
  //// Reading parameter values ////
  //// ------------------------ ////
  
  p = vecparam; // pointer for elements of vecparam
  struct str_param param;     // structure where hyperparameters will be stored
  
  param.nu_0              = p;     p++;
  param.nu_1              = p;     p++;
  param.gamma_a           = p;     p++;
  param.gamma_b           = p;     p++;
  param.sd_beta_num_fix   = p;     p++;
  param.sd_beta_num       = p;     p++;
  param.sd_beta_poi_fix   = p;     p++;
  param.sd_beta_poi       = p;     p++;
  param.sd_beta_bin_fix   = p;     p++;
  param.sd_beta_bin       = p;     p++;
  param.sd_beta_ord_fix   = p;     p++;
  param.sd_beta_ord       = p;     p++;
  param.api_prior         = p;     p++;
  param.sd_beta_cat_fix   = p;     p++;
  param.sd_beta_cat       = p;     p++;
  param.ae                = p;     p++;
  param.be                = p;     p++;
  param.InvV              = p;
  
  //printf("\nparam: nu0 = %f, nu1=%f, gamma_a=%f, gamma_b=%f",
  //       *(param.nu_0), *(param.nu_1), *(param.gamma_a), *(param.gamma_b));
  //printf("\nparam: ae = %f, be=%f, api_prior=%f",
  //       *(param.ae), *(param.be), *(param.api_prior));
  //for(i = 0; i < totnran; i++){
  //  printf("\n InV[%d,.] = ", i);
  //  for(j = i; j < totnran; j++){
  //    printf("%f, ", param.InvV[i+j*(j+1)/2]);
  //  }
  //}
  
  //Rprintf("\nC: Hyperparameters are set");
  //fflush(stdout);
  
  //// Reading initial values ////
  //// ---------------------- ////
  struct str_state inits;    // pointer to structure where initial values are stored
  double* pp = vecinits;     // pointer for elements of vecinits
  double space_for_naY = 0.0;
  
  inits.w             = pp;      pp+=dimswithG[25];
  inits.e0            = pp;      pp++; //dim is always 1
  inits.U             = pp;      pp+=dimswithG[29];
  inits.pUig          = pp;      pp+=dimswithG[28];
  inits.prec_num      = pp;      pp+=dimswithG[2];
  inits.beta_num_fix  = pp;      pp+=dimswithG[0];
  inits.beta_num      = pp;      pp+=dimswithG[1];
  inits.beta_poi_fix  = pp;      pp+=dimswithG[5];
  inits.beta_poi      = pp;      pp+=dimswithG[6];
  inits.beta_bin_fix  = pp;      pp+=dimswithG[7];
  inits.beta_bin      = pp;      pp+=dimswithG[8];
  inits.beta_ord_fix  = pp;      pp+=dimswithG[9];
  inits.beta_ord      = pp;      pp+=dimswithG[10];
  inits.c_ord         = pp;      pp+=dimswithG[11];
  inits.a_ord         = pp;      pp+=dimswithG[12];
  inits.pi_ord        = pp;      pp+=dimswithG[13];
  inits.beta_cat_fix  = pp;      pp+=dimswithG[14];
  inits.beta_cat      = pp;      pp+=dimswithG[15];
  inits.InvSigma      = pp;      pp+=dimswithG[16];
  inits.InvQ          = pp;      pp+=dimswithG[21];
  inits.b             = pp;      pp+=dimswithG[24];
  
  if(dims[32]>0){
    inits.naY    = pp;
  }else{
    inits.naY    = &space_for_naY;
  }
  
  // determinant calculation
  int g;              // index for class g = 1, ..., G
  double* chol;
  chol = (double *)R_alloc(dims[16]+1, sizeof(double));
  //double chol[dims[16]];
  double* space_for_det;
  space_for_det = (double *)R_alloc(*G, sizeof(double));
  //double space_for_det[*G]; // place for inits of detInvSigma
  inits.detInvSigma = space_for_det;
  
  if(totnran > 0){
    if(varying[2]){
      for(g = 0; g < *G; g++){
        justcholesky(inits.InvSigma + g*dims[16], chol, &totnran);
        detchol(chol, inits.detInvSigma + g, &totnran); 
      }
    }else{
      //for(i = 0; i < totnran; i++){
      //  printf("\n");
      //  for(j = i; j < totnran; j++){
      //    printf("[%d,%d] = %f", i,j,inits.InvSigma[i+j*(j+1)/2]);
      //  }
      //}
      justcholesky(inits.InvSigma, chol, &totnran);
      detchol(chol, inits.detInvSigma, &totnran);
    }
  }// else otherwise there is no InvSigma -> remain just pointers to nothingness
  
  //Rprintf("\nC: Initial values read");
  //fflush(stdout);
  
  ///----------------------------------///
  ///      Predictors preparation      ///
  ///----------------------------------///
  int* K_beta;
  K_beta = (int *)R_alloc(totnY, sizeof(int));
  //int K_beta[totnY];
  int kspec_bi_cat = 0;
  
  for(y = 0; y < nY[0]+nY[1]+nY[2]+nY[3]; y++){
    K_beta[y] = 1;
  }
  for(i = 0; i < nY[4]; i++){
    K_beta[y + i] = Kcat[i];
  }
  
  int update[4];
  //update[0] = update[1] = update[2] = update[3] = 0;
  update[0] = update[1] = update[2] = update[3] = 1;
  
  // Numeric outcomes first
  double* predictor_num;
  predictor_num = (double *)R_alloc((*N * nY[0] * 4) + 1, sizeof(double));
  //double predictor_num[(*N * nY[0] * 4) + 1]; // current value of predictor
  shiftnY = 0;
  
  calculate_predictor_separately(
    Id, X, 
    predictor_num, update,
    // Parameters describing dimensions //
    N, nY +0, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix +shiftnY, ngrp +shiftnY, nran +shiftnY, noff +shiftnY, // all start with numeric variables
    cumnfix +shiftnY, cumngrp +shiftnY, cumnran +shiftnY, cumnoff +shiftnY, 
    &totnran,
    dims+1,     // dimension of single group-specific beta_num
    K_beta +shiftnY, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    inits.beta_num_fix, inits.beta_num, inits.b, inits.U            
  );
  
  //printf("\nC: Numeric predictor calculated");
  //fflush(stdout);
  
  //for(y = 0; y < nY[0]; y++){
  //  for(i = 0; i < 10; i++){
  //    j = (i + *N * y) * 4;
  //    printf("\npredictor_num: i=%d, %f, %f, %f, %f",
  //           i, predictor_num[j], predictor_num[j+1], predictor_num[j+2], predictor_num[j+3]);
  //    
  //  }
  //}
  //fflush(stdout);
  
  //for(y = 0; y < nY[0]; y++){
  //  for(i = *N-10; i < *N; i++){
  //    j = (i + *N * y) * 4;
  //    printf("\npredictor_num: i=%d, %f, %f, %f, %f",
  //           i, predictor_num[j], predictor_num[j+1], predictor_num[j+2], predictor_num[j+3]);
  //    
  //  }
  //}
  //fflush(stdout);
  
  // Poisson outcomes
  double* predictor_poi;
  predictor_poi = (double *)R_alloc((*N * nY[1] * 4) + 1, sizeof(double));
  //double predictor_poi[(*N * nY[1] * 4) + 1]; // current value of predictor
  shiftnY += nY[0];
  
  calculate_predictor_separately(
    Id, X, 
    predictor_poi, update,
    // Parameters describing dimensions //
    N, nY +1, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix +shiftnY, ngrp +shiftnY, nran +shiftnY, noff +shiftnY, 
    cumnfix +shiftnY, cumngrp +shiftnY, cumnran +shiftnY, cumnoff +shiftnY, 
    &totnran,
    dims+6,     // dimension of single group-specific beta_num
    K_beta +shiftnY, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    inits.beta_poi_fix, inits.beta_poi, inits.b, inits.U            
  );
  
  //printf("\nC: Poisson predictor calculated");
  //fflush(stdout);
  
  //for(y = 0; y < nY[1]; y++){
  //  for(i = 0; i < 10; i++){
  //    j = (i + *N * y) * 4;
  //    printf("\npredictor_poi: i=%d, %f, %f, %f, %f",
  //           i, predictor_poi[j], predictor_poi[j+1], predictor_poi[j+2], predictor_poi[j+3]);
  //  }
  //}
  //fflush(stdout);
  
  //for(y = 0; y < nY[1]; y++){
  //  for(i = *N-10; i < *N; i++){
  //    j = (i + *N * y) * 4;
  //    printf("\npredictor_num: i=%d, %f, %f, %f, %f",
  //           i, predictor_poi[j], predictor_poi[j+1], predictor_poi[j+2], predictor_poi[j+3]);
  //  }
  //}
  //fflush(stdout);
  
  
  // Binary outcomes
  double* predictor_bin;
  predictor_bin = (double *)R_alloc((*N * nY[2] * 4) + 1, sizeof(double));
  //double* predictor_bin;
  //predictor_bin = (double*)malloc(*N * nY[2] * 4 * sizeof(double));
  //double predictor_bin[(*N * nY[2] * 4) + 1]; // current value of predictor

  shiftnY += nY[1];

  calculate_predictor_separately(
    Id, X, 
    predictor_bin, update,
    // Parameters describing dimensions //
    N, nY +2, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix +shiftnY, ngrp +shiftnY, nran +shiftnY, noff +shiftnY, 
    cumnfix +shiftnY, cumngrp +shiftnY, cumnran +shiftnY, cumnoff +shiftnY, 
    &totnran,
    dims+8,     // dimension of single group-specific beta_num
    K_beta +shiftnY, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    inits.beta_bin_fix, inits.beta_bin, inits.b, inits.U            
  );
  
  //printf("\nC: Binary predictor calculated");
  //fflush(stdout);
  
  // Ordinal outcomes
  double* predictor_ord;
  predictor_ord = (double *)R_alloc((*N * nY[3] * 4) + 1, sizeof(double));
  //double* predictor_ord;
  //predictor_ord = (double*)malloc(*N * nY[3] * 4 * sizeof(double));
  //double predictor_ord[(*N * nY[3] * 4) + 1]; // current value of predictor
  shiftnY += nY[2];
  
  calculate_predictor_separately(
    Id, X, 
    predictor_ord, update,
    // Parameters describing dimensions //
    N, nY +3, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix +shiftnY, ngrp +shiftnY, nran +shiftnY, noff +shiftnY, 
    cumnfix +shiftnY, cumngrp +shiftnY, cumnran +shiftnY, cumnoff +shiftnY, 
    &totnran,
    dims+10,     // dimension of single group-specific beta_ord
    K_beta +shiftnY, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    inits.beta_ord_fix, inits.beta_ord, inits.b, inits.U            
  );
  
  //printf("\nC: Ordinal predictor calculated");
  //fflush(stdout);
  
  // Categorical outcomes
  double* predictor_cat;
  predictor_cat = (double *)R_alloc(*N * sumKcat * 4, sizeof(double));
  //double* predictor_cat;
  //predictor_cat = (double*)malloc(*N * sumKcat * 4 * sizeof(double));
  //double predictor_cat[(*N * sumKcat * 4) + 1]; // current value of predictor
  shiftnY += nY[3];
  kspec_bi_cat = *(tuning.kspec_bi_cat);
  update[0] = update[1] = update[2] = update[3] = 1;
  
  calculate_predictor_separately(
    Id, X, 
    predictor_cat, update,
    // Parameters describing dimensions //
    N, nY +4, 
    FormulaF, FormulaG, FormulaR, FormulaO, 
    nfix +shiftnY, ngrp +shiftnY, nran +shiftnY, noff +shiftnY, 
    cumnfix +shiftnY, cumngrp +shiftnY, cumnran +shiftnY, cumnoff +shiftnY, 
    &totnran,
    dims+15,     // dimension of single group-specific beta_cat
    Kcat, &kspec_bi_cat,
    // Arrays with necessary parameters from one state //
    inits.beta_cat_fix, inits.beta_cat, inits.b, inits.U            
  );
  
  //printf("\nC: Categorical predictor calculated");
  //fflush(stdout);
  
  //for(y = 0; y < nY[4]; y++){
  //  for(k = 0; k < Kcat[y]; k++){
  //    for(i = 0; i < 10; i++){
  //      j = (i + *N * (cumKcat[y]+k)) * 4;
  //      printf("\npredictor_cat: i=%d, %f, %f, %f, %f",
  //             i, predictor_cat[j], predictor_cat[j+1], predictor_cat[j+2], predictor_cat[j+3]);
  //      
  //    }
  //  }
  //}
  //fflush(stdout);
  
  
  ////-------------------------------////
  //// Parameters for classification ////
  ////-------------------------------////
  int Gp1 = *G + 1;
  //printf("\nC: After Gp1");
  //fflush(stdout);
  int Gp, Gm;                 // number of non-empty/empty clusters
  //printf("\nC: After Gp, Gm");
  //fflush(stdout);
  int* nUg;
  nUg = (int *)R_alloc(Gp1, sizeof(int));
  //int nUg[Gp1];         // number of subjects     in classes 1, ..., G
  //printf("\nC: After nUg");
  //fflush(stdout);
  int* ngg;
  ngg = (int *)R_alloc(Gp1, sizeof(int));
  //int ngg[Gp1];         // number of observations in classes 1, ..., G
  //printf("\nC: After ngg");
  //fflush(stdout);
  int* listUi;
  listUi = (int *)R_alloc(*n * Gp1, sizeof(int));
  //int* listUi;
  //listUi = (int*)malloc(*n * Gp1 * sizeof(int));
  //int listUi[*n * Gp1]; // set of individuals within g-th group
  //printf("\nC: After listUi");
  //fflush(stdout);
  int* lag_proposal_bi;
  lag_proposal_bi = (int *)R_alloc(*n, sizeof(int));
  //int lag_proposal_bi[*n];    // counts of how many times proposal distributions have not been updated, 0 means to be updated
  //printf("\nC: After lag_proposal_bi");
  //fflush(stdout);
  
  for(g = 0; g < *G; g++){
    nUg[g] = 0;
    ngg[g] = 0;
  }
  nUg[*G] = *n;
  ngg[*G] = *N;
  
  for(i = 0; i < *N; i++){
    id = Id[i];       // observation id
    g  = inits.U[id]; // observations class
    ngg[g]++;
  }
  
  
  for(i = 0; i < *n; i++){
    g = inits.U[i];  // subjects class
    listUi[*n * g + nUg[g]] = i; // add to cluster g
    listUi[*n * (*G) + i] = i;   // add to all together 
    lag_proposal_bi[i] = 0;
    nUg[g]++;         
  }
  
  Gm = 0;
  for(g = 0; g<*G; g++){
    if(nUg[g] == 0){
      Gm++;
    }
  }
  Gp = *G - Gm;
  
  //Rprintf("\nC: ng and nUg calculated");
  //for(g = 0; g < *G; g++){
  //  Rprintf("\nC: ng[%d] = %d, nUg[%d] = %d, G=%d, Gp=%d, Gm=%d",
  //          g, ngg[g], g, nUg[g], *G, Gp, Gm);
  //}
  //fflush(stdout);
  
  ////------------------------------////
  //// Inicialization of last state ////
  ////------------------------------////
  
  struct str_state last;              // parameter of last state - will be graduallly updated 
  double* ldetInvSigma;
  ldetInvSigma = (double *)R_alloc(dimswithG[20]+1, sizeof(double));
  //double ldetInvSigma[dimswithG[20]]; // space for last detInvSigma 
  
  pp = veclast;
  
  // 
  // last structure will point to the memory given by input veclast 
  // will be modified gradually during generating states
  // After the generating ends this memory will contain last generated values -->
  // they will be returned back by the function
  //
  
  last.w            = pp;      pp+=dimswithG[25];
  last.e0           = pp;      pp++; // dim is always 1 double
  last.U            = pp;      pp+=dimswithG[29];
  last.pUig         = pp;      pp+=dimswithG[28];
  last.prec_num     = pp;      pp+=dimswithG[2];
  last.beta_num_fix = pp;      pp+=dimswithG[0];
  last.beta_num     = pp;      pp+=dimswithG[1];
  last.beta_poi_fix = pp;      pp+=dimswithG[5];
  last.beta_poi     = pp;      pp+=dimswithG[6];
  last.beta_bin_fix = pp;      pp+=dimswithG[7];
  last.beta_bin     = pp;      pp+=dimswithG[8];
  last.beta_ord_fix = pp;      pp+=dimswithG[9];
  last.beta_ord     = pp;      pp+=dimswithG[10];
  last.c_ord        = pp;      pp+=dimswithG[11];
  last.a_ord        = pp;      pp+=dimswithG[12];
  last.pi_ord       = pp;      pp+=dimswithG[13];
  last.beta_cat_fix = pp;      pp+=dimswithG[14];
  last.beta_cat     = pp;      pp+=dimswithG[15];
  last.InvSigma     = pp;      pp+=dimswithG[16];
  last.InvQ         = pp;      pp+=dimswithG[21];
  last.b            = pp;      pp+=dimswithG[24];
  
  if(dims[32]>0){
    last.naY    = pp;
  }else{
    last.naY    = &space_for_naY;
  }     
  last.detInvSigma = ldetInvSigma;
  
  // Initializing last state with values in inits
  for(i = 0; i < dimswithG[25]; i++){
    last.w[i] = inits.w[i];
  }
  *(last.e0) = *(inits.e0);
  for(i = 0; i < dimswithG[29]; i++){
    last.U[i] = inits.U[i];
  }
  for(i = 0; i < dimswithG[28]; i++){
    last.pUig[i] = inits.pUig[i];
  }
  for(i = 0; i < dimswithG[2]; i++){
    last.prec_num[i] = inits.prec_num[i];
  }
  for(i = 0; i < dimswithG[0]; i++){
    last.beta_num_fix[i] = inits.beta_num_fix[i];
  }
  for(i = 0; i < dimswithG[1]; i++){
    last.beta_num[i] = inits.beta_num[i];
  }
  for(i = 0; i < dimswithG[5]; i++){
    last.beta_poi_fix[i] = inits.beta_poi_fix[i];
  }
  for(i = 0; i < dimswithG[6]; i++){
    last.beta_poi[i] = inits.beta_poi[i];
  }
  for(i = 0; i < dimswithG[7]; i++){
    last.beta_bin_fix[i] = inits.beta_bin_fix[i];
  }
  for(i = 0; i < dimswithG[8]; i++){
    last.beta_bin[i] = inits.beta_bin[i];
  }
  for(i = 0; i < dimswithG[9]; i++){
    last.beta_ord_fix[i] = inits.beta_ord_fix[i];
  }
  for(i = 0; i < dimswithG[10]; i++){
    last.beta_ord[i] = inits.beta_ord[i];
  }
  for(i = 0; i < dimswithG[11]; i++){
    last.c_ord[i] = inits.c_ord[i];
  }
  for(i = 0; i < dimswithG[12]; i++){
    last.a_ord[i] = inits.a_ord[i];
  }
  for(i = 0; i < dimswithG[13]; i++){
    last.pi_ord[i] = inits.pi_ord[i];
  }
  for(i = 0; i < dimswithG[14]; i++){
    last.beta_cat_fix[i] = inits.beta_cat_fix[i];
  }
  for(i = 0; i < dimswithG[15]; i++){
    last.beta_cat[i] = inits.beta_cat[i];
  }
  for(i = 0; i < dimswithG[16]; i++){
    last.InvSigma[i] = inits.InvSigma[i];
  }
  for(i = 0; i < dimswithG[21]; i++){
    last.InvQ[i] = inits.InvQ[i];
  }
  for(i = 0; i < dimswithG[24]; i++){
    last.b[i] = inits.b[i];
  }
  for(i = 0; i < dimswithG[32]; i++){
    last.naY[i] = inits.naY[i];
  }
  for(i = 0; i < dimswithG[20]; i++){
    last.detInvSigma[i] = inits.detInvSigma[i];
  }
  
  //printf("\nC: Last state prepared");
  //fflush(stdout);
  
  
  ////----------------------------------////
  //// Proposal distribution parameters ////
  ////----------------------------------////
  
  //// Poisson outcome
  shiftnY = nY[0];
  // Fixed coefficients
  int* dim_prop_beta_poi_fix;
  dim_prop_beta_poi_fix = (int *)R_alloc(nY[1]+1, sizeof(int));
  //int dim_prop_beta_poi_fix[nY[1]+1];
  int* cumdim_prop_beta_poi_fix;
  cumdim_prop_beta_poi_fix = (int *)R_alloc(nY[1]+1, sizeof(int));
  //int cumdim_prop_beta_poi_fix[nY[1]+1];
  int totdim_prop_beta_poi_fix;
  
  cumdim_prop_beta_poi_fix[0] = 0;
  for(y = 0; y < nY[1]; y++){
    dim_prop_beta_poi_fix[y] = nfix[shiftnY + y]*(nfix[shiftnY + y]+1)/2;
    cumdim_prop_beta_poi_fix[y+1] = cumdim_prop_beta_poi_fix[y] + dim_prop_beta_poi_fix[y];
  }
  totdim_prop_beta_poi_fix = cumdim_prop_beta_poi_fix[nY[1]];
  
  double* prop_beta_poi_fix;
  prop_beta_poi_fix = (double *)R_alloc(dims[5]+1, sizeof(double));
  //double prop_beta_poi_fix[dims[5]+1];
  double* prop_chol_beta_poi_fix;
  prop_chol_beta_poi_fix = (double *)R_alloc(totdim_prop_beta_poi_fix+1, sizeof(double));
  //double prop_chol_beta_poi_fix[totdim_prop_beta_poi_fix+1];
  
  // Group-specific coefficients
  int* dim_prop_beta_poi;
  dim_prop_beta_poi = (int *)R_alloc(nY[1]+1, sizeof(int));
  //int dim_prop_beta_poi[nY[1]+1];
  int* cumdim_prop_beta_poi;
  cumdim_prop_beta_poi = (int *)R_alloc(nY[1]+1, sizeof(int));
  //int cumdim_prop_beta_poi[nY[1]+1];
  int totdim_prop_beta_poi;
  
  cumdim_prop_beta_poi[0] = 0;
  for(y = 0; y < nY[1]; y++){
    dim_prop_beta_poi[y] = ngrp[shiftnY + y]*(ngrp[shiftnY + y]+1)/2;
    cumdim_prop_beta_poi[y+1] = cumdim_prop_beta_poi[y] + dim_prop_beta_poi[y];
  }
  totdim_prop_beta_poi = cumdim_prop_beta_poi[nY[1]];
  
  double* prop_beta_poi;
  prop_beta_poi = (double *)R_alloc(dimswithG[6]+1, sizeof(double));
  //double prop_beta_poi[dimswithG[6]+1];
  double* prop_chol_beta_poi;
  prop_chol_beta_poi = (double *)R_alloc(*G * totdim_prop_beta_poi+1, sizeof(double));
  //double prop_chol_beta_poi[*G * totdim_prop_beta_poi+1];
  
  //printf("\nC: After poi");
  //fflush(stdout);
  
  //// Binary outcome
  shiftnY += nY[1];
  // Fixed coefficients
  int* dim_prop_beta_bin_fix;
  dim_prop_beta_bin_fix = (int *)R_alloc(nY[2]+1, sizeof(int));
  //int dim_prop_beta_bin_fix[nY[2]+1];
  int* cumdim_prop_beta_bin_fix;
  cumdim_prop_beta_bin_fix = (int *)R_alloc(nY[2]+1, sizeof(int));
  //int cumdim_prop_beta_bin_fix[nY[2]+1];
  int totdim_prop_beta_bin_fix;
  
  cumdim_prop_beta_bin_fix[0] = 0;
  for(y = 0; y < nY[2]; y++){
    dim_prop_beta_bin_fix[y] = nfix[shiftnY + y]*(nfix[shiftnY + y]+1)/2;
    cumdim_prop_beta_bin_fix[y+1] = cumdim_prop_beta_bin_fix[y] + dim_prop_beta_bin_fix[y];
  }
  totdim_prop_beta_bin_fix = cumdim_prop_beta_bin_fix[nY[2]];
  
  double* prop_beta_bin_fix;
  prop_beta_bin_fix = (double *)R_alloc(dims[7]+1, sizeof(double));
  //double prop_beta_bin_fix[dims[7]+1];
  double* prop_chol_beta_bin_fix;
  prop_chol_beta_bin_fix = (double *)R_alloc(totdim_prop_beta_bin_fix+1, sizeof(double));
  //double prop_chol_beta_bin_fix[totdim_prop_beta_bin_fix+1];
  
  // Group-specific coefficients
  int* dim_prop_beta_bin;
  dim_prop_beta_bin = (int *)R_alloc(nY[2]+1, sizeof(int));
  //int dim_prop_beta_bin[nY[2]+1];
  int* cumdim_prop_beta_bin;
  cumdim_prop_beta_bin = (int *)R_alloc(nY[2]+1, sizeof(int));
  //int cumdim_prop_beta_bin[nY[2]+1];
  int totdim_prop_beta_bin;
  
  cumdim_prop_beta_bin[0] = 0;
  for(y = 0; y < nY[2]; y++){
    dim_prop_beta_bin[y] = ngrp[shiftnY + y]*(ngrp[shiftnY + y]+1)/2;
    cumdim_prop_beta_bin[y+1] = cumdim_prop_beta_bin[y] + dim_prop_beta_bin[y];
  }
  totdim_prop_beta_bin = cumdim_prop_beta_bin[nY[2]];
  
  double* prop_beta_bin;
  prop_beta_bin = (double *)R_alloc(dimswithG[8]+1, sizeof(double));
  //double prop_beta_bin[dimswithG[8]+1];
  double* prop_chol_beta_bin;
  prop_chol_beta_bin = (double *)R_alloc(*G * totdim_prop_beta_bin+1, sizeof(double));
  //double prop_chol_beta_bin[*G * totdim_prop_beta_bin+1];
  
  //printf("\nC: After bin");
  //fflush(stdout);
  
  //// Ordinal outcome
  shiftnY += nY[2];
  // Fixed coefficients
  int* dim_prop_beta_ord_fix;
  dim_prop_beta_ord_fix = (int *)R_alloc(nY[3]+1, sizeof(int));
  //int dim_prop_beta_ord_fix[nY[3]+1];
  int* cumdim_prop_beta_ord_fix;
  cumdim_prop_beta_ord_fix = (int *)R_alloc(nY[3]+1, sizeof(int));
  //int cumdim_prop_beta_ord_fix[nY[3]+1];
  int totdim_prop_beta_ord_fix;
  
  cumdim_prop_beta_ord_fix[0] = 0;
  for(y = 0; y < nY[3]; y++){
    dim_prop_beta_ord_fix[y] = nfix[shiftnY + y]*(nfix[shiftnY + y]+1)/2;
    cumdim_prop_beta_ord_fix[y+1] = cumdim_prop_beta_ord_fix[y] + dim_prop_beta_ord_fix[y];
  }
  totdim_prop_beta_ord_fix = cumdim_prop_beta_ord_fix[nY[3]];
  
  double* prop_beta_ord_fix;
  prop_beta_ord_fix = (double *)R_alloc(dims[9]+1, sizeof(double));
  //double prop_beta_ord_fix[dims[9]+1];
  double* prop_chol_beta_ord_fix;
  prop_chol_beta_ord_fix = (double *)R_alloc(totdim_prop_beta_ord_fix+1, sizeof(double));
  //double prop_chol_beta_ord_fix[totdim_prop_beta_ord_fix+1];
  
  // Group-specific coefficients
  int* dim_prop_beta_ord;
  dim_prop_beta_ord = (int *)R_alloc(nY[3]+1, sizeof(int));
  //int dim_prop_beta_ord[nY[3]+1];
  int* cumdim_prop_beta_ord;
  cumdim_prop_beta_ord = (int *)R_alloc(nY[3]+1, sizeof(int));
  //int cumdim_prop_beta_ord[nY[3]+1];
  int totdim_prop_beta_ord;
  
  cumdim_prop_beta_ord[0] = 0;
  for(y = 0; y < nY[3]; y++){
    dim_prop_beta_ord[y] = ngrp[shiftnY + y]*(ngrp[shiftnY + y]+1)/2;
    cumdim_prop_beta_ord[y+1] = cumdim_prop_beta_ord[y] + dim_prop_beta_ord[y];
  }
  totdim_prop_beta_ord = cumdim_prop_beta_ord[nY[3]];
  
  //printf("\ntotdim_prop_beta_ord = %d", totdim_prop_beta_ord);
  //fflush(stdout);
  
  double* prop_beta_ord;
  prop_beta_ord = (double *)R_alloc(dimswithG[10]+1, sizeof(double));
  //double prop_beta_ord[dimswithG[10]+1];
  double* prop_chol_beta_ord;
  prop_chol_beta_ord = (double *)R_alloc(*G * totdim_prop_beta_ord+1, sizeof(double));
  //double prop_chol_beta_ord[*G * totdim_prop_beta_ord+1];
  
  //printf("\nC: After ord");
  //fflush(stdout);
  
  //// Ordinal outcome a_ord
  // First parameters for dimensions
  int* dim_prop_a_ord;
  dim_prop_a_ord = (int *)R_alloc(nY[3]+1, sizeof(int));
  //int dim_prop_a_ord[nY[3]+1];
  int* cumdim_prop_a_ord;
  cumdim_prop_a_ord = (int *)R_alloc(nY[3]+1, sizeof(int));
  //int cumdim_prop_a_ord[nY[3]+1];
  int totdim_prop_a_ord;
  int totdim_prop_a_ord_withG;
  
  cumdim_prop_a_ord[0] = 0;
  for(y = 0; y < nY[3]; y++){
    dim_prop_a_ord[y] = Kord[y]*(Kord[y]+1)/2;
    cumdim_prop_a_ord[y+1] = cumdim_prop_a_ord[y] + dim_prop_a_ord[y];
  }
  totdim_prop_a_ord = cumdim_prop_a_ord[nY[3]];
  if(varying[1]){
    // a_ord is group-specific
    totdim_prop_a_ord_withG = *G * totdim_prop_a_ord;
  }else{
    totdim_prop_a_ord_withG = totdim_prop_a_ord;
  }
  
  //printf("\nC: totdim_prop_a_ord = %d, totdim_prop_a_ord_withG = %d",
  //       totdim_prop_a_ord, totdim_prop_a_ord_withG);
  
  double* prop_a_ord;
  prop_a_ord = (double *)R_alloc(dimswithG[12]+1, sizeof(double));
  //double prop_a_ord[dimswithG[12]+1]; // maximal beta_ord within Newton-Raphson
  double* prop_chol_a_ord;
  prop_chol_a_ord = (double *)R_alloc(totdim_prop_a_ord_withG+1, sizeof(double));
  //double prop_chol_a_ord[totdim_prop_a_ord_withG+1]; // upper triangle of variance matrices of proposal distributions
  
  //printf("\nC: After a_ord");
  //fflush(stdout);
  
  //// Categorical outcome
  shiftnY += nY[3];
  // Fixed coefficients
  int* dim_prop_beta_cat_fix;
  dim_prop_beta_cat_fix = (int *)R_alloc(nY[4]+1, sizeof(int));
  //int dim_prop_beta_cat_fix[nY[4]+1];
  int* cumdim_prop_beta_cat_fix;
  cumdim_prop_beta_cat_fix = (int *)R_alloc(nY[4]+1, sizeof(int));
  //int cumdim_prop_beta_cat_fix[nY[4]+1];
  int totdim_prop_beta_cat_fix;
  
  cumdim_prop_beta_cat_fix[0] = 0;
  for(y = 0; y < nY[4]; y++){
    dim_prop_beta_cat_fix[y] = nfix[shiftnY + y]*Kcat[y]*(nfix[shiftnY + y]*Kcat[y]+1)/2;
    cumdim_prop_beta_cat_fix[y+1] = cumdim_prop_beta_cat_fix[y] + dim_prop_beta_cat_fix[y];
  }
  totdim_prop_beta_cat_fix = cumdim_prop_beta_cat_fix[nY[4]];
  
  double* prop_beta_cat_fix;
  prop_beta_cat_fix = (double *)R_alloc(dims[14]+1, sizeof(double));
  //double prop_beta_cat_fix[dims[14]+1];
  double* prop_chol_beta_cat_fix;
  prop_chol_beta_cat_fix = (double *)R_alloc(totdim_prop_beta_cat_fix+1, sizeof(double));
  //double prop_chol_beta_cat_fix[totdim_prop_beta_cat_fix+1];
  
  // Group-specific coefficients
  int* dim_prop_beta_cat;
  dim_prop_beta_cat = (int *)R_alloc(nY[4]+1, sizeof(int));
  //int dim_prop_beta_cat[nY[4]+1];
  int* cumdim_prop_beta_cat;
  cumdim_prop_beta_cat = (int *)R_alloc(nY[4]+1, sizeof(int));
  //int cumdim_prop_beta_cat[nY[4]+1];
  int totdim_prop_beta_cat;
  
  cumdim_prop_beta_cat[0] = 0;
  for(y = 0; y < nY[4]; y++){
    dim_prop_beta_cat[y] = ngrp[shiftnY + y]*Kcat[y]*(ngrp[shiftnY + y]*Kcat[y]+1)/2;
    cumdim_prop_beta_cat[y+1] = cumdim_prop_beta_cat[y] + dim_prop_beta_cat[y];
  }
  totdim_prop_beta_cat = cumdim_prop_beta_cat[nY[4]];
  
  //printf("\ntotdim_prop_beta_cat = %d", totdim_prop_beta_cat);
  //fflush(stdout);
  
  double* prop_beta_cat;
  prop_beta_cat = (double *)R_alloc(dimswithG[15]+1, sizeof(double));
  //double prop_beta_cat[dimswithG[15]+1];
  double* prop_chol_beta_cat;
  prop_chol_beta_cat = (double *)R_alloc(*G * totdim_prop_beta_cat+1, sizeof(double));
  //double prop_chol_beta_cat[*G * totdim_prop_beta_cat+1];
  
  //printf("\nC: After cat");
  //fflush(stdout);
  
  //// Random effects b_i proposal distributions
  int dim_prop_chol_bi = *n * totnran * (totnran + 1) / 2;
  double* prop_bi;
  prop_bi = (double *)R_alloc(dims[24]+1, sizeof(double));
  //prop_bi = (double*)malloc(dims[24] * sizeof(double));
  //double prop_bi[dims[24]+1]; // maximal bi's within Newton-Raphson
  //double prop_chol_bi[dim_prop_chol_bi+1]; 
  double* prop_chol_bi;
  prop_chol_bi = (double *)R_alloc(dim_prop_chol_bi+1, sizeof(double));
  //prop_chol_bi = (double*)malloc(dim_prop_chol_bi * sizeof(double));
  
  
  //printf("\ndims b = %d, dim_prop_chol_bi = %d", 
  //       dims[24], dim_prop_chol_bi);
  //fflush(stdout);
  
  // e0 or rather log(e0) = le0
  double e0_prop_prec;           // precision of proposal distribution of le0 = -d2 log(le0 | ...) at maximized le0 value
  e0_prop_prec = 1.0; 
  
  //Rprintf("\nC: Initialization of proposal distributions done");
  //fflush(stdout);
  
  //// ---------------------- ////
  //// Missing outcome values ////
  //// ---------------------- ////
  //int isYna_inv[*N * totnY];
  int* isYna_inv;
  isYna_inv  = (int *)R_alloc(*N * totnY, sizeof(int));
  //isYna_inv = (int*)malloc(*N * totnY * sizeof(int));
  int nacounter;
  nacounter = 0;
  for(i = 0; i < *N * totnY; i++){
    if(isYna[i]){
      isYna_inv[i] = nacounter;
      nacounter++;
    }else{
      isYna_inv[i] = 0;
    }
  }
  
  //printf("\nC: After nacounter");
  //fflush(stdout);
  
  ////!!!!!!!!!!!!!////
  //// ----------- ////
  //// Gibbs cycle ////
  //// ----------- ////
  ////!!!!!!!!!!!!!////
  
  // Last declarations //
  double* v;
  v = (double *)R_alloc(*G, sizeof(double));
  //double v[*G];                 // Sticks for w when sampling  
  //double prod1minv;             // cumulative product of 1-v
  //int cumnUg;                   // cumulative count of nUg /from behind
  //printf("\nC: After v");
  //fflush(stdout);
  
  int percentage;           // How many percent has been already generated?
  percentage = 0; 
  int newperc;
  newperc = 0;
  int coord;
  coord = 0;
  int picoord;
  picoord = 0;
  //printf("\nC: After picoord");
  //fflush(stdout);
  double new_loglik;
  double pcum, mcum;
  int tog;
  int verbose = 0;
  
  //printf("\nC: All loaded and prepared, about to start sampling");
  //fflush(stdout);
  Rprintf("\n");
  
  // *iter = 1;
  for(m = 0; m < *iter; m++){
    // Printing current iteration number
    //printf("Generating state   %d   out of   %d  ...  %d percent \n", i+1, *iter, 100*(i+1)/(*iter));
    newperc = 100*(m+1)/(*iter);
    
    if(percentage < newperc){
      percentage = newperc;
      //printf("\n");
      Rprintf("Chain: %d, %c%d %c%c", *chain, '(', percentage, '%', ')');
      Rprintf("\r");
      //fflush(stdout);
    }
    
    // Sampling unknown Y values according to current model parameters
    if(dims[32] > 0){
      // there are some missing values
      gibbs_naY(&last, &param, Y, isYna, X, varying,  dims,
                predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat,
                G, N, n, nY, Kord, Kcat, ngrp, cumngrp, FormulaG, n_i, i_j);
      
      if(save[32]){
        for(j = 0; j < dimswithG[32]; j++){
          naY[m*dimswithG[32] + j] = last.naY[j];
        }
      }
      
      if(verbose){
        Rprintf("\nC: naY sampled");
        //fflush(stdout);
      }
    }
    
    //// Numerical outcome parameters - beta_num_fix + beta_num
    gibbs_beta_num(&last, &param,  
                   Id, Y, X, varying, dims,
                   G, N, n, nY, predictor_num,
                   FormulaF, FormulaG, FormulaR, FormulaO,
                   nfix, ngrp, nran, noff, 
                   cumnfix, cumngrp, cumnran, cumnoff, 
                   &totnran,
                   n_i, i_j, nUg, listUi               // IN [n*G] list of who belongs to which cluster
    );
    if(save[0]){
      for(j = 0; j < dimswithG[0]; j++){
        beta_num_fix[m*dimswithG[0] + j] = last.beta_num_fix[j];
      }
    }
    if(save[1]){
      for(j = 0; j < dimswithG[1]; j++){
        beta_num[m*dimswithG[1] + j] = last.beta_num[j];
      }
    }
    
    if(verbose){
      Rprintf("\nC: beta_num_fix and beta_num sampled");
      //fflush(stdout);
    }
    
    
    //// prec_num update (and sd and var)
    gibbs_prec_num(&last, &param, Y, varying, dims, 
                   predictor_num, 
                   G, N, n, nY, ngrp, cumngrp, n_i, i_j, ngg, nUg, listUi);
    if(save[2]){
      for(j = 0; j < dimswithG[2]; j++){
        prec_num[m*dimswithG[2] + j] = last.prec_num[j];
      }
    }
    if(save[3]){
      for(j = 0; j < dimswithG[3]; j++){
        sd_num[m*dimswithG[3] + j] = 1/sqrt(last.prec_num[j]);
      }
    }
    if(save[4]){
      for(j = 0; j < dimswithG[4]; j++){
        var_num[m*dimswithG[4] + j] = 1/last.prec_num[j];
      }
    }
    if(verbose){
      Rprintf("\nC: prec_num sampled");
      //fflush(stdout);
    }
    
    //// Poisson outcome parameters - beta_poi_fix + beta_poi
    metrgibbs_beta_poi(&last, &param, Id, Y, X, varying, dims, 
                       predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat,
                       N, n, nY, G, Kord, Kcat, 
                       nfix, cumnfix, FormulaF,
                       ngrp, cumngrp, FormulaG,
                       nran, cumnran, &totnran, FormulaR,
                       noff, cumnoff, FormulaO,
                       n_i, i_j, nUg, listUi, 
                       prop_beta_poi_fix, prop_chol_beta_poi_fix, cumdim_prop_beta_poi_fix, &totdim_prop_beta_poi_fix,
                       prop_beta_poi, prop_chol_beta_poi, cumdim_prop_beta_poi, &totdim_prop_beta_poi,
                       &m, &tuning 
    );
    
    // Saving sample beta_poi_fix and beta_poi
    if(save[5]){
      for(j = 0; j < dimswithG[5]; j++){
        beta_poi_fix[m*dimswithG[5] + j] = last.beta_poi_fix[j];
      }
    }
    if(save[6]){
      for(j = 0; j < dimswithG[6]; j++){
        beta_poi[m*dimswithG[6] + j] = last.beta_poi[j];
      }
    }
    if(verbose){
      Rprintf("\nC: beta_poi_fix and beta_poi sampled");
      //fflush(stdout);
    }
    
    //// Binary outcome parameters - beta_bin_fix + beta_bin
    metrgibbs_beta_bin(&last, &param, Id, Y, X, varying, dims, 
                       predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat,
                       N, n, nY, G, Kord, Kcat, 
                       nfix, cumnfix, FormulaF,
                       ngrp, cumngrp, FormulaG,
                       nran, cumnran, &totnran, FormulaR,
                       noff, cumnoff, FormulaO,
                       n_i, i_j, nUg, listUi, 
                       prop_beta_bin_fix, prop_chol_beta_bin_fix, cumdim_prop_beta_bin_fix, &totdim_prop_beta_bin_fix,
                       prop_beta_bin, prop_chol_beta_bin, cumdim_prop_beta_bin, &totdim_prop_beta_bin,
                       &m, &tuning 
    );
    
    // Saving sample beta_bin_fix and beta_bin
    if(save[7]){
      for(j = 0; j < dimswithG[7]; j++){
        beta_bin_fix[m*dimswithG[7] + j] = last.beta_bin_fix[j];
      }
    }
    if(save[8]){
      for(j = 0; j < dimswithG[8]; j++){
        beta_bin[m*dimswithG[8] + j] = last.beta_bin[j];
      }
    }
    
    if(verbose){
      Rprintf("\nC: beta_bin_fix and beta_bin sampled");
      //fflush(stdout);
    }
    
    //// Ordinal outcome parameters
    /// beta_ord_fix + beta_ord
    metrgibbs_beta_ord(&last, &param, Id, Y, X, varying, dims, 
                       predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat,
                       N, n, nY, G, Kord, Kcat, 
                       nfix, cumnfix, FormulaF,
                       ngrp, cumngrp, FormulaG,
                       nran, cumnran, &totnran, FormulaR,
                       noff, cumnoff, FormulaO,
                       n_i, i_j, nUg, listUi, 
                       prop_beta_ord_fix, prop_chol_beta_ord_fix, cumdim_prop_beta_ord_fix, &totdim_prop_beta_ord_fix,
                       prop_beta_ord, prop_chol_beta_ord, cumdim_prop_beta_ord, &totdim_prop_beta_ord,
                       &m, &tuning 
    );
    
    // Saving sample beta_ord_fix and beta_ord
    if(save[9]){
      for(j = 0; j < dimswithG[9]; j++){
        beta_ord_fix[m*dimswithG[9] + j] = last.beta_ord_fix[j];
      }
    }
    if(save[10]){
      for(j = 0; j < dimswithG[10]; j++){
        beta_ord[m*dimswithG[10] + j] = last.beta_ord[j];
      }
    }
    if(verbose){
      Rprintf("\nC: beta_ord_fix and beta_ord sampled");
      //fflush(stdout);
    }
    
    /// Updating and sampling a_ord
    metrgibbs_a_ord(&last, &param, Id, Y, X, varying, dims, 
                    predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat,
                    N, n, nY, G, Kord, Kcat,
                    nfix, cumnfix, FormulaF,
                    n_i, i_j, nUg, listUi,
                    prop_a_ord, prop_chol_a_ord, cumdim_prop_a_ord, &totdim_prop_a_ord,
                    &m, &tuning 
    );
    
    // Saving a_ord
    if(save[12]){
      // saving a_ord
      for(j = 0; j < dimswithG[12]; j++){
        a_ord[m*dimswithG[12] + j] = last.a_ord[j];
      }
    }
    
    // updating last.c_ord and last.pi_ord
    if(varying[1]){
      tog = *G;
    }else{
      tog = 1;
    }
    // ordered intercepts + probabilities pi
    for(y = 0; y < nY[3]; y++){
      for(g = 0; g < tog; g++){
        coord = cumKord[y] + g*dims[12];
        // c_ord
        pcum = 0.0;
        mcum = 1.0;
        for(j = 0; j < Kord[y]; j++){
          mcum += exp(last.a_ord[coord+j]);
        }
        for(j = 0; j < Kord[y]; j++){
          pcum += exp(last.a_ord[coord+j]);
          mcum -= exp(last.a_ord[coord+j]);
          last.c_ord[coord+j] = log(pcum / mcum);
        }
        pcum += 1.0;
        // pi_ord
        picoord = cumKord[y] + y + g*dims[13];
        for(j = 0; j < Kord[y]; j++){
          last.pi_ord[picoord+j] = exp(last.a_ord[coord+j]) / pcum;
        }
        last.pi_ord[picoord+j] = 1 / pcum;
      }
    }
    
    // Saving c_ord
    if(save[11]){
      for(j = 0; j < dimswithG[11]; j++){
        c_ord[m*dimswithG[11] + j] = last.c_ord[j];
      }
    } 
    // Saving pi_ord
    if(save[13]){
      for(j = 0; j < dimswithG[13]; j++){
        pi_ord[m*dimswithG[13] + j] = last.pi_ord[j];
      }
    } 
    if(verbose){
      Rprintf("\nC: a_ord, c_ord and pi_ord sampled");
      //fflush(stdout);
    }
    
    //// Categorical outcome parameters - beta_cat_fix + beta_cat
    metrgibbs_beta_cat(&last, &param, Id, Y, X, varying, dims, 
                       predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat,
                       N, n, nY, G, Kord, Kcat, 
                       nfix, cumnfix, FormulaF,
                       ngrp, cumngrp, FormulaG,
                       nran, cumnran, &totnran, FormulaR,
                       noff, cumnoff, FormulaO,
                       n_i, i_j, nUg, listUi, 
                       prop_beta_cat_fix, prop_chol_beta_cat_fix, cumdim_prop_beta_cat_fix, &totdim_prop_beta_cat_fix,
                       prop_beta_cat, prop_chol_beta_cat, cumdim_prop_beta_cat, &totdim_prop_beta_cat,
                       &m, &tuning 
    );
    
    // Saving sample beta_cat_fix and beta_cat
    if(save[14]){
      for(j = 0; j < dimswithG[14]; j++){
        beta_cat_fix[m*dimswithG[14] + j] = last.beta_cat_fix[j];
      }
    }
    if(save[15]){
      for(j = 0; j < dimswithG[15]; j++){
        beta_cat[m*dimswithG[15] + j] = last.beta_cat[j];
      }
    }
    if(verbose){
      Rprintf("\nC: beta_cat_fix and beta_cat sampled");
      //fflush(stdout);
    }
    
    //// InvSigma
    if(dimswithG[16] > 0){
      gibbs_InvSigma(&last, &param, varying, save, dims, dimswithG, 
                     G, n, &totnran, &m, nUg, 
                     Sigma, detInvSigma, sdSigma, corSigma);
      if(save[16]){
        for(j = 0; j < dimswithG[16]; j++){
          InvSigma[m*dimswithG[16] + j] = last.InvSigma[j];
        }
      }
      if(verbose){
        Rprintf("\nC: InvSigma sampled");
        //fflush(stdout);
      }
    }
    
    //// InvQ
    if(dimswithG[21] > 0){
      gibbs_InvQ(&last, &param, varying, save, dims, dimswithG, 
                 G, &totnran, &m, Q, detInvQ);
      if(save[21]){
        for(j = 0; j < dimswithG[21]; j++){
          InvQ[m*dimswithG[21] + j] = last.InvQ[j];
        }
      }
      if(verbose){
        Rprintf("\nC: InvQ sampled");
        //fflush(stdout);
      }
    }
    
    //// Random effects b
    if(dimswithG[24] > 0){
      metrgibbs_bi(&last, &param, Id, Y, X, varying, dims, 
                   predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat,
                   N, n, nY, G, Kord, Kcat,
                   nfix, cumnfix, FormulaF,
                   ngrp, cumngrp, FormulaG,
                   nran, cumnran, &totnran, FormulaR,
                   noff, cumnoff, FormulaO,
                   n_i, i_j, nUg, listUi, 
                   // Proposal distribution parameters
                   prop_bi, prop_chol_bi,
                   //loglik_last_bi, 
                   // Tuning parameters
                   lag_proposal_bi, 
                   &tuning
      );
      
      // Saving sampled bi
      if(save[24]){
        for(j = 0; j < dimswithG[24]; j++){
          b[m*dimswithG[24] + j] = last.b[j];
        }
      }
      if(verbose){
        Rprintf("\nC: b sampled");
        //fflush(stdout);
      }
    }
    
    //// probability w
    // Original solution
    for(g = 0; g < *G; g++){
      v[g] = *(last.e0) + nUg[g];
    }
    // generate new w and store it to last.w
    my_rdirichlet(last.w, G, v);
    
    // Sparse clusters solution
    // First draw sticks v_1, ..., v_G
    //cumnUg = 0;
    //v[*G-1] = 1.0;
    //for(g = *G-2; g >= 0; g--){
    //  cumnUg += nUg[g+1];
    //  v[g] = rbeta(*(last.e0) + nUg[g], *(last.e0)*( *G - g - 1) + cumnUg);
    //  
    //}
    
    //prod1minv = 1.0;
    //for(g = 0; g < *G; g++){
    //  printf("\nC: v[%d]=%f, prod1minv=%f, w[%d]=%f, nUg=%d",g,v[g],prod1minv,g,v[g] * prod1minv,nUg[g]);
    //  fflush(stdout);
    //  last.w[g] = v[g] * prod1minv;
    //  prod1minv *= 1.0-v[g];
    //}
    
    // saving w
    if(save[25]){
      for(g = 0; g < dimswithG[25]; g++)
        w[m*dimswithG[25] + g] = last.w[g];
    }
    if(verbose){
      Rprintf("\nC: w sampled");
      //fflush(stdout);
    }
    
    //// Group=cluster indicators
    gibbs_pUig(&last, &param, Id, Y, isYna, isYna_inv, X, varying, dims,
               predictor_num, predictor_poi, predictor_bin, predictor_ord, predictor_cat,
               G, &Gp, &Gm, N, n, nY, Kord, Kcat,
               nfix, cumnfix, FormulaF,
               ngrp, cumngrp, FormulaG,
               nran, cumnran, &totnran, FormulaR,
               noff, cumnoff, FormulaO,
               n_i, i_j, ngg, nUg, listUi,
               &new_loglik,
               // Tuning parameters
               lag_proposal_bi, &tuning
    );
    if(verbose){
      Rprintf("\ngibbs_pUig has just ended");
      //fflush(stdout);
    }
    
    // Save sampled pUig, U, loglik, Gplus
    if(save[28]){
      for(j = 0; j < dimswithG[28]; j++){
        pUig[m*dimswithG[28] + j] = last.pUig[j];
      }
    }
    if(save[29]){
      for(j = 0; j < dimswithG[29]; j++){
        U[m*dimswithG[29] + j] = last.U[j];
      }
    }
    if(save[27]){
      loglik[m] = new_loglik;
    }
    if(save[30]){
      Gplus[m] = Gp;
    }
    
    if(save[26]){
      for(g = 0; g < *G; g++){
        ng[*G * m + g] = nUg[g];
      }
    }
    
    if(verbose){
      Rprintf("\nC: pUig and U[i] sampled");
      //fflush(stdout);
      
      for(g = 0; g < *G; g++){
        Rprintf("\nC: ng[%d] = %d, nUg[%d] = %d, G=%d, Gp=%d, Gm=%d", 
                g, ngg[g], g, nUg[g], *G, Gp, Gm);
      }
      //fflush(stdout);
    }
    
    //// New precision parameter e0 for prior of w
    // Sampling via log transformation and proposing from N(e0, prop_var)
    metrgibbs_e0(&last, &param,
                 G, &Gp, n, nUg, &m,
                 &e0_prop_prec,
                 // Tuning parameters
                 &tuning
    );
    
    // Saving sampled e0
    if(save[31]){
      e0[m] = *(last.e0);
    }
    if(verbose){
      Rprintf("\nC: e0 sampled\n");
      //fflush(stdout);
    }
    
    
    // All parameters have been updated --> continue with next m
  
  } // end of for m in 1:iter
  
  Rprintf("\n");
  
}




