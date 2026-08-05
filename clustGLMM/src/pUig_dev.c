//
//  PURPOSE:   Calculation of classification probabilitites for 
//              subjects from the study (and possibly other as well)
//              GLMM based model for Nums, Pois, Bins, Ords, Cats
//              uses Adaptive Gaussian Quadrature Approximation of integrals
//              also calculates deviance of all available data
//
//  AUTHOR:    Jan Vavra
//             vavraj[AT]karlin.mff.cuni.cz
//
//  LOG:       20210914  created
//
// ================================================================
//
// To obtain a dll file in Windows, run
//
//    library("callr")
//    rcmd(cmd = "SHLIB", cmdargs = "pUig_dev.c")
//
//    To be precise, you need to compile all in one:
// rcmd(cmd = "SHLIB", cmdargs = c("pUig_dev.c",
//                                  "cholesky.c",
//                                  "matrmult.c",
//                                  "my_math.c",
//                                  "newton_raphson_bi_dev.c",
//                                  "pdfs_derivatives_bi_dev.c"))
//
// To use declared functions in R use
//
//    dyn.load("./pUig_dev.dll")
//


/*** These are headers available within the R source tree      ***/
/*** that provide mathematical and also many statistical       ***/
/*** functions (densities, cdf's, random number generators)    ***/
/*** available in R itself.                                    ***/

#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>
#define _USE_MATH_DEFINES
#include <math.h>
#include <stdio.h>

#include "my_math.h"
#include "matrmult.h"
#include "cholesky.h"

#include "newton_raphson_bi_dev.h"
#include "pdfs_derivatives_bi_dev.h"

#define LARGE_VALUE 1000000000000
#define LOW_VALUE  -1000000000000
#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

/*** ================================================================================ ***/
/*** THE KEY PART OF THE CODE ***/
/*** Function that performs integration of latent variables ***/
/*** to obtain model deviance and classification probabilities ***/
/*** ================================================================================ ***/


void pUig_dev( 
    /** IN parameters **/
    int* Id,              // [N] IDs
    double* Y,            // [N*sum(nY)]  responses
    int* isYna,           // [N*sum(nY)]  0 = Y value present, 1 = Y value is NA
    double* X,            // [N*#regr]    regressors
    int* varying,            // [5]                class-specific parameters
    // order:   [0] prec_num, 
    //          [1] c_ord,
    //          [2] InvSigma, 
    //          [3] InvQ, 
    //          [4] naY
    /** Parameters describing dimensions **/
    int* chain,           // [1]        number of generated chain (just for printing)
    int* G,               // [1]        number of classes
    int* iter,            // [1]        total number of generated states
    int* N,               // [1]        total number of observations
    int* n,               // [1]        total number of subjects (different ids in the dataset)
    int* nY,              // [5]        counts of Nums, Ords and Bins variables
    int* FormulaF,        // [totnfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
    int* FormulaG,        // [totnfrp]  numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
    int* FormulaR,        // [totnran]  numbers of columns of X that should be used for RANDOM effects of modelled responses
    int* FormulaO,        // [totnoff]  numbers of columns of X that should be used for OFFSET effects of modelled responses
    int* nfix,            // [sum(nY)]  number of FIXED  regressors for each response
    int* ngrp,            // [sum(nY)]  number of GROUP-SPECIFIC  regressors for each response
    int* nran,            // [sum(nY)]  number of RANDOM regressors for each response
    int* noff,            // [sum(nY)]  number of OFFSET regressors for each response
    int* totnran,         // [1]        total number of random effects across all vars
    int* Kord,            // [nY[Ords]]  the counts of categories of ordinal variables -1
    int* Kcat,            // [nY[Cats]]  the counts of categories of ordinal variables -1
    int* dims,            // [17]:      the length of subarray that corresponds to one state (disected by various parameters)
    // order:   [0-1]   beta_num_fix, beta_num, 
    //          [2]     prec_num, 
    //          [3-4]   beta_poi_fix, beta_poi, 
    //          [5-6]   beta_bin_fix, beta_bin, 
    //          [7-8]   beta_ord_fix, beta_ord, 
    //          [9]     c_ord, 
    //          [10-11] beta_cat_fix, beta_cat, 
    //          [12]    InvSigma,
    //          [13]    w,
    //          [14]    pUig_int,
    //          [15]    deviance
    //          [16]    dev_i
    int* dimswithG,       // [17]       the length of subarray that corresponds to one state 
    // order:   [0-1]   beta_num_fix, beta_num, 
    //          [2]     prec_num, 
    //          [3-4]   beta_poi_fix, beta_poi, 
    //          [5-6]   beta_bin_fix, beta_bin, 
    //          [7-8]   beta_ord_fix, beta_ord, 
    //          [9]     c_ord, 
    //          [10-11] beta_cat_fix, beta_cat, 
    //          [12]    InvSigma,
    //          [13]    w,
    //          [14]    pUig_int,
    //          [15]    deviance
    //          [16]    dev_i
    int* kspec_bi_cat,    // [1]        TRUE = each cat var has different set of bi for each of K-1 levels
    //            FALSE = all levels of cat var have one set of bi - compares last category with others
    //double* predictor,
    // those are used to construct totnfix, totnran and cummulative versions
    /** Arrays to store generated states **/
    /** Some of them might be NULL (see whatsave and calc) **/
    double* beta_num_fix, // [(B+M)*sum(nfix[Nums])]   Beta parameters for fixed effects of numerical variables
    double* beta_num,     // [(B+M)*sum(ngrp[Nums])*G] Beta parameters for group-specific effects of numerical variables
    double* prec_num,     // [(B+M)*nY[0]*(G)]         Precision parameter of numerical variables
    double* beta_poi_fix, // [(B+M)*sum(nfix[Pois])]   Beta parameters for fixed effects of Poisson variables
    double* beta_poi,     // [(B+M)*sum(ngrp[Pois])*G] Beta parameters for group-specific effects of Poisson variables
    double* beta_bin_fix, // [(B+M)*sum(nfix[Bins])]   Beta parameters for fixed effects of binary variables
    double* beta_bin,     // [(B+M)*sum(nfix[Bins])*G] Beta parameters for group-specific effects of binary variables
    double* beta_ord_fix, // [(B+M)*sum(nfix[Ords])]   Beta parameters for fixed effects of ordinal variables
    double* beta_ord,     // [(B+M)*sum(nfix[Ords])*G] Beta parameters for group-specific effects of ordinal variables
    double* c_ord,        // [(B+M)*sum(Kord)(* G)]    Ordered intercepts for ordinal variables
    double* beta_cat_fix, // [(B+M)*sum(nfix[Cats]*Kcat)]   Beta parameters for fixed effects of categorical variables
    double* beta_cat,     // [(B+M)*sum(nfix[Cats]*Kcat)*G] Beta parameters for group-specific effects of categorical variables
    double* InvSigma,     // [(B+M)*totnran*(totnran+1)/2(* G)] Precision matrix of random effects
    double* w,            // [(B+M)*G]                    Class probabilities
    double* pUig_int,     // [(B+M)*(n or N) * G]         Probability of i-th subject belong to each class
    double* deviance,     // [(B+M) * 1]               Deviance of the model
    double* dev_i,        // [(B+M) * (n or N)]        Individual contributions to deviance 
    int* dynamic_prob,    // [1] Should probabilities be calculated dynamically? --> (n or N) prob calculations
    int* NGQ,             // [1] number of roots within one dimension
                          // if==1 then only single Laplace approximation is performed
    double* roots,        // [NGQ] roots of Hermitean polynomials at which to evaluate normalized underlying distribution of b_i
    double* weights,      // [NGQ] weights of roots of Hermitean polynomials
    double* tolerance,    // [1] tolerance when computing norm of the shift within Newton-Raphson
    int* maxiter,         // [1] maximum iterations allowed during Newton-Raphson update of proposal distribution
    int* maxnrep          // [1] maximum number of repetitions of Newton-Raphson from different starting points
)
{
  //// Declarations ////
  //// ------------ ////
  int g, i, ij, l, k, m;    // looping indeces
  int y;                // index variable for outcomes
  
  //// Calculation of useful dimensions, etc. ////
  //// -------------------------------------- ////
  // Total number of responses
  int totnY;            // total number of responses
  totnY = 0;
  for(i = 0; i < 5; i++)
    totnY += nY[i];
  
  // Total and cummulative numbers of fixed and random regressors
  // Will be needed separately for Nums, Bins, Ords and Cats
  int* cumnfix;
  cumnfix = (int*)R_alloc((totnY + 1),  sizeof(int));
  //int cumnfix [totnY + 1];      // cummulative number of fixed regressors of alltogether
  int* cumngrp;
  cumngrp = (int*)R_alloc((totnY + 1),  sizeof(int));
  //int cumngrp [totnY + 1];      // cummulative number of group-specific regressors of alltogether
  int* cumnran;
  cumnran = (int*)R_alloc((totnY + 1),  sizeof(int));
  //int cumnran [totnY + 1];      // cummulative number of random regressors of alltogether
  int* cumnoff;
  cumnoff = (int*)R_alloc((totnY + 1),  sizeof(int));
  //int cumnoff [totnY + 1];      // cummulative number of offset regressors of alltogether
  
  cumnfix[0] = cumngrp[0] = cumnran[0] = cumnoff[0] = 0;
  for(y = 0; y < totnY; y++){
    cumnfix[y+1] = cumnfix[y] + nfix[y];
    cumngrp[y+1] = cumngrp[y] + ngrp[y]; 
    cumnran[y+1] = cumnran[y] + nran[y];
    cumnoff[y+1] = cumnoff[y] + noff[y]; 
  }
  
  // Subjects 
  int* n_i;
  n_i = (int*)R_alloc(*n, sizeof(int));
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
  i_j = (int*)R_alloc(*n * max_n_i, sizeof(int));
  //int i_j [*n * max_n_i];      // matrix of indeces to j-th subject
  int* nn_ii;
  nn_ii = (int*)R_alloc(*n, sizeof(int));
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
  
  //// All combinations of roots ////
  int nrootcombs = 1;
  
  for(k = 0; k < *totnran; k++){
    nrootcombs *= *NGQ;
  }
  
  //fflush(stdout);
  
  int whichroot;
  int auxl;
  double* z;
  z = (double*)R_alloc(nrootcombs * *totnran + 1, sizeof(double));
  //double z[nrootcombs * *totnran];
  //double zw[*totnran];
  //double normz;
  double* lognormw;
  lognormw = (double*)R_alloc(nrootcombs, sizeof(double));
  //double lognormw[nrootcombs];
  
  m = 0;
  for(l = 0; l < nrootcombs; l++){
    // transfer l into *NGQ-system
    auxl = l;
    lognormw[l] = 0.0;
    //printf("\n l = %d, ", l);
    for(k = 0; k < *totnran; k++){
      if(auxl == 0){
        whichroot = 0;
      }else{
        whichroot = (auxl % *NGQ); // m = l*(*totnran) + k
        auxl = (auxl - whichroot)/(*NGQ);
      }
      //printf("rootcombs[%d]=%d, auxl=%d", m, rootcombs[m], auxl);
      
      // Computing the weights ahead
      z[m] = roots[whichroot];
      lognormw[l] += z[m] * z[m];
      lognormw[l] += log(weights[whichroot]);
      
      m++;
    }
  }
  
  // if totnran == 0, then z does not exist and lognormw=0 is of size 1 
  
  //fflush(stdout);
  
  //// Declarations ////
  double* lp;
  lp = (double*)R_alloc(*G, sizeof(double));
  //double lp [*G];             // probabilities
  //double dev;                 // deviance
  double sump;                // sum of probabilities
  
  int percentage = 0;         // How many percent has been already generated?
  int newperc = 0;
  
  double* pw;                             // pointer to w
  double* pprec_num, *ppprec_num;            // pointer to prec_num
  double* pbeta_num_fix;                  // pointer to beta_num_fix
  double* pbeta_num, *ppbeta_num;          // pointer to beta_num
  double* pbeta_poi_fix;                  // pointer to beta_poi_fix
  double* pbeta_poi, *ppbeta_poi;          // pointer to beta_poi
  double* pbeta_bin_fix;                  // pointer to beta_bin_fix
  double* pbeta_bin, *ppbeta_bin;          // pointer to beta_bin
  double* pbeta_ord_fix;                  // pointer to beta_ord_fix
  double* pbeta_ord, *ppbeta_ord;          // pointer to beta_rod
  double* pc_ord, *ppc_ord;                // pointer to c_ord
  double* pbeta_cat_fix;                  // pointer to beta_cat_fix
  double* pbeta_cat, *ppbeta_cat;          // pointer to beta_cat
  double* pInvSigma, *ppInvSigma;          // pointer to InvSigma
  
  // the following does not exist when totnran=0
  double* chol;
  chol = (double*)R_alloc(dimswithG[12] + 1, sizeof(double));
  //double chol[dimswithG[12]];         // for Cholesky decomposition of InvSigma
  double* pchol;                      // pointer for chol decomposition
  double logdetcholInvSigma;         // log of determinant of cholesky decomposition of InvSigma
  double* shifted_bi;
  shifted_bi = (double*)R_alloc(*totnran + 1, sizeof(double));
  //double shifted_bi[*totnran];        // bi_hat + shift given by scaled combination of roots
  
  int from;
  int idnew;
  int nnew;
  
  if(*dynamic_prob){
    nnew = *N;
  }else{
    nnew = *n;
  }
  
  //fflush(stdout);
  
  
  // the following does not exist when totnran=0
  //double bi_hat[*totnran * nnew * *G];    // bi maximizing the log (p(yi | bi) * p(bi))
  //double chol_at_bi_hat[dims[12]];    // cholesky dec of d2 at bi maximizing the log (p(yi | bi) * p(bi))
  double* bi_hat;
  bi_hat = (double*)R_alloc(*totnran * nnew * *G + 1, sizeof(double));
  double* chol_at_bi_hat;
  chol_at_bi_hat = (double*)R_alloc(dims[12] + 1, sizeof(double));
  
  
  for(l = 0; l < *totnran * nnew * *G; l++){
    bi_hat[l] = 0.0;
    //bi_hat[l] = rnorm(0.0, 0.75);
  }
  
  for(l = 0; l < *totnran; l++){
    chol_at_bi_hat[l*(l+3)/2] = 1.0;
    for(k = l+1; k < *totnran; k++){
      chol_at_bi_hat[l + k*(k+1)/2] = 0.0;
    }
  }
  
  int aux_iter;
  int converged;
  int verbose = 0;
  int nrep;
  double max_value = LOW_VALUE;
  double value_at_shifted_bi;
  double approx_sum;
  double approx;
  double maxlpg;
  
  Rprintf("\n");
  //fflush(stdout);
  
  
  //for(m = 0; m < 1; m++){
  for(m = 0; m < *iter; m++){
    // Printing current iteration number
    //printf("Generating state   %d   out of   %d  ...  %d percent \n", i+1, *iter, 100*(i+1)/(*iter));
    newperc = 100*(m+1)/(*iter);
    
    if(percentage < newperc){
      percentage = newperc;
      //printf("Chain: %d, %d\n", *chain, percentage);
      Rprintf("Chain: %d, %c%d %c%c", *chain, '(', percentage, '%', ')');
      Rprintf("\r");
      //fflush(stdout);
    }
    
    // deviance starts from zero
    deviance[m] = 0.0;
    
    // pointer actualisation
    // w
    pw = w + m*dimswithG[13];
    // beta_num
    pbeta_num_fix = beta_num_fix + m*dims[0];
    ppbeta_num = beta_num + m*dimswithG[1];
    ppprec_num = prec_num + m*dimswithG[2];
    // beta_poi
    pbeta_poi_fix = beta_poi_fix + m*dims[3];
    ppbeta_poi = beta_poi + m*dimswithG[4];
    // beta_bin
    pbeta_bin_fix = beta_bin_fix + m*dims[5];
    ppbeta_bin = beta_bin + m*dimswithG[6];
    // beta_ord
    pbeta_ord_fix = beta_ord_fix + m*dims[7];
    ppbeta_ord = beta_ord + m*dimswithG[8];
    // c_ord
    ppc_ord = c_ord + m*dimswithG[9];
    // beta_cat
    pbeta_cat_fix = beta_cat_fix + m*dims[10];
    ppbeta_cat = beta_cat + m*dimswithG[11];
    // InvSigma
    ppInvSigma = InvSigma + m*dimswithG[12];
    
    // cholesky decomposition of InvSigmas 
    // (ahead to not to compute it several times)
    pchol = chol;
    
    if(varying[2]){
      for(g = 0; g < *G; g++){
        justcholesky(ppInvSigma + g*dims[12], pchol, totnran);
        pchol += dims[12];
      }
    }else{
      justcholesky(ppInvSigma, pchol, totnran);
    }
    
    //printf("\n InvSigma[%d]:", m);
    //for(l = 0; l < *totnran; l++){
    //  printf("\n");
    //  for(k = l; k < *totnran; k++){
    //    printf("[%d,%d] = %f", l, k, ppInvSigma[l + k*(k+1)/2]);
    //  }
    //}
    //fflush(stdout);
    
    //printf("\n chol[%d]:", m);
    //for(l = 0; l < *totnran; l++){
    //  printf("\n");
    //  for(k = l; k < *totnran; k++){
    //    printf("[%d,%d] = %f", l, k, chol[l + k*(k+1)/2]);
    //  }
    //}
    //fflush(stdout);
    
    idnew = 0; // counter of subjects (even dynamically)
    
    // Cycle through all subjects
    //for(i = 0; i < 1; i++){
    for(i = 0; i < *n; i++){
      // Cycle through all classes
      
      // If probabilities should be calculated dynamically
      if(*dynamic_prob){
        from = 1;
      }else{
        from = n_i[i];
      }
      
      sump = 1.0; // in case nothing is done with it
      
      // Dynamic probabilities calculation
      // if no dynamic then only all observations are used, ij=n_i[j]
      // for deviation calculation we only need sump from last iteration
      // --> the one corresponding to all abservations at our disposal
      maxlpg = 0.0;
      for(ij = from; ij <= n_i[i]; ij++){
        
        sump = 0.0;
        maxlpg = LOW_VALUE;
        for(g = 0; g < *G; g++){
          // The goal is to fill in lp[g] with corresponding log w + ell_AGQ
          // and to add it to sump, which is needed for the overall deviance
          
          //// Step 0 - pointer actualisation
          // beta_num
          pbeta_num = ppbeta_num + g*dims[1];
          // prec_num
          if(varying[0]){
            pprec_num = ppprec_num + g*dims[2];
          }else{
            pprec_num = ppprec_num;
          }
          // beta_poi
          pbeta_poi = ppbeta_poi + g*dims[4];
          // beta_bin
          pbeta_bin = ppbeta_bin + g*dims[6];
          // beta_ord
          pbeta_ord = ppbeta_ord + g*dims[8];
          // c_ord
          if(varying[1]){
            pc_ord = ppc_ord + g*dims[9];
          }else{
            pc_ord = ppc_ord;
          }
          // beta_cat
          pbeta_cat = ppbeta_cat + g*dims[11];
          
          // InvSigma
          if(varying[2]){
            pInvSigma = ppInvSigma + g*dims[12];
            pchol = chol + g*dims[12];
          }else{
            pInvSigma = ppInvSigma;
            pchol = chol;
          }
          
          //// Step 1 - start with log w
          if(pw[g] == 0){
            lp[g] = LOW_VALUE;
          }else{
            lp[g] = log(pw[g]);
          }
          
          //// Step 2 - add log determinant of chol InvSigma
          // it may be omitted if InvSigma not cluster-specific
          // however we still include it for deviance
          logdetcholInvSigma = 0.0;
          for(l = 0; l < *totnran; l++){
            logdetcholInvSigma += log(pchol[l*(l+3)/2]); // product of elements on the diagonal
          }
          // remains zero if totnran=0
          lp[g] += logdetcholInvSigma;
          if(isfinite(lp[g]) == 0){
            //printf("\nlp is infinite after addition of logdetcholInvSigma: i = %d, g = %d, lp = %f",
            //       i, g, lp[g]);
          }
          
          //// Step 3 - perform Newton-Raphson for finding bi_hat
          // dont forget that ij stands the maximal index of observation to be used
          //printf("\npUig_dev: idnew = %d", idnew);
          //fflush(stdout);
          
          //printf("\nInvSigma: ");
          //for(l = 0; l < *totnran*(*totnran + 1)/2; l++){
          //  printf("[%d] = %f, ", l, pInvSigma[l]);
          //}
          if(*totnran > 0){
            converged = 0;
            nrep = 0;
            while((!converged) & (nrep < *maxnrep)){
              newton_raphson_bi_dev(Y, isYna, X,
                                    pbeta_num_fix, pbeta_num, pprec_num, 
                                    pbeta_poi_fix, pbeta_poi, 
                                    pbeta_bin_fix, pbeta_bin, 
                                    pbeta_ord_fix, pbeta_ord, pc_ord, 
                                    pbeta_cat_fix, pbeta_cat, 
                                    pInvSigma,
                                    N, n, nY, &i, Kord, Kcat, 
                                    nfix, cumnfix, FormulaF,
                                    ngrp, cumngrp, FormulaG,
                                    nran, cumnran, totnran, FormulaR,
                                    noff, cumnoff, FormulaO,
                                    &ij, i_j, kspec_bi_cat, 
                                    bi_hat + *totnran * idnew * *G + g * *totnran,         // OUT [dims[.]] theta maximizing the function
                                    chol_at_bi_hat, // OUT [dims[.] * (dims[.]+1) / 2]
                                    // Tuning parameters
                                    tolerance, maxiter, maxnrep,
                                    &aux_iter, &converged, &max_value          // OUT [1] the value of maximized log-likelihood
              );
              if(!converged){
                nrep++;
                for(l = 0; l<*totnran; l++){
                  bi_hat[l + *totnran * idnew * *G + g * *totnran] = rnorm(0.0, 3.0);
                }
              }
            }
            if(isfinite(max_value) == 0){
              //printf("\n maxvalue from newton raphson is infinite!");
              //fflush(stdout);
            }
          }
          
          
          //// Step 4 subtract logdet of chol_at_bi_hat
          for(l = 0; l < *totnran; l++){
            lp[g] -= log(chol_at_bi_hat[l*(l+3)/2]); // product of elements on the diagonal
          }
          // nothing is done when totnran=0
          if(isfinite(lp[g]) == 0){
            //printf("\nlp is infinite after addition of log(chol_at_bi_hat): i = %d, g = %d, lp = %f",
            //       i, g, lp[g]);
            //fflush(stdout);
          }
          
          //// Step 5 - Approximate the integral
          approx_sum = approx = 0.0;
          //if(j < 5){
          //  printf("\nApproximation: i = %d, ij = %d, g = %d, nrootcombs = %d, NGQ = %d",
          //         j, ij, g, nrootcombs, *NGQ);
          //}
          for(l = 0; l < nrootcombs; l++){
            // a) Start with adding logarithm of pre-calculated weight 
            //    corresponding to l-th combination of roots
            approx = lognormw[l];
            //printf("\nApproximation: i = %d, g = %d, l = %d, approx = %f, approx_sum = %f",
            //       i, g, l, approx, approx_sum);
            
            // b) computing shifted bi
            // l-th combination of roots is stored in z after element *totnran * l
            // solve for chol * x = z to scale the roots z
            //printf("\nz");
            //for(k = 0; k < *totnran; k++){
            //  printf("[%d] = %f, ", k, z[*totnran * l  + k]);
            //}
            if(*totnran > 0){
              backsolve2(chol_at_bi_hat, z + *totnran * l , totnran, shifted_bi);
              // add the optimal bi_hat
              //printf("\nshifted_bi:");
              for(k = 0; k < *totnran; k++){
                //printf("[%d] = %f, ", k, shifted_bi[k]);
                shifted_bi[k] += bi_hat[*totnran * idnew * *G + g * *totnran + k];
                //printf("[%d] = %f\n", k, shifted_bi[k]);
              }
            }else{
              // shifted_bi does not exist
              shifted_bi[0] = 0.0;
            }
              

            
            // c) evaluate log( p(yi | bi) * p(bi) ) at this shifted bi
            logpbi_dev(Y, isYna, X,  
                       pbeta_num_fix, pbeta_num, pprec_num, 
                       pbeta_poi_fix, pbeta_poi, 
                       pbeta_bin_fix, pbeta_bin, 
                       pbeta_ord_fix, pbeta_ord, pc_ord, 
                       pbeta_cat_fix, pbeta_cat, 
                       pInvSigma,
                       N, n, nY, &i, Kord, Kcat, 
                       nfix, cumnfix, FormulaF,
                       ngrp, cumngrp, FormulaG,
                       nran, cumnran, totnran, FormulaR,
                       noff, cumnoff, FormulaO,
                       &ij, i_j, kspec_bi_cat, 
                       &verbose, shifted_bi, &value_at_shifted_bi);
            if(isfinite(value_at_shifted_bi) == 0 ){
              //printf("\nApproximation: i = %d, g = %d, l = %d, approx = %f, value = %f",
              //      i, g, l, approx, value_at_shifted_bi);
              //fflush(stdout);
              verbose = 1;
              logpbi_dev(Y, isYna, X,  
                         pbeta_num_fix, pbeta_num, pprec_num, 
                         pbeta_poi_fix, pbeta_poi, 
                         pbeta_bin_fix, pbeta_bin, 
                         pbeta_ord_fix, pbeta_ord, pc_ord, 
                         pbeta_cat_fix, pbeta_cat, 
                         pInvSigma,
                         N, n, nY, &i, Kord, Kcat, 
                         nfix, cumnfix, FormulaF,
                         ngrp, cumngrp, FormulaG,
                         nran, cumnran, totnran, FormulaR,
                         noff, cumnoff, FormulaO,
                         &ij, i_j, kspec_bi_cat, 
                         &verbose, shifted_bi, &value_at_shifted_bi);
            }else{
              verbose = 0;
            }
            
            // d) add the weighted exp value_at_shifted_bi to the approximation sum
            approx += value_at_shifted_bi; // weight enriched by the value at shifted bi
            approx_sum += exp(approx);
            
            //if(isfinite(log(approx_sum)) == 0 ){
            //  printf("\nApproximation: i = %d, g = %d, l = %d, approx = %f, approx_sum = %f, log(approx_sum) = %f",
            //         j, g, l, approx, approx_sum, log(approx_sum));
            //  fflush(stdout);
            //}
          }
          
          //// Step 6 - add log of approx_sum  to the log-likelihood
          if((*NGQ == 1) | (totnran == 0)){
            lp[g] += approx;
          }else{
            lp[g] += log(approx_sum);
          }
          if(isfinite(lp[g]) == 0){
            //printf("\nlp: i = %d, g = %d, lp = %f",
            //       i, g, lp[g]);
            //fflush(stdout);
          }
          
          //// Step 7 - update the maximal value among lp[g]
          if(lp[g] > maxlpg){
            maxlpg = lp[g];
          }
          
          
          //sump += exp(lp[g]);
          //printf("\nprobs: i = %d, g = %d, sump = %f, approx_sum = %f, lp[g] = %f",
          //       j, g, sump, approx_sum, lp[g]);
          
                             
        } // end of for g
        
        //// Step 7 - add exp of log probs (-the max lp[g]) to sump
        for(g = 0; g < *G; g++){
          lp[g] = lp[g] - maxlpg;
          sump += exp(lp[g]);
        }
        
        for(g = 0; g < *G; g++){
          pUig_int[m*dimswithG[14] + nnew * g + idnew] = exp(lp[g]) / sump;
        }
        
        
        //printf("idnew = %d, nnew = %d, sump = %f\n", idnew, nnew, sump);
        //fflush(stdout);
        idnew++;
        
      } // end of for ij in from:n_i[j]
      
      //printf("\nprobs: i = %d, sump = %f, dev = %f",
      //         j, sump, dev);
        
      // sump now posseses integral based on all ij=n_i[j] observations
      // increase deviance 
      
      dev_i[*n * m + i] = -2.0 * (log(sump) + maxlpg);
      // update final deviance 
      deviance[m] += dev_i[*n * m + i];
      
      //if((i <= 2) && (j <= 5) && (*chain == 1)){
      if(isfinite(sump) == 0){
        //printf("\nidnew = %d, dev_i = %f, deviance = %f, sump = %f", 
        //       idnew, dev_i[*n * m + i], deviance[m], sump);
      }
      //}
      //fflush(stdout);
      
    } // end of for i
    
    //printf("\n");
    //fflush(stdout);
    
  } // end of for m
  
  //printf("\nlog = %f", log(10e-5));
  //printf("\nlog = %f", log(10e-10));
  //printf("\nlog = %f", log(10e-15));
  //printf("\nlog = %f", log(10e-20));
  //printf("\nlog = %f", log(10e-50));
  //printf("\nlog = %f", log(10e-100));
  //printf("\nlog = %f", log(10e-200));
  //printf("\nlog = %f", log(10e-500));
  //printf("\nlog = %f", log(10e-1000));
  //printf("\nlog = %f", log(10e-2000));
  //fflush(stdout);
  
}

