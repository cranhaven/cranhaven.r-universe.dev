#ifndef PDFS_LAST_H
#define PDFS_LAST_H

void logpYi_g(struct str_state* last,  // IN last known values of generated parameters
              struct str_param* param, // IN hyperparameters
              double* Y,                // IN [totnY*N]
              int* isYna,               // IN [*N * totnY]
              int* isYna_inv,           // IN [*N * totnY]
              double* X,                // IN [N*(#regr)]    regressors
              int* spec,            // [5]                class-specific parameters
              // order:   [0] tau_num, 
              //          [1] c_ord,
              //          [2] InvSigma, 
              //          [3] InvQ, 
              //          [4] naY
              int* dims,            // [33]:              what parameters are saved
              // order:   [0-1]   beta_num_fix, beta_num,
              //          [2-4]   tau_num, sd_num, var_num,
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
              int* g,                 // IN [1] group=cluster number
              int* N,                 // IN [1] number of observations
              int* n,                 // IN [1] number of subjects
              int* nY,                // IN [4]
              int* i,                 // IN [1] number of subject 
              int* Kord,              // IN [nY[2]] total number of categories of Ord outcomes -1
              int* Kcat,              // IN [nY[3]] total number of categories of Cat outcomes -1
              int* ngrp,              // IN [1] number of GROUP-SPECIFIC  regressors or each response
              int* cumngrp,           // IN [1] where to start in FormulaG
              int* FormulaG,          // IN [totngrp]  numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
              int* n_i,               // IN [n] number of observations dedicated to i-th subject
              int* i_j,               // IN [n * max_n_j] indeces dedicated to i-th subject 
              int* print,             // IN [1] T/F Should we print sequentially what is going on?
              double* value           // OUT [1]
);
#endif
