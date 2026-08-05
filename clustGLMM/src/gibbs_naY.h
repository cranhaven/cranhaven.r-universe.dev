#ifndef GIBBS_NAY_H
#define GIBBS_NAY_H

void gibbs_naY(struct str_state* last,   // OUT+IN last known values of generated parameters
               struct str_param* param,  // IN hyperparameters
               double* Y,                // IN+OUT [*N * (totnY)]
               int* isYna,               // IN [*N * totnY]
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
               double* predictor_cat,    // IN [N*2*nY]  both F and R parts of predictor for numeric outcomes 
               int* G,                   // IN [1]        number of groups=classes=clusters
               int* N,                   // IN [1] number of observations
               int* n,                   // IN [1] number of subjects
               int* nY,                  // IN [4] counts of each of the outcome types
               int* Kord,                // IN [nY[2]] number of categories of each of the Ord outcomes -1
               int* Kcat,                // IN [nY[3]] number of categories of each of the Cat outcomes -1
               int* ngrp,                // IN [totnY] number of FIXED  regressors or each response
               int* cumngrp,             // IN [totnY] where to start in FormulaF
               int* FormulaG,            // IN [nfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
               int* n_i,                 // IN [n] number of observations dedicated to j-th subject
               int* i_j                 // IN [n * max_n_j] indeces dedicated to j-th subject 
);   
#endif
