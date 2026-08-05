#ifndef METRGIBBS_BETA_POI_H
#define METRGIBBS_BETA_POI_H

void metrgibbs_beta_poi(struct str_state* last,   // OUT+IN last known values of generated parameters
                        struct str_param* param,  // IN hyperparameters
                        int* Id,                  // [N] IDs
                        double* Y,                // IN [*N * (1 + totnY)]
                        double* X,                // [N*(1 + #regr)]    ID + regressors
                        int* spec,                // [5]                class-specific parameters
                        // order:   [0] tau_num, 
                        //          [1] c_ord,
                        //          [2] InvSigma, 
                        //          [3] InvQ, 
                        //          [4] naY
                        int* dims,                // [33]:              what parameters are saved
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
                        int* N,                 // IN [1] number of observations
                        int* n,                 // IN [1] number of subjects
                        int* nY,                // IN [5] counts of outcomes
                        int* G,                 // IN [1] number of classes
                        int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
                        int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
                        int* nfix,              // IN [1] number of FIXED  regressors or each response
                        int* cumnfix,           // IN [1] where to start in FormulaF
                        int* FormulaF,          // IN [nfix]  numbers of columns of X that should be used for FIXED  effects of modelled responses
                        int* ngrp,              // IN [1] number of GROUP-SPECIFIC  regressors or each response
                        int* cumngrp,           // IN [1] where to start in FormulaG
                        int* FormulaG,          // IN [ngrp]  numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
                        int* nran,              // IN [1] number of RANDOM  regressors or each response
                        int* cumnran,           // IN [1] where to start in FormulaR
                        int* totnran,           // IN [1] total dimension of RANDOM parameter (useless)
                        int* FormulaR,          // IN [nran]  numbers of columns of X that should be used for RANDOM  effects of modelled responses
                        int* noff,              // IN [1] number of OFFSET regressors or each response
                        int* cumnoff,           // IN [1] where to start in FormulaO
                        int* FormulaO,          // IN [noff]  numbers of columns of X that should be used for OFFSET  effects of modelled responses
                        int* n_i,               // IN [n] number of observations dedicated to j-th subject
                        int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
                        int* nUg,               // IN [G] number of subjects in classes 1, ..., G
                        int* listUi,            // IN [n*G] row by row indices of subjects in group g
                        // Proposal distribution parameters
                        double* prop_beta_poi_fix,      // OUT [] place for prop. distribution mean
                        double* prop_chol_beta_poi_fix, // OUT [] cholesky decomposition of proposal distributions precision matrices
                        int* cumdim_prop_beta_poi_fix,  // IN [nY[1]+1] cummulative dimensions of chol matrices
                        int* totdim_prop_beta_poi_fix,  // IN [1] total dimension of chol matrices (per one cluster)
                        double* prop_beta_poi,          // OUT [] place for prop. distribution mean
                        double* prop_chol_beta_poi,     // OUT [] cholesky decomposition of proposal distributions precision matrices
                        int* cumdim_prop_beta_poi,      // IN [nY[1]+1] cummulative dimensions of chol matrices
                        int* totdim_prop_beta_poi,      // IN [1] total dimension of chol matrices (per one cluster)
                        int* m,                         // IN [1] number of the step (to decide whether to update proposal distribution)
                        // Tuning parameters
                        struct str_tuning* tuning       // tuning parameters
);
  
#endif
