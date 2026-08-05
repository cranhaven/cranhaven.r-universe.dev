#ifndef GIBBS_BETA_NUM_H
#define GIBBS_BETA_NUM_H

void gibbs_beta_num(struct str_state* last,   // OUT+IN last known values of generated parameters
                    struct str_param* param,  // IN hyperparameters
                    int* Id,                  // [N] IDs
                    double* Y,                // IN [*N * totnY]
                    double* X,                // [N*(#regr)]    ID + regressors
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
                    int* G,                   // IN [1] number of classes
                    int* N,                   // IN [1] number of observations
                    int* n,                   // IN [1] number of subjects
                    int* nY,                  // IN [1]        counts of Nums variables
                    double* predictor_num,    // IN [N*2*nY]  both F and R parts of predictor for numeric outcomes 
                    int* FormulaF,    // [sum(nfix)]  numbers of columns of X that should be used for FIXED  effects of modelled responses
                    int* FormulaG,    // [sum(ngrp)]  numbers of columns of X that should be used for GROUP-SPECIFIC  effects of modelled responses
                    int* FormulaR,    // [sum(nran)]  numbers of columns of X that should be used for RANDOM effects of modelled responses
                    int* FormulaO,    // [sum(noff)]  numbers of columns of X that should be used for OFFSET effects of modelled responses
                    int* nfix,        // [sum(nY)]  number of FIXED  regressors for each response
                    int* ngrp,        // [sum(nY)]  number of GROUP-SPECIFIC  regressors for each response
                    int* nran,        // [sum(nY)]  number of RANDOM regressors for each response
                    int* noff,        // [sum(nY)]  number of OFFSET regressors for each response
                    int* cumnfix,     // []         cummulative sums of nfix values of all together (for Formula)
                    int* cumngrp,     // []         cummulative sums of ngrp values of all together (for Formula)
                    int* cumnran,     // []         cummulative sums of nran values of all together (for Formula)
                    int* cumnoff,     // []         cummulative sums of noff values of all together (for Formula)
                    int* totnran,     // [1]        total dimension of one single b_i
                    int* n_i,                 // IN [n] number of observations dedicated to j-th subject
                    int* i_j,                 // IN [n * max_n_j] indeces dedicated to j-th subject 
                    int* nUg,                 // IN [G] number of subjects     in classes 1, ..., G
                    int* listUi               // IN [n*G] list of who belongs to which cluster
);   


#endif
