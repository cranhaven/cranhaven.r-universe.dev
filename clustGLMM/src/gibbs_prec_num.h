#ifndef GIBBS_TAU_NUM_H
#define GIBBS_TAU_NUM_H

void gibbs_prec_num(struct str_state* last,  // OUT+IN last known values of generated parameters
                    struct str_param* param, // IN hyperparameters
                    double* Y,            // IN [*N * totnY]
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
                    double* predictor_num, // IN [N * 2 * nY[0]]  both R and F parts of predictor of numeric outcomes 
                    int* G,           // IN [1] number of classes
                    int* N,           // IN [1] number of observations
                    int* n,           // IN [1] number of subjects
                    int* nY,          // IN [1]        counts of Nums outcomes
                    int* ngrp,        // IN [sum(nY)]  number of FIXED  regressors for each response
                    int* cumngrp,     // IN [totnY] cummulative number of fixed  regressors
                    int* n_i,         // IN [n] number of observations dedicated to j-th subject
                    int* i_j,         // IN [n * max_n_j] indeces dedicated to j-th subject
                    int* ng,          // IN [G] number of observations in classes 1, ..., G
                    int* nUg,         // IN [G] number of subjects in classes 1, ..., G
                    int* listUi       // IN [n*G] row by row indices of subjects in group g
);

#endif
