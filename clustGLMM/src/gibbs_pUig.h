#ifndef GIBBS_PUIG_H
#define GIBBS_PUIG_H

void gibbs_pUig(struct str_state* last,  // IN last known values of generated parameters
                struct str_param* param, // IN hyperparameters
                int* Id,                  // [N] IDs
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
                int* G,                 // IN [1] total number of clusters
                int* Gp,                // OUT [1] number of nonempty clusters
                int* Gm,                // OUt [1] number of empty clusters
                int* N,                 // IN [1] number of observations
                int* n,                 // IN [1] number of subjects
                int* nY,                // IN [5]
                int* Kord,              // IN [nY[3]] total number of categories of Ord outcomes -1
                int* Kcat,              // IN [nY[4]] total number of categories of Cat outcomes -1
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
                int* n_i,               // IN [n] number of observations dedicated to i-th subject
                int* i_j,               // IN [n * max_n_j] indeces dedicated to i-th subject 
                int* ng,                // OUT [G] individual observation count in g-th cluster
                int* nUg,               // OUT [G] number of subjects in g-th cluster
                int* listUi,            // OUT [(G+1) * n] row by row indices of subjects in group g
                double* loglik,         // OUT [1] conditional-loglikelihood under new sampled U[i]
                // Tuning parameters
                int* lag_proposal_bi,     // OUT [*n] number of non-updated proposals for bi in a row, if 0 --> to be updated
                struct str_tuning* tuning // tuning parameters
);
#endif
