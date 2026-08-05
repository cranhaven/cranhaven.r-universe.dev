#ifndef PDFS_DERIVATIVES_BI_H
#define PDFS_DERIVATIVES_BI_H

void logpYi(struct str_state* last,  // IN last known values of generated parameters
            double* Y,                // IN [*N]
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
            int* nY,                // IN [5] counts of outcomes
            int* N,                 // IN [1] number of observations
            int* n,                 // IN [1] number of subjects
            int* i,                 // IN [1] index of a subject
            int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
            int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
            int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
            //        FALSE = all levels of cat var have one set of bi - compares last category with others
            int* nran,              // IN [1] number of RANDOM  regressors or each response
            int* cumnran,           // IN [1] where to start in FormulaR
            int* totnran,           // IN [1] total dimension of fixed parameter (useless)
            int* FormulaR,          // IN [totnran]  numbers of columns of X that should be used for RANDOM  effects of modelled responses
            int* n_i,               // IN [n] number of observations dedicated to j-th subject
            int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
            int* nUg,               // IN [G] number of subjects in classes 1, ..., G
            int* listUi,            // IN [n*G] row by row indices of subjects in group g
            double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
            double* value           // OUT [1]
);

void logpbi(struct str_state* last,  // IN last known values of generated parameters
            double* hyperparameter,   // not needed pointer to potential hyperparameter
            double* Y,                // IN [*N]
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
            int* nY,                // IN [5] counts of outcomes
            int* N,                 // IN [1] number of observations
            int* n,                 // IN [1] number of subjects
            int* i,                 // IN [1] index of a subject
            int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
            int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
            int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
            //        FALSE = all levels of cat var have one set of bi - compares last category with others
            int* nran,              // IN [1] number of RANDOM  regressors or each response
            int* cumnran,           // IN [1] where to start in FormulaR
            int* totnran,           // IN [1] total dimension of fixed parameter (useless)
            int* FormulaR,          // IN [totnran]  numbers of columns of X that should be used for RANDOM  effects of modelled responses
            int* n_i,               // IN [n] number of observations dedicated to j-th subject
            int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
            int* nUg,               // IN [G] number of subjects in classes 1, ..., G
            int* listUi,            // IN [n*G] row by row indices of subjects in group g
            double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
            double* value           // OUT [1]
);

void d_d2_logpbi(struct str_state* last,  // IN last known values of generated parameters
                 double* hyperparameter,   // not needed pointer to potential hyperparameter
                 double* Y,                // IN [*N]
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
                 int* nY,                // IN [5] counts of outcomes
                 int* N,                 // IN [1] number of observations
                 int* n,                 // IN [1] number of subjects
                 int* i,                 // IN [1] index of a subject
                 int* Kord,              // IN [1] total number of categories of Ord outcome -1 (useless)
                 int* Kcat,              // IN [1] total number of categories of Cat outcome -1 (useless)
                 int* kspec_bi_cat,      // IN [1] TRUE = each cat var has different set of bi for each of K-1 levels
                 //        FALSE = all levels of cat var have one set of bi - compares last category with others
                 int* nran,              // IN [1] number of RANDOM  regressors or each response
                 int* cumnran,           // IN [1] where to start in FormulaR
                 int* totnran,           // IN [1] total dimension of fixed parameter (useless)
                 int* FormulaR,          // IN [totnran]  numbers of columns of X that should be used for RANDOM  effects of modelled responses
                 int* n_i,               // IN [n] number of observations dedicated to j-th subject
                 int* i_j,               // IN [n * max_n_j] indeces dedicated to j-th subject 
                 int* nUg,               // IN [G] number of subjects in classes 1, ..., G
                 int* listUi,            // IN [n*G] row by row indices of subjects in group g
                 double* theta,          // IN [dims[.]] the parameter in which the function is evaluated
                 double* grad,           // OUT [totnran]
                 double* hess            // OUT [totnran*(totnran+1)/2]
);
#endif
