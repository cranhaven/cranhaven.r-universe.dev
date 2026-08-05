#ifndef PREDICTOR_H
#define PREDICTOR_H

void calculate_predictor_separately( 
        /** IN parameters **/
        int* Id,        // [N]          IDs
        double* X,      // [N*#regr]    ID + regressors
        /** Output **/
        double* predictor,      //   OUT       [*N * *nY] current value of predictor
        int* update,      // [4] 0/1 - update Fixed, Group-specific, Random, Offset part
        /** Parameters describing dimensions **/
        int* N,           // [1]        total number of observations
        int* nY,          // [1]        total number of responses
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
        int* beta_dim,    // [1]        dimension of single group-specific beta parameter
        int* K,           // [nY]        number of categories (K=1 if Num,Poi,Bin,Ord, but K=Kcat for Cats)
        int* kspec_bi_cat,// [1]        are random effects k-specific
        //** Arrays with necessary parameters from one state **/
        double* beta_fix,     // [totnfix ]       Beta parameters for fixed effects
        double* beta,         // [totngrp * G]    Beta parameters for group-specific effects
        double* b,            // [totnran * n]    Random effects
        double* U                // [n]              What is the current class of i-th subject
);

#endif
