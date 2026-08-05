#ifndef STRUCTURES_H
#define STRUCTURES_H

struct str_tuning {
  int* freq_proposal_update;            // [1]    how frequently change proposal distributions
  int* times_proposal;                  // [1]    how many times propose new state within one Metropolis step 
  double* const_proposal_beta_poi_fix;  // [1]    constant by which the proposal direction is multiplied (const_proposal^2 to variance)
  double* const_proposal_beta_poi;      // [1]    constant by which the proposal direction is multiplied (const_proposal^2 to variance)
  double* const_proposal_beta_bin_fix;  // [1]    constant by which the proposal direction is multiplied (const_proposal^2 to variance)
  double* const_proposal_beta_bin;      // [1]    constant by which the proposal direction is multiplied (const_proposal^2 to variance)
  double* const_proposal_beta_ord_fix;  // [1]    constant by which the proposal direction is multiplied (const_proposal^2 to variance)
  double* const_proposal_beta_ord;      // [1]    constant by which the proposal direction is multiplied (const_proposal^2 to variance)
  double* const_proposal_beta_cat_fix;  // [1]    constant by which the proposal direction is multiplied (const_proposal^2 to variance)
  double* const_proposal_beta_cat;      // [1]    constant by which the proposal direction is multiplied (const_proposal^2 to variance)
  double* const_proposal_a_ord;         // [1]    constant by which the proposal direction is multiplied (const_proposal^2 to variance)
  double* const_proposal_b;             // [1]    constant by which the proposal direction is multiplied (const_proposal^2 to variance)
  double* const_proposal_e0;            // [1]    constant by which the proposal direction is multiplied (const_proposal^2 to variance)
  double* tolerance;                    // [1]    tolerance when computing norm of the shift within Newton-Raphson
  int* maxiter;                         // [1]    maximum iterations allowed during Newton-Raphson update of proposal distribution
  int* maxnrep;                         // [1]    maximum number of repetitions of Newton-Raphson with a different start location
  int* kspec_bi_cat;                    // [1]    TRUE = each cat var has different set of bi for each of K-1 levels
                                        //        FALSE = all levels of cat var have one set of bi - compares last category with others
};

struct str_param {
  double* nu_0;               // [1]    prior df for InvSigma
  double* nu_1;               // [1]    prior df for InvQ  
  double* gamma_a;            // [1]    prior shape of tau
  double* gamma_b;            // [1]    prior rate  of tau
  double* sd_beta_num_fix;    // [1]    prior sd for beta parameters
  double* sd_beta_num;        // [1]    prior sd for beta parameters
  double* sd_beta_poi_fix;    // [1]    prior sd for beta parameters
  double* sd_beta_poi;        // [1]    prior sd for beta parameters
  double* sd_beta_bin_fix;    // [1]    prior sd for beta parameters
  double* sd_beta_bin;        // [1]    prior sd for beta parameters
  double* sd_beta_ord_fix;    // [1]    prior sd for beta parameters
  double* sd_beta_ord;        // [1]    prior sd for beta parameters
  double* api_prior;          // [1]    prior alpha parameter for Dirichlet distribution of pi - same for all categories
  double* sd_beta_cat_fix;    // [1]    prior sd for beta parameters
  double* sd_beta_cat;        // [1]    prior sd for beta parameters
  double* ae;       // [1]    prior shape of e0
  double* be;       // [1]    prior rate of e0
  double* InvV;     // [totnran*(totnran+1)/2] prior inverse scale matrix for InvQ
};

struct str_state {
  double* w;            // [G]                        Class probabilities
  double* e0;           // [1]                        Precision parameter for prior of w
  double* U;            // [n]                        To which class does i-th subject belong to
  double* pUig;         // [n * G]                    Probability of i-th subject belong to each class
  double* prec_num;     // [nY[0](* G)]               Precision parameter of numerical variables
  double* beta_num_fix; // [sum(nfix[Nums])]          Beta parameters for fixed effects of Nums
  double* beta_num;     // [sum(ngrp[Nums]) *G]       Beta parameters for group-specific effects of Nums
  double* beta_poi_fix; // [sum(nfix[Pois])]          Beta parameters for fixed effects of Pois
  double* beta_poi;     // [sum(ngrp[Pois]) *G]       Beta parameters for group-specific effects of Pois
  double* beta_bin_fix; // [sum(nfix[Bins])]          Beta parameters for fixed effects of Bins
  double* beta_bin;     // [sum(ngrp[Bins]) *G]       Beta parameters for group-specific effects of Bins
  double* beta_ord_fix; // [sum(nfix[Ords])]          Beta parameters for fixed effects of Ords
  double* beta_ord;     // [sum(ngrp[Ords]) *G]       Beta parameters for group-specific effects of Ords
  double* c_ord;        // [sum(Kord)(*G)             Ordered intercepts of Ords
  double* a_ord;        // [sum(Kord)(*G)             Non-constrained transformed intercepts of Ords
  double* pi_ord;       // [sum(Kord)(*G)             P(Y = k | b=0, U, x=0) = logit_inv(c_k) - logit_inv(c_{k-1})
  double* beta_cat_fix; // [sum(nfix[Cats]*Kcat)]     Beta parameters for fixed effects of Cats
  double* beta_cat;     // [sum(ngrp[Cats]*Kcat) *G]  Beta parameters for group-specific effects of Cats
  double* InvSigma;     // [totnran*(totnran+1)/2(*G)] Precision matrix of random effects
  double* InvQ;         // [totnran*(totnran+1)/2(*G)] Prior inverse scale matrix of InvSigma
  double* b;            // [totnran * n]              Random effects
  double* naY;          // [sum(isYna)(*G)]           Imputed missing outcome values
  double* detInvSigma;  // [1 (*G)]                   determinant of InvSigma (calculable parameter)
};

#endif
