#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

/* Declare all C functions used in .C() calls */
void random_permutation(int *perm_length, int *x);
void marginals(int *length_h, int *dist_id, int *h, double *theta, double *res);
void expectation(int *dist_id, int *model, int *perm_length, double *theta, double *expec);
void probability(int *dist_id, int *length_perm, int *perm, int *sigma0, double *theta, double *res);
void save_counts_to_files(int *perm_length);
void compute_distance(int *dist_id, int *perm_length, int *perm1, int *perm2, int *dist);
void count_permus_at_dist(int *dist_id, int *perm_length, int *dist_value, double *count);
void get_altern_repre_for_permu(int *dist_id, int *perm_length, int *perm, int *vec);
void get_permu_given_altern_repre(int *dist_id, int *perm_length, int *vec, int *sigma);

/* Declare all .Call() functions */
SEXP distances_sampling(SEXP dist_id, SEXP perm_length, SEXP num_perms, SEXP theta);
SEXP sampling_multi_gibbs_cayley(SEXP dist_id, SEXP perm_length, SEXP num_perms, SEXP theta, SEXP model, SEXP algorithm_id);
SEXP consensus(SEXP dist_id, SEXP data, SEXP mm_var, SEXP estim_var, SEXP sigma_0_ini);
SEXP estimate_theta(SEXP dist_id, SEXP perm_length, SEXP num_perms, SEXP sigma0, SEXP data, SEXP estim_var);
SEXP get_random_sample_at_dist_d(SEXP dist_id, SEXP perm_length, SEXP num_perms, SEXP dist_value);

/* Define .C() routines */
static const R_CMethodDef CEntries[] = {
  {"random_permutation", (DL_FUNC) &random_permutation, 2},
  {"marginals", (DL_FUNC) &marginals, 5},
  {"expectation", (DL_FUNC) &expectation, 5},
  {"probability", (DL_FUNC) &probability, 6},
  {"save_counts_to_files", (DL_FUNC) &save_counts_to_files, 1},
  {"compute_distance", (DL_FUNC) &compute_distance, 5},
  {"count_permus_at_dist", (DL_FUNC) &count_permus_at_dist, 4},
  {"get_altern_repre_for_permu", (DL_FUNC) &get_altern_repre_for_permu, 4},
  {"get_permu_given_altern_repre", (DL_FUNC) &get_permu_given_altern_repre, 4},
  {NULL, NULL, 0}
};

/* Define .Call() routines */
static const R_CallMethodDef CallEntries[] = {
  {"distances_sampling", (DL_FUNC) &distances_sampling, 4},
  {"sampling_multi_gibbs_cayley", (DL_FUNC) &sampling_multi_gibbs_cayley, 6},
  {"consensus", (DL_FUNC) &consensus, 5},
  {"estimate_theta", (DL_FUNC) &estimate_theta, 6},
  {"get_random_sample_at_dist_d", (DL_FUNC) &get_random_sample_at_dist_d, 4},
  {NULL, NULL, 0}
};

/* Register routines */
void R_init_PerMallows(DllInfo *dll) {
  R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
