#include "myomp.h"
#include <R.h>
#include <Rmath.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Lapack.h>
#include <R_ext/Boolean.h>

/**
 * @brief Get the max number of CPU cores
 * @param n Pointer to the number of CPU cores 
 */
void getNCores(int *n) {
  #if __VOPENMP
    *n = omp_get_num_procs();
  #else
    *n = 1;
  #endif
}

/**
 * @brief Get the number of threads to use
 * @param n Pointer to the number of threads
 */
void getNThreads(int *n) {
  #if __VOPENMP
    #pragma omp parallel default(shared)
    {
      #pragma omp master
        *n = omp_get_num_threads();
    }
  #else
    *n = 0;
  #endif
}

/**
 * @brief Set the number of threads to use
 * @param n Pointer to the number of threads
 */
void setNThreads(int *n) {
  #if __VOPENMP
    if (omp_get_num_procs() < *n) {
      *n = omp_get_num_procs();
      omp_set_num_threads(*n);
    }
    else if(*n > 0) {
      omp_set_num_threads(*n);
    }
    else {
      omp_set_num_threads(1);
      *n = 1;
    }
  #else
    *n = 1;
  #endif
}

SEXP isOmp(void) {
  SEXP ans;
  PROTECT(ans = allocVector(LGLSXP, 1));
  #if __VOPENMP
    LOGICAL(ans)[0] = TRUE;
  #else
    LOGICAL(ans)[0] = FALSE;
  #endif
  UNPROTECT(1);
  return ans;
}
SEXP openMP_version(void) {
  SEXP ans;
  PROTECT(ans = allocVector(REALSXP, 1));
  #if __VOPENMP
    REAL(ans)[0] = (double) _OPENMP * 1e-2;
  #else
    REAL(ans)[0] = NA_REAL;
  #endif
  UNPROTECT(1);
  return ans;
}


void R_init_HRTnomaly(DllInfo *info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
