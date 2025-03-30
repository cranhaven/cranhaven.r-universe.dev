//
// Created by andrew on 11/21/2023.
//
#include "detect_blas.h"
#include <dlfcn.h>
#ifdef _OPENMP
#include <omp.h>
#endif

openblas_handle_t get_openblas_handle(void) {
    const openblas_handle_t blas_handle = dlopen(0, RTLD_NOW);
    return blas_handle;
}

openblas_init_t get_openblas_parallel(const openblas_handle_t libloc) {
    const openblas_init_t parallel_address = dlsym(libloc, "openblas_get_parallel");
    return parallel_address;
}

openblas_set_t get_openblas_set(const openblas_handle_t libloc) {
    const openblas_set_t set_address = dlsym(libloc, "openblas_set_num_threads");
    return set_address;
}

bool is_openmp(void) {
#ifdef _OPENMP
    const int threads = omp_get_num_threads();
    if (threads == 1) return false;
    return true;
#endif
    return false;
}
