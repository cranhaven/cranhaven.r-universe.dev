/**
 * dress/omp/dress.h — OpenMP-parallel DRESS via include-based switching.
 *
 * Drop-in replacement for dress/dress.h.
 * Including this header redirects dress_fit() and dress_delta_fit()
 * to their OpenMP implementations — no source changes required:
 *
 *   // Sequential
 *   #include "dress/dress.h"
 *   dress_fit(g, 100, 1e-6, &iters, &delta);
 *
 *   // OpenMP — same call, different include
 *   #include "dress/omp/dress.h"
 *   dress_fit(g, 100, 1e-6, &iters, &delta);
 *
 * Do not include both this header and dress/dress.h in the same
 * translation unit — the macros will conflict.
 */

#ifndef DRESS_OMP_REDIRECT_H
#define DRESS_OMP_REDIRECT_H

#include "dress/dress.h"
#include "dress/omp/dress_omp.h"

/* Redirect sequential symbols to OpenMP implementations. */
#define dress_fit              dress_fit_omp
#define dress_delta_fit        dress_delta_fit_omp
#define dress_delta_fit_strided dress_delta_fit_omp_strided
#define dress_nabla_fit        dress_nabla_fit_omp
#define dress_nabla_fit_strided dress_nabla_fit_omp_strided

#endif /* DRESS_OMP_REDIRECT_H */
