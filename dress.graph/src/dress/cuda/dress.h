/**
 * dress/cuda/dress.h — GPU-accelerated DRESS via include-based switching.
 *
 * Drop-in replacement for dress/dress.h.
 * Including this header redirects dress_fit() and dress_delta_fit()
 * to their CUDA implementations — no source changes required:
 *
 *   // CPU
 *   #include "dress/dress.h"
 *   dress_fit(g, 100, 1e-6, &iters, &delta);
 *
 *   // CUDA — same call, different include
 *   #include "dress/cuda/dress.h"
 *   dress_fit(g, 100, 1e-6, &iters, &delta);
 *
 * Do not include both this header and dress/dress.h in the same
 * translation unit — the macros will conflict.
 */

#ifndef DRESS_CUDA_REDIRECT_H
#define DRESS_CUDA_REDIRECT_H

#include "dress/dress.h"
#include "dress/cuda/dress_cuda.h"

/* Redirect CPU symbols to CUDA implementations. */
#define dress_fit       dress_fit_cuda
#define dress_delta_fit dress_delta_fit_cuda
#define dress_delta_fit_strided dress_delta_fit_cuda_strided
#define dress_nabla_fit dress_nabla_fit_cuda
#define dress_nabla_fit_strided dress_nabla_fit_cuda_strided

#endif /* DRESS_CUDA_REDIRECT_H */
