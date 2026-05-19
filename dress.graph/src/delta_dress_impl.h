/**
 * delta_dress_impl.h — internal declarations for the shared Δ^k-DRESS
 * implementation.
 *
 * The actual code lives in delta_dress_impl.c.  This header is included
 * by delta_dress.c and cuda/delta_dress_cuda.c so they can call
 * dress_delta_fit_impl_flat() with different fit-function pointers.
 */

#ifndef DELTA_DRESS_IMPL_H
#define DELTA_DRESS_IMPL_H

#include "dress/dress.h"
#include <stdint.h>

/* ── Portable thread-safe RNG ────────────────────────────────────
 * Replaces POSIX rand_r to avoid CRAN NOTE about system RNGs and
 * to work on Windows MSVC.  Defined in the header (static inline)
 * so both delta and nabla impl files share a single definition
 * even when compiled into the same translation unit (Go CGo). */
static inline int dress_rand_r(unsigned int *seed)
{
    *seed = *seed * 1103515245u + 12345u;
    return (int)((*seed >> 16) & 0x7fff);
}

/* Fit-function signature: same as dress_fit / dress_fit_cuda. */
typedef void (*dress_fit_fn)(p_dress_graph_t, int, double, int *, double *);

/**
 * Core Δ^k-DRESS: enumerate C(N,k) deletion subsets, fit each subgraph
 * using `fit_fn`, and accumulate the pooled histogram.
 *
 * Returns a flat packed array [val_bits, count, val_bits, count, ...].
 * hist_size is set to the total length (2 * distinct_count).
 *
 * offset/stride control which subgraphs to process:
 *   offset=0, stride=1  →  all subgraphs (default, non-MPI)
 *   offset=rank, stride=nprocs  →  round-robin MPI distribution
 *
 * n_samples controls random sampling of deletion subsets:
 *   n_samples=0  →  enumerate all C(N,k) subsets (full)
 *   n_samples>0  →  randomly sample n_samples subsets from this stride
 *   seed         →  random seed for reproducibility
 */
int64_t *dress_delta_fit_impl_flat(p_dress_graph_t g, int k,
                                    int iterations, double epsilon,
                                    int n_samples, unsigned int seed,
                                    int *hist_size,
                                    int keep_multisets,
                                    double **multisets,
                                    int64_t *num_subgraphs,
                                    dress_fit_fn fit_fn,
                                    int offset, int stride);

/**
 * Public binomial coefficient C(n, k).
 * Exposed so that OMP/MPI wrappers can compute cnk without duplicating.
 */
int64_t dress_delta_binom(int n, int k);

#endif /* DELTA_DRESS_IMPL_H */
