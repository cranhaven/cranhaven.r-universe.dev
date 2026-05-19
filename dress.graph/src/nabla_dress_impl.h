/**
 * nabla_dress_impl.h — internal declarations for the shared ∇^k-DRESS
 * implementation.
 *
 * The actual code lives in nabla_dress_impl.c.  This header is included
 * by nabla_dress.c and cuda/nabla_dress_cuda.c so they can call
 * dress_nabla_fit_impl_flat() with different fit-function pointers.
 *
 * ∇^k-DRESS (Nabla-DRESS) enumerates P(N,k) ordered k-tuples of
 * vertices, marks each tuple with generic injective vertex weights,
 * runs DRESS on the resulting weighted graph, and pools the
 * fingerprints.  This is the individualization variant of DRESS,
 * analogous to WL with distinguished constants.
 */

#ifndef NABLA_DRESS_IMPL_H
#define NABLA_DRESS_IMPL_H

#include "dress/dress.h"
#include <stdint.h>

/* Fit-function signature: same as dress_fit / dress_fit_cuda. */
typedef void (*dress_fit_fn)(p_dress_graph_t, int, double, int *, double *);

/**
 * Core ∇^k-DRESS: enumerate P(N,k) ordered k-tuples, mark each
 * tuple with distinct generic vertex weights, fit DRESS on the
 * marked graph, and accumulate the pooled histogram.
 *
 * Returns a flat packed array [val_bits, count, val_bits, count, ...].
 * hist_size is set to the total length (2 * distinct_count).
 *
 * offset/stride control which tuples to process:
 *   offset=0, stride=1  →  all tuples (default, non-MPI)
 *   offset=rank, stride=nprocs  →  round-robin MPI distribution
 *
 * n_samples controls random sampling of tuples:
 *   n_samples=0  →  enumerate all P(N,k) tuples (full)
 *   n_samples>0  →  randomly sample n_samples tuples from this stride
 *   seed         →  random seed for reproducibility
 *
 */
int64_t *dress_nabla_fit_impl_flat(p_dress_graph_t g, int k,
                                    int iterations, double epsilon,
                                    int n_samples, unsigned int seed,
                                    int *hist_size,
                                    int keep_multisets,
                                    double **multisets,
                                    int64_t *num_tuples,
                                    dress_fit_fn fit_fn,
                                    int offset, int stride);

/**
 * P(n, k) = n! / (n-k)!  — number of ordered k-tuples.
 * Exposed so that OMP/MPI wrappers can compute the count.
 */
int64_t dress_nabla_perm_count(int n, int k);

#endif /* NABLA_DRESS_IMPL_H */
