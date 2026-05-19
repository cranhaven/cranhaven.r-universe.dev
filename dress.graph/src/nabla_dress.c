/**
 * nabla_dress.c — CPU ∇^k-DRESS public API wrappers.
 *
 * Thin wrappers around dress_nabla_fit_impl_flat() using dress_fit
 * as the fit function.
 */

#include "dress/dress.h"
#include "nabla_dress_impl.h"
#include "dress_histogram.h"

#include <stdlib.h>

/* Convert flat [bits, count, ...] to public histogram pairs. */
static dress_hist_pair_t *nabla_wrap_flat(int64_t *flat, int flat_len,
                                          int *hist_size)
{
    dress_hist_pair_t *pairs = dress_hist_flat_to_pairs(flat, flat_len, hist_size);
    free(flat);
    return pairs;
}

/* ── Flat variants ─────────────────────────────────────────────────── */

int64_t *dress_nabla_fit_flat(p_dress_graph_t g, int k, int iterations,
                              double epsilon,
                              int n_samples, unsigned int seed,
                              int *hist_size,
                              int keep_multisets, double **multisets,
                              int64_t *num_tuples)
{
    return dress_nabla_fit_impl_flat(g, k, iterations, epsilon,
                                     n_samples, seed,
                                     hist_size,
                                     keep_multisets, multisets, num_tuples,
                                     dress_fit, 0, 1);
}

int64_t *dress_nabla_fit_strided_flat(p_dress_graph_t g, int k,
                                      int iterations, double epsilon,
                                      int n_samples, unsigned int seed,
                                      int *hist_size,
                                      int keep_multisets, double **multisets,
                                      int64_t *num_tuples,
                                      int offset, int stride)
{
    return dress_nabla_fit_impl_flat(g, k, iterations, epsilon,
                                     n_samples, seed,
                                     hist_size,
                                     keep_multisets, multisets, num_tuples,
                                     dress_fit, offset, stride);
}

/* ── Public pair-based variant ─────────────────────────────────────── */

dress_hist_pair_t *dress_nabla_fit(p_dress_graph_t g, int k, int iterations,
                                   double epsilon,
                                   int n_samples, unsigned int seed,
                                   int *hist_size,
                                   int keep_multisets, double **multisets,
                                   int64_t *num_tuples)
{
    int flat_len = 0;
    int64_t *flat = dress_nabla_fit_flat(g, k, iterations, epsilon,
                                          n_samples, seed,
                                          &flat_len,
                                          keep_multisets, multisets, num_tuples);
    return nabla_wrap_flat(flat, flat_len, hist_size);
}
