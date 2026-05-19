/**
 * delta_dress_cuda.c — GPU-accelerated Δ^k-DRESS histogram.
 *
 * Delegates to the shared delta_dress_impl.h, passing dress_fit_cuda
 * as the fitting function.
 */

#ifdef DRESS_CUDA

#include "dress/cuda/dress_cuda.h"
#include "delta_dress_impl.h"
#include "dress_histogram.h"

#include <stdlib.h>

static dress_hist_pair_t *delta_cuda_wrap_flat(int64_t *flat, int flat_len,
                                               int *hist_size)
{
    dress_hist_pair_t *pairs = dress_hist_flat_to_pairs(flat, flat_len, hist_size);
    free(flat);
    return pairs;
}

int64_t *dress_delta_fit_cuda_flat(p_dress_graph_t g, int k, int iterations,
                                   double epsilon,
                                   int n_samples, unsigned int seed,
                                   int *hist_size,
                                   int keep_multisets, double **multisets,
                                   int64_t *num_subgraphs)
{
    return dress_delta_fit_impl_flat(g, k, iterations, epsilon,
                                     n_samples, seed,
                                     hist_size,
                                     keep_multisets, multisets, num_subgraphs,
                                     dress_fit_cuda, 0, 1);
}

int64_t *dress_delta_fit_cuda_strided_flat(p_dress_graph_t g, int k,
                                           int iterations, double epsilon,
                                           int n_samples, unsigned int seed,
                                           int *hist_size,
                                           int keep_multisets, double **multisets,
                                           int64_t *num_subgraphs,
                                           int offset, int stride)
{
    return dress_delta_fit_impl_flat(g, k, iterations, epsilon,
                                     n_samples, seed,
                                     hist_size,
                                     keep_multisets, multisets, num_subgraphs,
                                     dress_fit_cuda, offset, stride);
}

dress_hist_pair_t *dress_delta_fit_cuda(p_dress_graph_t g, int k, int iterations,
                                        double epsilon,
                                        int n_samples, unsigned int seed,
                                        int *hist_size,
                                        int keep_multisets, double **multisets,
                                        int64_t *num_subgraphs)
{
    int flat_len = 0;
    int64_t *flat = dress_delta_fit_cuda_flat(g, k, iterations, epsilon,
                                              n_samples, seed,
                                              hist_size ? &flat_len : NULL,
                                              keep_multisets, multisets,
                                              num_subgraphs);
    return delta_cuda_wrap_flat(flat, flat_len, hist_size);
}

dress_hist_pair_t *dress_delta_fit_cuda_strided(p_dress_graph_t g, int k,
                                                int iterations, double epsilon,
                                                int n_samples, unsigned int seed,
                                                int *hist_size,
                                                int keep_multisets, double **multisets,
                                                int64_t *num_subgraphs,
                                                int offset, int stride)
{
    int flat_len = 0;
    int64_t *flat = dress_delta_fit_cuda_strided_flat(g, k, iterations, epsilon,
                                                      n_samples, seed,
                                                      hist_size ? &flat_len : NULL,
                                                      keep_multisets, multisets,
                                                      num_subgraphs,
                                                      offset, stride);
    return delta_cuda_wrap_flat(flat, flat_len, hist_size);
}

#endif /* DRESS_CUDA */

/* Prevent -Wempty-translation-unit when CUDA is unavailable. */
typedef int delta_dress_cuda_unused_t;
