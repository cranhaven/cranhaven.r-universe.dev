/**
 * dress_cuda.h — GPU-accelerated dress_fit() for libdress.
 *
 * Drop-in replacement for the CPU dress_fit():
 *
 *   #include "dress/dress.h"
 *   #include "dress/cuda/dress_cuda.h"
 *
 *   p_dress_graph_t g = dress_init_graph(N, E, U, V, W, variant, precompute);
 *   dress_fit_cuda(g, max_iterations, epsilon, &iterations, &delta);
 *   // g->edge_dress and g->vertex_dress are now populated
 *   dress_free_graph(g);
 *
 * Graph construction (dress_init_graph) stays on the CPU.
 * Only the iterative fitting loop runs on the GPU.
 */

#ifndef DRESS_CUDA_H
#define DRESS_CUDA_H

#include "dress/dress.h"
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * GPU-accelerated iterative DRESS fitting.
 *
 * Same signature and semantics as the CPU dress_fit() in dress.h.
 * Uploads arrays to GPU, runs the iteration loop with CUDA kernels,
 * downloads edge_dress and vertex_dress back to the host.
 *
 * Supports all four variants (UNDIRECTED, DIRECTED, FORWARD, BACKWARD)
 * and both intercept / non-intercept code paths.
 */
void dress_fit_cuda(p_dress_graph_t g, int max_iterations, double epsilon,
                   int *iterations, double *delta);

/**
 * GPU-accelerated Δ^k-DRESS histogram computation.
 *
 * Same interface as the CPU dress_delta_fit() in dress.h, but each
 * subgraph is fitted on the GPU via dress_fit_cuda().
 */
dress_hist_pair_t *dress_delta_fit_cuda(p_dress_graph_t g, int k, int iterations,
                                        double epsilon,
                                        int n_samples, unsigned int seed,
                                        int *hist_size,
                                        int keep_multisets, double **multisets,
                                        int64_t *num_subgraphs);

/**
 * GPU-accelerated strided Δ^k-DRESS for distributed computation.
 *
 * Same as dress_delta_fit_cuda but processes only subgraphs where
 * index % stride == offset.  With offset=0, stride=1 processes all.
 */
dress_hist_pair_t *dress_delta_fit_cuda_strided(p_dress_graph_t g, int k,
                                                int iterations, double epsilon,
                                                int n_samples, unsigned int seed,
                                                int *hist_size,
                                                int keep_multisets, double **multisets,
                                                int64_t *num_subgraphs,
                                                int offset, int stride);

/**
 * GPU-accelerated strided Δ^k-DRESS returning flat packed histogram.
 * Used internally by MPI reduction (returns int64_t* packed array).
 */
int64_t *dress_delta_fit_cuda_strided_flat(p_dress_graph_t g, int k,
                                           int iterations, double epsilon,
                                           int n_samples, unsigned int seed,
                                           int *hist_size,
                                           int keep_multisets, double **multisets,
                                           int64_t *num_subgraphs,
                                           int offset, int stride);

/* ── ∇^k-DRESS (Nabla) CUDA variants ───────────────────────────── */

dress_hist_pair_t *dress_nabla_fit_cuda(p_dress_graph_t g, int k, int iterations,
                                        double epsilon,
                                        int n_samples, unsigned int seed,
                                        int *hist_size,
                                        int keep_multisets, double **multisets,
                                        int64_t *num_tuples);

dress_hist_pair_t *dress_nabla_fit_cuda_strided(p_dress_graph_t g, int k,
                                                int iterations, double epsilon,
                                                int n_samples, unsigned int seed,
                                                int *hist_size,
                                                int keep_multisets, double **multisets,
                                                int64_t *num_tuples,
                                                int offset, int stride);

int64_t *dress_nabla_fit_cuda_flat(p_dress_graph_t g, int k, int iterations,
                                   double epsilon,
                                   int n_samples, unsigned int seed,
                                   int *hist_size,
                                   int keep_multisets, double **multisets,
                                   int64_t *num_tuples);

int64_t *dress_nabla_fit_cuda_strided_flat(p_dress_graph_t g, int k,
                                           int iterations, double epsilon,
                                           int n_samples, unsigned int seed,
                                           int *hist_size,
                                           int keep_multisets, double **multisets,
                                           int64_t *num_tuples,
                                           int offset, int stride);

#ifdef __cplusplus
}
#endif

#endif /* DRESS_CUDA_H */
