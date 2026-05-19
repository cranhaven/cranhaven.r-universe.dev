/**
 * dress_omp.h — OpenMP-parallel DRESS fitting for libdress.
 *
 * Drop-in replacement for the sequential dress_fit():
 *
 *   #include "dress/dress.h"
 *   #include "dress/omp/dress_omp.h"
 *
 *   p_dress_graph_t g = dress_init_graph(N, E, U, V, W, NW, variant, precompute);
 *   dress_fit_omp(g, max_iterations, epsilon, &iterations, &delta);
 *   // g->edge_dress and g->vertex_dress are now populated
 *   dress_free_graph(g);
 *
 * dress_fit_omp() parallelises the per-edge and per-vertex loops within
 * each iteration using OpenMP.  Suitable for large single graphs.
 *
 * dress_delta_fit_omp() parallelises the outer C(N,k) subgraph loop:
 * each thread processes a strided slice of subgraphs, fitting each one
 * sequentially.  Suitable for Δ^k-DRESS on many small subgraphs.
 */

#ifndef DRESS_OMP_H
#define DRESS_OMP_H

#include "dress/dress.h"
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * OpenMP-parallel iterative DRESS fitting.
 *
 * Same signature and semantics as the sequential dress_fit() in dress.h.
 * Parallelises the per-vertex and per-edge loops within each iteration.
 */
void dress_fit_omp(p_dress_graph_t g, int max_iterations, double epsilon,
                   int *iterations, double *delta);

/**
 * OpenMP-parallel Δ^k-DRESS.
 *
 * Same interface as the sequential dress_delta_fit() in dress.h.
 * Parallelises the outer subgraph loop: each thread processes a
 * strided slice of C(N,k) subgraphs, fitting each one sequentially.
 */
dress_hist_pair_t *dress_delta_fit_omp(p_dress_graph_t g, int k,
                                       int iterations, double epsilon,
                                       int n_samples, unsigned int seed,
                                       int *hist_size,
                                       int keep_multisets, double **multisets,
                                       int64_t *num_subgraphs);

/**
 * OpenMP-parallel strided Δ^k-DRESS for distributed computation.
 *
 * Like dress_delta_fit_omp but restricted to subgraphs where
 * index % stride == offset.  Useful for MPI+OMP hybrid.
 */
dress_hist_pair_t *dress_delta_fit_omp_strided(p_dress_graph_t g, int k,
                                               int iterations, double epsilon,
                                               int n_samples, unsigned int seed,
                                               int *hist_size,
                                               int keep_multisets,
                                               double **multisets,
                                               int64_t *num_subgraphs,
                                               int offset, int stride);

/**
 * Flat variants — return packed [bits, count, ...] int64_t arrays
 * instead of dress_hist_pair_t.  Used by MPI+OMP for Allgatherv.
 */
int64_t *dress_delta_fit_omp_strided_flat(p_dress_graph_t g, int k,
                                          int iterations, double epsilon,
                                          int n_samples, unsigned int seed,
                                          int *hist_size,
                                          int keep_multisets,
                                          double **multisets,
                                          int64_t *num_subgraphs,
                                          int offset, int stride);

/* ── ∇^k-DRESS (Nabla) OMP variants ────────────────────────────── */

dress_hist_pair_t *dress_nabla_fit_omp(p_dress_graph_t g, int k,
                                       int iterations, double epsilon,
                                       int n_samples, unsigned int seed,
                                       int *hist_size,
                                       int keep_multisets, double **multisets,
                                       int64_t *num_tuples);

dress_hist_pair_t *dress_nabla_fit_omp_strided(p_dress_graph_t g, int k,
                                               int iterations, double epsilon,
                                               int n_samples, unsigned int seed,
                                               int *hist_size,
                                               int keep_multisets,
                                               double **multisets,
                                               int64_t *num_tuples,
                                               int offset, int stride);

int64_t *dress_nabla_fit_omp_strided_flat(p_dress_graph_t g, int k,
                                          int iterations, double epsilon,
                                          int n_samples, unsigned int seed,
                                          int *hist_size,
                                          int keep_multisets,
                                          double **multisets,
                                          int64_t *num_tuples,
                                          int offset, int stride);

#ifdef __cplusplus
}
#endif

#endif /* DRESS_OMP_H */
