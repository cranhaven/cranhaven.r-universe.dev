/**
 * dress_mpi.h — MPI-distributed Δ^k-DRESS for libdress.
 *
 * Distributes the C(N,k) subgraph enumeration across MPI ranks using
 * the strided API: each rank processes subgraphs where
 * index % nprocs == rank, then the per-rank histograms are gathered and
 * merged by key.  All MPI logic lives in C — language bindings are
 * thin FFI calls.
 *
 * CPU usage:
 *
 *   #include "dress/dress.h"
 *   #include "dress/mpi/dress_mpi.h"
 *
 *   MPI_Init(&argc, &argv);
 *   p_dress_graph_t g = dress_init_graph(N, E, U, V, NULL, 0, 1);
 *   dress_hist_pair_t *hist = dress_delta_fit_mpi(g, 2, 100, 1e-6,
 *                                                 &hist_size, 0, NULL,
 *                                                 &num_sub, MPI_COMM_WORLD);
 *   free(hist);
 *   dress_free_graph(g);
 *   MPI_Finalize();
 *
 * GPU + MPI:
 *
 *   #include "dress/cuda/dress_cuda.h"
 *   #include "dress/mpi/dress_mpi.h"
 *
 *   dress_delta_fit_mpi_cuda(g, 2, 100, 1e-6,
 *                            &hist_size, 0, NULL,
 *                            &num_sub, MPI_COMM_WORLD);
 *
 * FFI callers (Python, Rust, Julia, R) use the _fcomm variants which
 * accept a Fortran MPI communicator handle (int) instead of MPI_Comm:
 *
 *   int comm_f = MPI_Comm_c2f(MPI_COMM_WORLD);
 *   dress_delta_fit_mpi_fcomm(g, ..., comm_f);
 */

#ifndef DRESS_MPI_H
#define DRESS_MPI_H

#include "dress/dress.h"
#include <mpi.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ── CPU + MPI ──────────────────────────────────────────────────── */

/**
 * MPI-distributed Δ^k-DRESS (CPU backend).
 *
 * Same parameters as dress_delta_fit plus an MPI communicator.
 * Each rank computes a local histogram for its stride of subgraphs.
 * The returned histogram is the global sum (identical on all ranks).
 * Caller must free() the returned pointer.
 *
 * When keep_multisets is non-zero, *multisets is set to a heap-allocated
 * C(N,k)*E matrix (same layout as dress_delta_fit).  Each rank computes
 * its own rows, then MPI_Allreduce(SUM) merges them (non-owned rows are
 * zero-initialised).  Caller must free(*multisets).
 */
dress_hist_pair_t *dress_delta_fit_mpi(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    MPI_Comm comm);

dress_hist_pair_t *dress_delta_fit_mpi_fcomm(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    int comm_f);

/* ── CUDA + MPI ─────────────────────────────────────────────────── */

#if defined(DRESS_CUDA) || defined(DRESS_CUDA_H)

dress_hist_pair_t *dress_delta_fit_mpi_cuda(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    MPI_Comm comm);

dress_hist_pair_t *dress_delta_fit_mpi_cuda_fcomm(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    int comm_f);

#endif /* DRESS_CUDA || DRESS_CUDA_H */

/* ── OMP + MPI ──────────────────────────────────────────────────── */

#if defined(_OPENMP) || defined(DRESS_OMP_H)

dress_hist_pair_t *dress_delta_fit_mpi_omp(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    MPI_Comm comm);

dress_hist_pair_t *dress_delta_fit_mpi_omp_fcomm(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    int comm_f);

dress_hist_pair_t *dress_delta_fit_mpi_omp_world(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs);

#endif /* _OPENMP || DRESS_OMP_H */

/* ── FFI helpers — no MPI package required in wrappers ──────────── */

void dress_mpi_init(void);
void dress_mpi_finalize(void);
int dress_mpi_rank(void);
int dress_mpi_size(void);

dress_hist_pair_t *dress_delta_fit_mpi_world(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs);

#if defined(DRESS_CUDA) || defined(DRESS_CUDA_H)
dress_hist_pair_t *dress_delta_fit_mpi_cuda_world(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs);
#endif /* DRESS_CUDA || DRESS_CUDA_H */

/* ── ∇^k-DRESS (Nabla) MPI variants ────────────────────────────── */

dress_hist_pair_t *dress_nabla_fit_mpi(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    MPI_Comm comm);

dress_hist_pair_t *dress_nabla_fit_mpi_fcomm(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    int comm_f);

#if defined(DRESS_CUDA) || defined(DRESS_CUDA_H)

dress_hist_pair_t *dress_nabla_fit_mpi_cuda(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    MPI_Comm comm);

dress_hist_pair_t *dress_nabla_fit_mpi_cuda_fcomm(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    int comm_f);

#endif /* DRESS_CUDA || DRESS_CUDA_H */

#if defined(_OPENMP) || defined(DRESS_OMP_H)

dress_hist_pair_t *dress_nabla_fit_mpi_omp(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    MPI_Comm comm);

dress_hist_pair_t *dress_nabla_fit_mpi_omp_fcomm(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    int comm_f);

dress_hist_pair_t *dress_nabla_fit_mpi_omp_world(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples);

#endif /* _OPENMP || DRESS_OMP_H */

dress_hist_pair_t *dress_nabla_fit_mpi_world(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples);

#if defined(DRESS_CUDA) || defined(DRESS_CUDA_H)
dress_hist_pair_t *dress_nabla_fit_mpi_cuda_world(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples);
#endif /* DRESS_CUDA || DRESS_CUDA_H */

#ifdef __cplusplus
}
#endif

#endif /* DRESS_MPI_H */
