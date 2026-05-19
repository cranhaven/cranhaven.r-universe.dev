/*
 * dress_mpi.c — MPI-distributed Δ^k-DRESS implementation.
 *
 * Provides linkable (non-inline) versions of the MPI distribution
 * functions so every language binding can call them through FFI.
 *
 * Build: compile with -DDRESS_MPI  to enable this file.
 *        compile with -DDRESS_CUDA to include the GPU+MPI variants.
 *        Link against libmpi (or equivalent).
 */

#ifdef DRESS_MPI

#include "dress/dress.h"
#include "delta_dress_impl.h"
#include "dress_histogram.h"
#include <mpi.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* NaN-aware MPI reduce: pick the non-NaN value (or NaN if both are NaN).
 * Used instead of MPI_SUM for multiset merging so that NaN-initialized
 * unsampled rows survive the Allreduce without corrupting data. */
static void dress_nan_merge(void *invec, void *inoutvec, int *len,
                            MPI_Datatype *dtype) {
    (void)dtype;
    double *in    = (double *)invec;
    double *inout = (double *)inoutvec;
    for (int i = 0; i < *len; i++) {
        if (!isnan(in[i]))
            inout[i] = in[i];
        /* else: keep inout[i] (either data or NaN) */
    }
}

#ifdef DRESS_CUDA
#include "dress/cuda/dress_cuda.h"
#endif

/* ── internal: strided fit + Allreduce ──────────────────────────── */

static dress_hist_pair_t *_mpi_reduce(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    MPI_Comm comm,
    int use_cuda)
{
    int rank, nprocs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    double *local_ms = NULL;
    int local_nflat = 0;
    int64_t *local_hist;

#ifdef DRESS_CUDA
    if (use_cuda) {
        local_hist = dress_delta_fit_cuda_strided_flat(
            g, k, iterations, epsilon,
            n_samples, seed,
            &local_nflat,
            keep_multisets, keep_multisets ? &local_ms : NULL,
            num_subgraphs, rank, nprocs);
    } else
#endif
    {
        (void)use_cuda;
        local_hist = dress_delta_fit_strided_flat(
            g, k, iterations, epsilon,
            n_samples, seed,
            &local_nflat,
            keep_multisets, keep_multisets ? &local_ms : NULL,
            num_subgraphs, rank, nprocs);
    }

    int do_hist = (hist_size != NULL);

    int *all_counts = NULL;
    int *displs = NULL;
    int64_t *all_flat = NULL;
    dress_hist_pair_t *global_hist = NULL;

    if (do_hist) {
        all_counts = (int *)malloc((size_t)nprocs * sizeof(int));
        MPI_Allgather(&local_nflat, 1, MPI_INT, all_counts, 1, MPI_INT, comm);

        int total_flat = 0;
        displs = (int *)malloc((size_t)nprocs * sizeof(int));
        for (int i = 0; i < nprocs; i++) {
            displs[i] = total_flat;
            total_flat += all_counts[i];
        }

        if (total_flat > 0) {
            all_flat = (int64_t *)malloc((size_t)total_flat * sizeof(int64_t));
        }
        MPI_Allgatherv(local_hist, local_nflat, MPI_INT64_T,
                       all_flat, all_counts, displs, MPI_INT64_T, comm);

        dress_histogram_t merged_hist;
        
        dress_hist_init(&merged_hist, (size_t)g->E);
        for (int rank_idx = 0; rank_idx < nprocs; rank_idx++) {
            for (int pos = displs[rank_idx]; pos < displs[rank_idx] + all_counts[rank_idx]; pos += 2) {
                double value;
                memcpy(&value, &all_flat[pos], sizeof(double));
                dress_hist_add_count(&merged_hist, value, all_flat[pos + 1]);
            }
        }

        global_hist = dress_hist_pairs(&merged_hist, hist_size);
        dress_hist_free(&merged_hist);
    }

    if (keep_multisets && multisets && local_ms && num_subgraphs) {
        int E = g->E;
        int64_t cnk = *num_subgraphs;
        /* NaN-init so unsampled rows stay NaN after merge. */ 
        size_t _ms_len = (size_t)cnk * E; 
        double *global_ms = (double *)malloc(_ms_len * sizeof(double)); 
        for (size_t _i = 0; _i < _ms_len; _i++) global_ms[_i] = NAN;
        MPI_Op _nan_op;
        MPI_Op_create(dress_nan_merge, 1, &_nan_op);
        MPI_Allreduce(local_ms, global_ms, (int)(cnk * E),
                      MPI_DOUBLE, _nan_op, comm);
        MPI_Op_free(&_nan_op);
        free(local_ms);
        *multisets = global_ms;
    } else if (multisets) {
        *multisets = NULL;
    }

    free(local_hist);
    free(all_counts);
    free(displs);
    free(all_flat);
    return global_hist;
}

/* ── CPU + MPI ──────────────────────────────────────────────────── */

dress_hist_pair_t *dress_delta_fit_mpi(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    MPI_Comm comm)
{
    return _mpi_reduce(g, k, iterations, epsilon,
                       n_samples, seed,
                       hist_size,
                       keep_multisets, multisets, num_subgraphs, comm, 0);
}

dress_hist_pair_t *dress_delta_fit_mpi_fcomm(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    int comm_f)
{
    return dress_delta_fit_mpi(g, k, iterations, epsilon,
                               n_samples, seed,
                               hist_size,
                               keep_multisets, multisets, num_subgraphs,
                               MPI_Comm_f2c(comm_f));
}

/* ── CUDA + MPI ─────────────────────────────────────────────────── */

#ifdef DRESS_CUDA

dress_hist_pair_t *dress_delta_fit_mpi_cuda(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    MPI_Comm comm)
{
    return _mpi_reduce(g, k, iterations, epsilon,
                       n_samples, seed,
                       hist_size,
                       keep_multisets, multisets, num_subgraphs, comm, 1);
}

dress_hist_pair_t *dress_delta_fit_mpi_cuda_fcomm(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    int comm_f)
{
    return dress_delta_fit_mpi_cuda(g, k, iterations, epsilon,
                               n_samples, seed,
                               hist_size,
                               keep_multisets, multisets, num_subgraphs,
                               MPI_Comm_f2c(comm_f));
}

#endif /* DRESS_CUDA */

/* ── OMP + MPI ──────────────────────────────────────────────────── */

#ifdef _OPENMP

#include "dress/omp/dress_omp.h"

static dress_hist_pair_t *_mpi_reduce_omp(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    MPI_Comm comm)
{
    int rank, nprocs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    double *local_ms = NULL;
    int local_nflat = 0;

    /* Each rank uses OMP to parallelise its strided slice. */
    int64_t *local_hist = dress_delta_fit_omp_strided_flat(
        g, k, iterations, epsilon,
        n_samples, seed,
        &local_nflat,
        keep_multisets, keep_multisets ? &local_ms : NULL,
        num_subgraphs, rank, nprocs);

    int do_hist = (hist_size != NULL);

    int *all_counts = NULL;
    int *displs = NULL;
    int64_t *all_flat = NULL;
    dress_hist_pair_t *global_hist = NULL;

    if (do_hist) {
        /* Allgather flat histograms. */
        all_counts = (int *)malloc((size_t)nprocs * sizeof(int));
        MPI_Allgather(&local_nflat, 1, MPI_INT, all_counts, 1, MPI_INT, comm);

        int total_flat = 0;
        displs = (int *)malloc((size_t)nprocs * sizeof(int));
        for (int i = 0; i < nprocs; i++) {
            displs[i] = total_flat;
            total_flat += all_counts[i];
        }

        if (total_flat > 0)
            all_flat = (int64_t *)malloc((size_t)total_flat * sizeof(int64_t));
        MPI_Allgatherv(local_hist, local_nflat, MPI_INT64_T,
                       all_flat, all_counts, displs, MPI_INT64_T, comm);

        /* Merge per-rank histograms. */
        dress_histogram_t merged_hist;
        
        dress_hist_init(&merged_hist, (size_t)g->E);
        for (int ri = 0; ri < nprocs; ri++) {
            for (int pos = displs[ri]; pos < displs[ri] + all_counts[ri]; pos += 2) {
                double value;
                memcpy(&value, &all_flat[pos], sizeof(double));
                dress_hist_add_count(&merged_hist, value, all_flat[pos + 1]);
            }
        }

        global_hist = dress_hist_pairs(&merged_hist, hist_size);
        dress_hist_free(&merged_hist);
    }

    /* Allreduce multisets. */
    if (keep_multisets && multisets && local_ms && num_subgraphs) {
        int E = g->E;
        int64_t cnk = *num_subgraphs;
        /* NaN-init so unsampled rows stay NaN after merge. */ 
        size_t _ms_len = (size_t)cnk * E; 
        double *global_ms = (double *)malloc(_ms_len * sizeof(double)); 
        for (size_t _i = 0; _i < _ms_len; _i++) global_ms[_i] = NAN;
        MPI_Op _nan_op;
        MPI_Op_create(dress_nan_merge, 1, &_nan_op);
        MPI_Allreduce(local_ms, global_ms, (int)(cnk * E),
                      MPI_DOUBLE, _nan_op, comm);
        MPI_Op_free(&_nan_op);
        free(local_ms);
        *multisets = global_ms;
    } else if (multisets) {
        *multisets = NULL;
    }

    free(local_hist);
    free(all_counts);
    free(displs);
    free(all_flat);
    return global_hist;
}

dress_hist_pair_t *dress_delta_fit_mpi_omp(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    MPI_Comm comm)
{
    return _mpi_reduce_omp(g, k, iterations, epsilon,
                           n_samples, seed,
                           hist_size,
                           keep_multisets, multisets, num_subgraphs, comm);
}

dress_hist_pair_t *dress_delta_fit_mpi_omp_fcomm(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs,
    int comm_f)
{
    return dress_delta_fit_mpi_omp(g, k, iterations, epsilon,
                                   n_samples, seed,
                                   hist_size,
                                   keep_multisets, multisets, num_subgraphs,
                                   MPI_Comm_f2c(comm_f));
}

#endif /* _OPENMP */

/* ── FFI helpers — no MPI package needed in wrappers ────────────── */

void dress_mpi_init(void)
{
    int initialized;
    MPI_Initialized(&initialized);
    if (!initialized) MPI_Init(NULL, NULL);
}

void dress_mpi_finalize(void)
{
    int finalized;
    MPI_Finalized(&finalized);
    if (!finalized) MPI_Finalize();
}

int dress_mpi_rank(void)
{
    int rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    return rank;
}

int dress_mpi_size(void)
{
    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    return size;
}

/* COMM_WORLD convenience — CPU */
dress_hist_pair_t *dress_delta_fit_mpi_world(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs)
{
    return dress_delta_fit_mpi(g, k, iterations, epsilon,
                               n_samples, seed,
                               hist_size,
                               keep_multisets, multisets, num_subgraphs,
                               MPI_COMM_WORLD);
}

#ifdef DRESS_CUDA
/* COMM_WORLD convenience — CUDA */
dress_hist_pair_t *dress_delta_fit_mpi_cuda_world(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs)
{
    return dress_delta_fit_mpi_cuda(g, k, iterations, epsilon,
                               n_samples, seed,
                               hist_size,
                               keep_multisets, multisets, num_subgraphs,
                               MPI_COMM_WORLD);
}
#endif /* DRESS_CUDA */

#ifdef _OPENMP
/* COMM_WORLD convenience — OMP */
dress_hist_pair_t *dress_delta_fit_mpi_omp_world(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_subgraphs)
{
    return dress_delta_fit_mpi_omp(g, k, iterations, epsilon,
                               n_samples, seed,
                               hist_size,
                               keep_multisets, multisets, num_subgraphs,
                               MPI_COMM_WORLD);
}
#endif /* _OPENMP */

/* ================================================================== */
/*  ∇^k-DRESS (Nabla) MPI variants                                    */
/* ================================================================== */

#include "nabla_dress_impl.h"

/* ── internal: nabla strided fit + Allreduce ───────────────────── */

static dress_hist_pair_t *_nabla_mpi_reduce(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    MPI_Comm comm,
    int use_cuda)
{
    int rank, nprocs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    double *local_ms = NULL;
    int local_nflat = 0;
    int64_t *local_hist;

#ifdef DRESS_CUDA
    if (use_cuda) {
        local_hist = dress_nabla_fit_cuda_strided_flat(
            g, k, iterations, epsilon,
            n_samples, seed,
            &local_nflat,
            keep_multisets, keep_multisets ? &local_ms : NULL,
            num_tuples, rank, nprocs);
    } else
#endif
    {
        (void)use_cuda;
        local_hist = dress_nabla_fit_strided_flat(
            g, k, iterations, epsilon,
            n_samples, seed,
            &local_nflat,
            keep_multisets, keep_multisets ? &local_ms : NULL,
            num_tuples, rank, nprocs);
    }

    int do_hist = (hist_size != NULL);

    int *all_counts = NULL;
    int *displs = NULL;
    int64_t *all_flat = NULL;
    dress_hist_pair_t *global_hist = NULL;

    if (do_hist) {
        all_counts = (int *)malloc((size_t)nprocs * sizeof(int));
        MPI_Allgather(&local_nflat, 1, MPI_INT, all_counts, 1, MPI_INT, comm);

        int total_flat = 0;
        displs = (int *)malloc((size_t)nprocs * sizeof(int));
        for (int i = 0; i < nprocs; i++) {
            displs[i] = total_flat;
            total_flat += all_counts[i];
        }

        if (total_flat > 0)
            all_flat = (int64_t *)malloc((size_t)total_flat * sizeof(int64_t));
        MPI_Allgatherv(local_hist, local_nflat, MPI_INT64_T,
                       all_flat, all_counts, displs, MPI_INT64_T, comm);

        dress_histogram_t merged_hist;
        dress_hist_init(&merged_hist, (size_t)g->E);
        for (int ri = 0; ri < nprocs; ri++) {
            for (int pos = displs[ri]; pos < displs[ri] + all_counts[ri]; pos += 2) {
                double value;
                memcpy(&value, &all_flat[pos], sizeof(double));
                dress_hist_add_count(&merged_hist, value, all_flat[pos + 1]);
            }
        }

        global_hist = dress_hist_pairs(&merged_hist, hist_size);
        dress_hist_free(&merged_hist);
    }

    if (keep_multisets && multisets && local_ms && num_tuples) {
        int E = g->E;
        int64_t pnk = *num_tuples;
        /* NaN-init so unsampled rows stay NaN after merge. */ 
        size_t _ms_len = (size_t)pnk * E; 
        double *global_ms = (double *)malloc(_ms_len * sizeof(double)); 
        for (size_t _i = 0; _i < _ms_len; _i++) global_ms[_i] = NAN;
        MPI_Op _nan_op;
        MPI_Op_create(dress_nan_merge, 1, &_nan_op);
        MPI_Allreduce(local_ms, global_ms, (int)(pnk * E),
                      MPI_DOUBLE, _nan_op, comm);
        MPI_Op_free(&_nan_op);
        free(local_ms);
        *multisets = global_ms;
    } else if (multisets) {
        *multisets = NULL;
    }

    free(local_hist);
    free(all_counts);
    free(displs);
    free(all_flat);
    return global_hist;
}

/* ── CPU + MPI ──────────────────────────────────────────────────── */

dress_hist_pair_t *dress_nabla_fit_mpi(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    MPI_Comm comm)
{
    return _nabla_mpi_reduce(g, k, iterations, epsilon,
                             n_samples, seed,
                             hist_size,
                             keep_multisets, multisets, num_tuples, comm, 0);
}

dress_hist_pair_t *dress_nabla_fit_mpi_fcomm(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    int comm_f)
{
    return dress_nabla_fit_mpi(g, k, iterations, epsilon,
                               n_samples, seed,
                               hist_size,
                               keep_multisets, multisets, num_tuples,
                               MPI_Comm_f2c(comm_f));
}

/* ── CUDA + MPI ─────────────────────────────────────────────────── */

#ifdef DRESS_CUDA

dress_hist_pair_t *dress_nabla_fit_mpi_cuda(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    MPI_Comm comm)
{
    return _nabla_mpi_reduce(g, k, iterations, epsilon,
                             n_samples, seed,
                             hist_size,
                             keep_multisets, multisets, num_tuples, comm, 1);
}

dress_hist_pair_t *dress_nabla_fit_mpi_cuda_fcomm(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    int comm_f)
{
    return dress_nabla_fit_mpi_cuda(g, k, iterations, epsilon,
                               n_samples, seed,
                               hist_size,
                               keep_multisets, multisets, num_tuples,
                               MPI_Comm_f2c(comm_f));
}

#endif /* DRESS_CUDA */

/* ── OMP + MPI ──────────────────────────────────────────────────── */

#ifdef _OPENMP

static dress_hist_pair_t *_nabla_mpi_reduce_omp(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    MPI_Comm comm)
{
    int rank, nprocs;
    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    double *local_ms = NULL;
    int local_nflat = 0;

    int64_t *local_hist = dress_nabla_fit_omp_strided_flat(
        g, k, iterations, epsilon,
        n_samples, seed,
        &local_nflat,
        keep_multisets, keep_multisets ? &local_ms : NULL,
        num_tuples, rank, nprocs);

    int do_hist = (hist_size != NULL);

    int *all_counts = NULL;
    int *displs = NULL;
    int64_t *all_flat = NULL;
    dress_hist_pair_t *global_hist = NULL;

    if (do_hist) {
        all_counts = (int *)malloc((size_t)nprocs * sizeof(int));
        MPI_Allgather(&local_nflat, 1, MPI_INT, all_counts, 1, MPI_INT, comm);

        int total_flat = 0;
        displs = (int *)malloc((size_t)nprocs * sizeof(int));
        for (int i = 0; i < nprocs; i++) {
            displs[i] = total_flat;
            total_flat += all_counts[i];
        }

        if (total_flat > 0)
            all_flat = (int64_t *)malloc((size_t)total_flat * sizeof(int64_t));
        MPI_Allgatherv(local_hist, local_nflat, MPI_INT64_T,
                       all_flat, all_counts, displs, MPI_INT64_T, comm);

        dress_histogram_t merged_hist;
        dress_hist_init(&merged_hist, (size_t)g->E);
        for (int ri = 0; ri < nprocs; ri++) {
            for (int pos = displs[ri]; pos < displs[ri] + all_counts[ri]; pos += 2) {
                double value;
                memcpy(&value, &all_flat[pos], sizeof(double));
                dress_hist_add_count(&merged_hist, value, all_flat[pos + 1]);
            }
        }

        global_hist = dress_hist_pairs(&merged_hist, hist_size);
        dress_hist_free(&merged_hist);
    }

    if (keep_multisets && multisets && local_ms && num_tuples) {
        int E = g->E;
        int64_t pnk = *num_tuples;
        /* NaN-init so unsampled rows stay NaN after merge. */ 
        size_t _ms_len = (size_t)pnk * E; 
        double *global_ms = (double *)malloc(_ms_len * sizeof(double)); 
        for (size_t _i = 0; _i < _ms_len; _i++) global_ms[_i] = NAN;
        MPI_Op _nan_op;
        MPI_Op_create(dress_nan_merge, 1, &_nan_op);
        MPI_Allreduce(local_ms, global_ms, (int)(pnk * E),
                      MPI_DOUBLE, _nan_op, comm);
        MPI_Op_free(&_nan_op);
        free(local_ms);
        *multisets = global_ms;
    } else if (multisets) {
        *multisets = NULL;
    }

    free(local_hist);
    free(all_counts);
    free(displs);
    free(all_flat);
    return global_hist;
}

dress_hist_pair_t *dress_nabla_fit_mpi_omp(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    MPI_Comm comm)
{
    return _nabla_mpi_reduce_omp(g, k, iterations, epsilon,
                                 n_samples, seed,
                                 hist_size,
                                 keep_multisets, multisets, num_tuples, comm);
}

dress_hist_pair_t *dress_nabla_fit_mpi_omp_fcomm(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    int comm_f)
{
    return dress_nabla_fit_mpi_omp(g, k, iterations, epsilon,
                                   n_samples, seed,
                                   hist_size,
                                   keep_multisets, multisets, num_tuples,
                                   MPI_Comm_f2c(comm_f));
}

#endif /* _OPENMP */

/* ── COMM_WORLD convenience wrappers ────────────────────────────── */

dress_hist_pair_t *dress_nabla_fit_mpi_world(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples)
{
    return dress_nabla_fit_mpi(g, k, iterations, epsilon,
                               n_samples, seed,
                               hist_size,
                               keep_multisets, multisets, num_tuples,
                               MPI_COMM_WORLD);
}

#ifdef DRESS_CUDA
dress_hist_pair_t *dress_nabla_fit_mpi_cuda_world(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples)
{
    return dress_nabla_fit_mpi_cuda(g, k, iterations, epsilon,
                               n_samples, seed,
                               hist_size,
                               keep_multisets, multisets, num_tuples,
                               MPI_COMM_WORLD);
}
#endif /* DRESS_CUDA */

#ifdef _OPENMP
dress_hist_pair_t *dress_nabla_fit_mpi_omp_world(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples)
{
    return dress_nabla_fit_mpi_omp(g, k, iterations, epsilon,
                               n_samples, seed,
                               hist_size,
                               keep_multisets, multisets, num_tuples,
                               MPI_COMM_WORLD);
}
#endif /* _OPENMP */

#endif /* DRESS_MPI */

/* Prevent -Wempty-translation-unit when MPI is unavailable. */
typedef int dress_mpi_unused_t;
