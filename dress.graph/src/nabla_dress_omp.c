/*
 * nabla_dress_omp.c — OpenMP-parallel ∇^k-DRESS.
 *
 * Parallelises the outer P(N,k) tuple loop: each thread processes
 * a strided slice of tuples, fitting each one sequentially via
 * dress_fit() (no nested OpenMP).  Per-thread histograms are merged
 * after the parallel region.
 *
 * Build: compile with OpenMP enabled (-fopenmp / /openmp).
 */

#include "dress/dress.h"
#include "dress/omp/dress_omp.h"
#include "nabla_dress_impl.h"
#include "dress_histogram.h"

#ifdef _OPENMP
#include <omp.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <math.h>

/* ── helpers ────────────────────────────────────────────────────── */

static dress_hist_pair_t *nabla_omp_wrap_flat(int64_t *flat, int flat_len,
                                              int *hist_size)
{
    dress_hist_pair_t *pairs = dress_hist_flat_to_pairs(flat, flat_len, hist_size);
    free(flat);
    return pairs;
}

static void nabla_merge_flat_into_hist(dress_histogram_t *dst,
                                 const int64_t *flat, int flat_len)
{
    for (int i = 0; i + 1 < flat_len; i += 2) {
        double val;
        memcpy(&val, &flat[i], sizeof(double));
        dress_hist_add_count(dst, val, flat[i + 1]);
    }
}

/* ── core parallel implementation ───────────────────────────────── */

static int64_t *dress_nabla_fit_omp_flat_impl(
    p_dress_graph_t g, int k, int iterations,
    double epsilon,
    int n_samples, unsigned int seed,
    int *hist_size,
    int keep_multisets, double **multisets,
    int64_t *num_tuples,
    int outer_offset, int outer_stride)
{
    int N = g->N;
    int E = g->E;

#ifdef _OPENMP
    int nthreads = omp_get_max_threads();
#else
    int nthreads = 1;
#endif

    int64_t pnk = (k == 0) ? 1 : dress_nabla_perm_count(N, k);
    if (num_tuples) *num_tuples = pnk;

    /* For k=0 or single thread, fall through to sequential impl. */
    if (nthreads <= 1 || k < 1) {
        return dress_nabla_fit_impl_flat(g, k, iterations, epsilon,
                                         n_samples, seed,
                                         hist_size,
                                         keep_multisets, multisets, num_tuples,
                                         dress_fit, outer_offset, outer_stride);
    }

    int do_hist = (hist_size != NULL);
    int wants_ms = keep_multisets && multisets;
    double *ms = NULL;
    if (wants_ms) {
        size_t total = (size_t)pnk * E;
        ms = (double *)malloc(total * sizeof(double));
        *multisets = ms;
        if (!ms) {
            wants_ms = 0;
        } else {
            for (size_t i = 0; i < total; i++) ms[i] = NAN;
        }
    }

    /* Per-thread flat histograms. */
    int64_t **thread_flats = NULL;
    int *thread_flat_lens = NULL;
    if (do_hist) {
        thread_flats = (int64_t **)calloc(nthreads, sizeof(int64_t *));
        thread_flat_lens = (int *)calloc(nthreads, sizeof(int));
    }

#ifdef _OPENMP
    #pragma omp parallel
#endif
    {
#ifdef _OPENMP
        int tid = omp_get_thread_num();
        int nt  = omp_get_num_threads();
#else
        int tid = 0;
        int nt  = 1;
#endif
        int eff_offset = outer_offset + tid * outer_stride;
        int eff_str    = outer_stride * nt;

        int t_hist_size = 0;
        double *t_ms = NULL;

        int64_t *flat = dress_nabla_fit_impl_flat(
            g, k, iterations, epsilon,
            n_samples, seed,
            do_hist ? &t_hist_size : NULL,
            wants_ms ? 1 : 0,
            wants_ms ? &t_ms : NULL,
            NULL,
            dress_fit,
            eff_offset, eff_str);

        if (do_hist && thread_flats) {
            thread_flats[tid] = flat;
            thread_flat_lens[tid] = t_hist_size;
        }

        /* Scatter per-thread multiset rows into the shared matrix. */
        if (wants_ms && t_ms && ms) {
            for (int64_t s = eff_offset; s < pnk; s += eff_str) {
                memcpy(ms + s * E, t_ms + s * E, E * sizeof(double));
            }
            free(t_ms);
        }
    }

    /* Merge per-thread histograms. */
    int64_t *result = NULL;
    if (do_hist) {
        dress_histogram_t merged;
        dress_hist_init(&merged, (size_t)E);

        for (int t = 0; t < nthreads; t++) {
            if (thread_flats[t]) {
                nabla_merge_flat_into_hist(&merged, thread_flats[t], thread_flat_lens[t]);
                free(thread_flats[t]);
            }
        }
        free(thread_flats);
        free(thread_flat_lens);

        int len = 0;
        result = dress_hist_flatten(&merged, &len);
        dress_hist_free(&merged);
        if (hist_size) *hist_size = len;
    }

    return result;
}

/* ── public API ─────────────────────────────────────────────────── */

dress_hist_pair_t *dress_nabla_fit_omp(p_dress_graph_t g, int k,
                                       int iterations, double epsilon,
                                       int n_samples, unsigned int seed,
                                       int *hist_size,
                                       int keep_multisets, double **multisets,
                                       int64_t *num_tuples)
{
    int flat_len = 0;
    int64_t *flat = dress_nabla_fit_omp_flat_impl(
        g, k, iterations, epsilon,
        n_samples, seed,
        hist_size ? &flat_len : NULL,
        keep_multisets, multisets, num_tuples,
        0, 1);
    return nabla_omp_wrap_flat(flat, flat_len, hist_size);
}

dress_hist_pair_t *dress_nabla_fit_omp_strided(p_dress_graph_t g, int k,
                                               int iterations, double epsilon,
                                               int n_samples, unsigned int seed,
                                               int *hist_size,
                                               int keep_multisets,
                                               double **multisets,
                                               int64_t *num_tuples,
                                               int offset, int stride)
{
    int flat_len = 0;
    int64_t *flat = dress_nabla_fit_omp_flat_impl(
        g, k, iterations, epsilon,
        n_samples, seed,
        hist_size ? &flat_len : NULL,
        keep_multisets, multisets, num_tuples,
        offset, stride);
    return nabla_omp_wrap_flat(flat, flat_len, hist_size);
}

int64_t *dress_nabla_fit_omp_strided_flat(p_dress_graph_t g, int k,
                                          int iterations, double epsilon,
                                          int n_samples, unsigned int seed,
                                          int *hist_size,
                                          int keep_multisets,
                                          double **multisets,
                                          int64_t *num_tuples,
                                          int offset, int stride)
{
    return dress_nabla_fit_omp_flat_impl(
        g, k, iterations, epsilon,
        n_samples, seed,
        hist_size,
        keep_multisets, multisets, num_tuples,
        offset, stride);
}
