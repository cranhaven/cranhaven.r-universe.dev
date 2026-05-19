/**
 * nabla_dress_impl.c — shared ∇^k-DRESS implementation.
 *
 * Parameterised by a fit-function pointer so the same permutation-
 * enumeration and histogram logic drives both the CPU and CUDA backends.
 *
 * Key difference from Δ^k-DRESS:
 *   - Enumerates P(N,k) ordered k-tuples instead of C(N,k) combinations.
 *   - Marks tuple vertices with distinct generic vertex weights instead of
 *     deleting them.
 *   - The graph topology is unchanged; only vertex weights differ per tuple.
 *
 * Features:
 *   - Hash-map histogram (exact double keys, no epsilon binning)
 *   - Optional random sampling of tuples via permutation pick-filter
 *   - Default generic weights: sqrt(prime(i+1)) for algebraic independence
 */

#include "nabla_dress_impl.h"
#include "delta_dress_impl.h"   /* for dress_rand_r */
#include "dress_histogram.h"
#include "dress/dress.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

/* ------------------------------------------------------------------ */
/*  Generic marker weights                                             */
/* ------------------------------------------------------------------ */

/* First 16 primes — more than enough for practical k values. */
static const int PRIMES[] = {
    2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53
};
#define MAX_DEFAULT_K 16

/* Fill `out` with k default generic weights: sqrt(prime(i+1)). */
static void nabla_default_weights(double *out, int k)
{
    for (int i = 0; i < k && i < MAX_DEFAULT_K; i++)
        out[i] = sqrt((double)PRIMES[i]);
    /* For k > MAX_DEFAULT_K, extend with sqrt(prime) approximation. */
    for (int i = MAX_DEFAULT_K; i < k; i++)
        out[i] = sqrt((double)(2 * i + 3));
}

/* ------------------------------------------------------------------ */
/*  Internal helpers                                                   */
/* ------------------------------------------------------------------ */

/**
 * Build a marked copy of the graph for a given ordered k-tuple.
 *
 * The graph topology is identical to `g`.  vertex weights are set to
 * marker_weights[i] for tuple[i], and 1.0 for all other vertexs.
 *
 * Returns a freshly allocated dress graph.  The caller must free it
 * with dress_free_graph().
 */
static p_dress_graph_t nabla_build_marked_graph(p_dress_graph_t g,
                                                 const int *tuple, int k,
                                                 const double *marker_weights)
{
    int N = g->N;
    int E = g->E;

    /* Allocate edge arrays (dress_init_graph takes ownership). */
    int *sub_U = (int *)malloc(E * sizeof(int));
    int *sub_V = (int *)malloc(E * sizeof(int));
    memcpy(sub_U, g->U, E * sizeof(int));
    memcpy(sub_V, g->V, E * sizeof(int));

    /* Copy edge weights if present. */
    double *sub_W = NULL;
    if (g->W != NULL) {
        sub_W = (double *)malloc(E * sizeof(double));
        memcpy(sub_W, g->W, E * sizeof(double));
    }

    /* Build vertex weights: 1.0 everywhere, marker at tuple positions. */
    double *sub_NW = (double *)malloc(N * sizeof(double));
    for (int v = 0; v < N; v++)
        sub_NW[v] = 1.0;
    for (int i = 0; i < k; i++)
        sub_NW[tuple[i]] = marker_weights[i];

    return dress_init_graph(N, E, sub_U, sub_V,
                            sub_W, sub_NW, g->variant,
                            g->precompute_intercepts);
}

/* Accumulate converged edge dress values into hash-map histogram. */
static void nabla_accumulate_histogram(p_dress_graph_t sub,
                                       dress_histogram_t *hist)
{
    for (int e = 0; e < sub->E; e++) {
        dress_hist_add(hist, sub->edge_dress[e]);
    }
}

/* Fill one row of the multisets matrix. */
static void nabla_fill_multiset_row(p_dress_graph_t sub, int E,
                                    double *row)
{
    for (int e = 0; e < E; e++)
        row[e] = sub->edge_dress[e];
}

/* P(n, k) = n! / (n-k)! */
int64_t dress_nabla_perm_count(int n, int k)
{
    if (k < 0 || k > n) return 0;
    int64_t r = 1;
    for (int i = 0; i < k; i++)
        r *= (n - i);
    return r;
}

/* ── Sampling helpers ─────────────────────────────────────────────── */


/* 64-bit random from two rand_r calls. */
static int64_t nabla_rand64(unsigned int *seed, int64_t bound)
{
    int64_t r = ((int64_t)dress_rand_r(seed) << 31) | (int64_t)dress_rand_r(seed);
    return (r & 0x7FFFFFFFFFFFFFFFLL) % bound;
}

/* qsort comparator for int64_t. */
static int nabla_cmp_i64(const void *a, const void *b)
{
    int64_t x = *(const int64_t *)a;
    int64_t y = *(const int64_t *)b;
    return (x > y) - (x < y);
}

/* ------------------------------------------------------------------ */
/*  Shared ∇^k-DRESS implementation                                    */
/* ------------------------------------------------------------------ */

int64_t *dress_nabla_fit_impl_flat(p_dress_graph_t g, int k,
                                    int iterations, double epsilon,
                                    int n_samples, unsigned int seed,
                                    int *hist_size,
                                    int keep_multisets,
                                    double **multisets,
                                    int64_t *num_tuples,
                                    dress_fit_fn fit_fn,
                                    int offset, int stride)
{
    int N = g->N;
    int E = g->E;
    int do_hist = (hist_size != NULL);

    /* Hash-map histogram (exact double keys) — only when requested. */
    dress_histogram_t hist;
    if (do_hist) dress_hist_init(&hist, (size_t)E);

    int64_t pnk = (k == 0) ? 1 : dress_nabla_perm_count(N, k);
    if (num_tuples) *num_tuples = pnk;

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

    /* ── k = 0: ∇^0 — run DRESS on the unmarked graph ──────────── */
    if (k == 0) {
        if (offset != 0) {
            int64_t *flat = do_hist ? dress_hist_flatten(&hist, hist_size) : NULL;
            if (do_hist) dress_hist_free(&hist);
            return flat;
        }

        fit_fn(g, iterations, epsilon, NULL, NULL);
        if (do_hist) nabla_accumulate_histogram(g, &hist);

        if (wants_ms) {
            for (int e = 0; e < E; e++)
                ms[e] = g->edge_dress[e];
        }
        int64_t *flat = do_hist ? dress_hist_flatten(&hist, hist_size) : NULL;
        if (do_hist) dress_hist_free(&hist);
        return flat;
    }

    /* ── k >= N: no valid tuples ────────────────────────────────── */
    if (k >= N) {
        int64_t *flat = do_hist ? dress_hist_flatten(&hist, hist_size) : NULL;
        if (do_hist) dress_hist_free(&hist);
        return flat;
    }

    /* ── Compute generic marker weights: sqrt(prime(i+1)) ──────── */
    double *weights = (double *)malloc(k * sizeof(double));
    nabla_default_weights(weights, k);

    /* ── Sampling setup ─────────────────────────────────────────── */
    int64_t pnk_per_stride = (pnk + stride - 1 - offset) / stride;
    int64_t my_limit = (n_samples == 0)
        ? pnk_per_stride
        : (n_samples < pnk_per_stride ? n_samples : pnk_per_stride);

    int64_t *picks = NULL;
    int n_picks = 0;
    int use_sampling = (n_samples > 0 && my_limit < pnk_per_stride);
    if (use_sampling) {
        picks = (int64_t *)malloc((size_t)my_limit * sizeof(int64_t));
        unsigned int my_seed = seed + (unsigned int)offset;
        for (int64_t i = 0; i < my_limit; i++)
            picks[i] = nabla_rand64(&my_seed, pnk_per_stride);
        qsort(picks, (size_t)my_limit, sizeof(int64_t), nabla_cmp_i64);
        /* dedupe in-place */
        int64_t u = 0;
        for (int64_t i = 0; i < my_limit; i++)
            if (i == 0 || picks[i] != picks[i - 1])
                picks[u++] = picks[i];
        n_picks = (int)u;
    }

    /* ── k >= 1: iterative enumeration of P(N,k) ordered tuples ── */

    int *tuple = (int *)malloc(k * sizeof(int));
    int64_t my_s = 0;    /* count within this stride */
    int pick_idx = 0;

    /* Iterative DFS over ordered k-tuples (without repetition).
     * At each depth, we iterate over all vertices not yet used. */
    int *used = (int *)calloc(N, sizeof(int));
    int depth = 0;
    tuple[0] = -1;
    int64_t s = 0;

    while (depth >= 0) {
        /* Advance to next candidate at current depth. */
        tuple[depth]++;

        /* Un-use the previous vertex at this depth (if any). */
        if (tuple[depth] > 0 && tuple[depth] <= N) {
            int prev = tuple[depth] - 1;
            if (prev < N && used[prev]) used[prev] = 0;
        }

        /* Skip used vertices. */
        while (tuple[depth] < N && used[tuple[depth]])
            tuple[depth]++;

        if (tuple[depth] >= N) {
            /* Backtrack. */
            depth--;
            if (depth >= 0) {
                /* Un-mark the vertex we're about to leave. */
                used[tuple[depth]] = 0;
            }
            continue;
        }

        /* Mark current vertex as used. */
        used[tuple[depth]] = 1;

        if (depth == k - 1) {
            /* Complete tuple found. */
            if (s % stride == offset) {
                int process = !use_sampling
                    || (pick_idx < n_picks && my_s == picks[pick_idx]);
                pick_idx += (process & use_sampling);

                if (process) {
                    p_dress_graph_t marked = nabla_build_marked_graph(
                        g, tuple, k, weights);
                    fit_fn(marked, iterations, epsilon, NULL, NULL);
                    if (do_hist) nabla_accumulate_histogram(marked, &hist);
                    if (wants_ms)
                        nabla_fill_multiset_row(marked, E,
                                                ms + s * E);
                    dress_free_graph(marked);
                }
                my_s++;

                if (use_sampling && pick_idx >= n_picks)
                    break;
            }
            s++;

            /* Un-mark and stay at same depth (will advance). */
            used[tuple[depth]] = 0;
        } else {
            /* Descend to next depth. */
            depth++;
            tuple[depth] = -1;
        }
    }

    free(picks);
    free(tuple);
    free(used);
    free(weights);

    int64_t *flat = do_hist ? dress_hist_flatten(&hist, hist_size) : NULL;
    if (do_hist) dress_hist_free(&hist);
    return flat;
}
