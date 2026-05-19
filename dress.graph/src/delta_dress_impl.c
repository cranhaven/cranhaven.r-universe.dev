/**
 * delta_dress_impl.c — shared Δ^k-DRESS implementation.
 *
 * Parameterised by a fit-function pointer so the same combination-
 * enumeration and histogram logic drives both the CPU and CUDA backends.
 *
 * Features:
 *   - Hash-map histogram (exact double keys, no epsilon binning)
 *   - Optional random sampling of deletion subsets via DFS pick-filter
 */

#include "delta_dress_impl.h"
#include "dress_histogram.h"
#include "dress/dress.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

/* ------------------------------------------------------------------ */
/*  Internal helpers                                                   */
/* ------------------------------------------------------------------ */

/* Build a subgraph by removing the vertices listed in `del` (sorted,
 * length `k`) from the original graph `g`.
 *
 * Returns a freshly allocated dress graph with remapped vertex ids
 * (0 .. N-k-1) and only the edges whose both endpoints survive.
 * Returns NULL if the subgraph has zero edges.
 *
 * If `edge_map` is non-NULL (size E), fills edge_map[e] with the
 * subgraph edge index of original edge e, or -1 if edge e was removed. */
static p_dress_graph_t delta_build_subgraph(p_dress_graph_t g,
                                            const int *del, int k,
                                            int *edge_map)
{
    int N = g->N;
    int E = g->E;
    int sub_N = N - k;

    /* Map old vertex id -> new id (-1 = deleted). */
    int *vertex_map = (int *)malloc(N * sizeof(int));
    int di = 0, new_id = 0;
    for (int v = 0; v < N; v++) {
        if (di < k && v == del[di]) {
            vertex_map[v] = -1;
            di++;
        } else {
            vertex_map[v] = new_id++;
        }
    }

    /* Count surviving edges. */
    int sub_E = 0;
    for (int e = 0; e < E; e++) {
        if (vertex_map[g->U[e]] >= 0 && vertex_map[g->V[e]] >= 0)
            sub_E++;
    }

    free(vertex_map);

    if (sub_E == 0) {
        if (edge_map) {
            for (int e = 0; e < E; e++) edge_map[e] = -1;
        }
        return NULL;
    }

    /* Rebuild mapping (two-pass keeps hot path lean). */
    vertex_map = (int *)malloc(N * sizeof(int));
    di = 0; new_id = 0;
    for (int v = 0; v < N; v++) {
        if (di < k && v == del[di]) {
            vertex_map[v] = -1;
            di++;
        } else {
            vertex_map[v] = new_id++;
        }
    }

    /* Allocate edge arrays (dress_init_graph takes ownership). */
    int *sub_U = (int *)malloc(sub_E * sizeof(int));
    int *sub_V = (int *)malloc(sub_E * sizeof(int));
    double *sub_W = NULL;
    if (g->W != NULL)
        sub_W = (double *)malloc(sub_E * sizeof(double));
    int idx = 0;
    for (int e = 0; e < E; e++) {
        int mu = vertex_map[g->U[e]];
        int mv = vertex_map[g->V[e]];
        if (mu >= 0 && mv >= 0) {
            sub_U[idx] = mu;
            sub_V[idx] = mv;
            if (sub_W) sub_W[idx] = g->W[e];
            if (edge_map) edge_map[e] = idx;
            idx++;
        } else {
            if (edge_map) edge_map[e] = -1;
        }
    }

    free(vertex_map);

    return dress_init_graph(sub_N, sub_E, sub_U, sub_V,
                            sub_W, NULL, g->variant,
                            g->precompute_intercepts);
}

/* Accumulate converged edge dress values into hash-map histogram. */
static void delta_accumulate_histogram(p_dress_graph_t sub,
                                       dress_histogram_t *hist)
{
    for (int e = 0; e < sub->E; e++) {
        dress_hist_add(hist, sub->edge_dress[e]);
    }
}

/* Fill one row of the multisets matrix. */
static void delta_fill_multiset_row(p_dress_graph_t sub,
                                    const int *edge_map, int orig_E,
                                    double *row)
{
    for (int e = 0; e < orig_E; e++) {
        if (edge_map[e] >= 0)
            row[e] = sub->edge_dress[edge_map[e]];
        else
            row[e] = NAN;
    }
}

/* Binomial coefficient C(n, k). */
static int64_t delta_binom(int n, int k)
{
    if (k < 0 || k > n) return 0;
    if (k == 0 || k == n) return 1;
    if (k > n - k) k = n - k;
    int64_t r = 1;
    for (int i = 0; i < k; i++) {
        r = r * (n - i) / (i + 1);
    }
    return r;
}

/* Public binomial — exposed for OMP/MPI wrappers. */
int64_t dress_delta_binom(int n, int k)
{
    return delta_binom(n, k);
}

/* ── Sampling helpers ─────────────────────────────────────────────── */

/* 64-bit random from two dress_rand_r calls. */
static int64_t delta_rand64(unsigned int *seed, int64_t bound)
{
    int64_t r = ((int64_t)dress_rand_r(seed) << 31) | (int64_t)dress_rand_r(seed);
    return (r & 0x7FFFFFFFFFFFFFFFLL) % bound;
}

/* qsort comparator for int64_t. */
static int delta_cmp_i64(const void *a, const void *b)
{
    int64_t x = *(const int64_t *)a;
    int64_t y = *(const int64_t *)b;
    return (x > y) - (x < y);
}

/* ------------------------------------------------------------------ */
/*  Shared Δ^k-DRESS implementation                                    */
/* ------------------------------------------------------------------ */

int64_t *dress_delta_fit_impl_flat(p_dress_graph_t g, int k,
                                   int iterations, double epsilon,
                                   int n_samples, unsigned int seed,
                                   int *hist_size,
                                   int keep_multisets,
                                   double **multisets,
                                   int64_t *num_subgraphs,
                                   dress_fit_fn fit_fn,
                                   int offset, int stride)
{
    int N = g->N;
    int E = g->E;
    int do_hist = (hist_size != NULL);

    /* Hash-map histogram (exact double keys) — only when requested. */
    dress_histogram_t hist;
    if (do_hist) dress_hist_init(&hist, (size_t)E);

    int64_t cnk = (k == 0) ? 1 : delta_binom(N, k);
    if (num_subgraphs) *num_subgraphs = cnk;

    int wants_ms = keep_multisets && multisets;
    double *ms = NULL;
    if (wants_ms) {
        size_t ms_len = (size_t)cnk * E;
        ms = (double *)malloc(ms_len * sizeof(double));
        if (ms) {
            for (size_t i = 0; i < ms_len; i++) ms[i] = NAN;
            *multisets = ms;
        } else {
            wants_ms = 0;
        }
    }

    /* ── k = 0: Δ^0 — run DRESS on the full graph ──────────────── */
    if (k == 0) {
        if (offset != 0) {
            int64_t *flat = do_hist ? dress_hist_flatten(&hist, hist_size) : NULL;
            if (do_hist) dress_hist_free(&hist);
            return flat;
        }

        /* Fit on the original graph directly — no copy needed. */
        fit_fn(g, iterations, epsilon, NULL, NULL);
        if (do_hist) delta_accumulate_histogram(g, &hist);

        if (wants_ms) {
            for (int e = 0; e < E; e++)
                ms[e] = g->edge_dress[e];
        }
        int64_t *flat = do_hist ? dress_hist_flatten(&hist, hist_size) : NULL;
        if (do_hist) dress_hist_free(&hist);
        return flat;
    }

    /* ── k >= N: no valid deletion subsets ───────────────────────── */
    if (k >= N) {
        int64_t *flat = do_hist ? dress_hist_flatten(&hist, hist_size) : NULL;
        if (do_hist) dress_hist_free(&hist);
        return flat;
    }

    /* ── Sampling setup ─────────────────────────────────────────── */
    int64_t cnk_per_stride = (cnk + stride - 1 - offset) / stride;
    int64_t my_limit = (n_samples == 0)
        ? cnk_per_stride
        : (n_samples < cnk_per_stride ? n_samples : cnk_per_stride);

    int64_t *picks = NULL;
    int n_picks = 0;
    int use_sampling = (n_samples > 0 && my_limit < cnk_per_stride);
    if (use_sampling) {
        picks = (int64_t *)malloc((size_t)my_limit * sizeof(int64_t));
        unsigned int my_seed = seed + (unsigned int)offset;
        for (int64_t i = 0; i < my_limit; i++)
            picks[i] = delta_rand64(&my_seed, cnk_per_stride);
        qsort(picks, (size_t)my_limit, sizeof(int64_t), delta_cmp_i64);
        /* dedupe in-place */
        int64_t u = 0;
        for (int64_t i = 0; i < my_limit; i++)
            if (i == 0 || picks[i] != picks[i - 1])
                picks[u++] = picks[i];
        n_picks = (int)u;
    }

    int64_t my_s = 0;    /* count within this stride */
    int pick_idx = 0;

    /* ── k >= 1: iterative DFS over C(N, k) combinations ───────── */

    int *combo = (int *)malloc(k * sizeof(int));
    int *edge_map = wants_ms ? (int *)malloc(E * sizeof(int)) : NULL;

    int depth = 0;
    combo[0] = -1;
    int64_t s = 0;

    while (depth >= 0) {
        combo[depth]++;

        if (combo[depth] > N - k + depth) {
            depth--;
            continue;
        }

        if (depth == k - 1) {
            if (s % stride == offset) {
                /* Decide whether to process this combo. */
                int process = !use_sampling
                    || (pick_idx < n_picks && my_s == picks[pick_idx]);
                pick_idx += (process & use_sampling);

                if (process) {
                    p_dress_graph_t sub = delta_build_subgraph(g, combo, k,
                                                               edge_map);
                    if (sub) {
                        fit_fn(sub, iterations, epsilon, NULL, NULL);
                        if (do_hist) delta_accumulate_histogram(sub, &hist);
                        if (wants_ms)
                            delta_fill_multiset_row(sub, edge_map, E,
                                                    ms + s * E);
                        dress_free_graph(sub);
                    } else if (wants_ms) {
                        double *row = ms + s * E;
                        for (int e = 0; e < E; e++) row[e] = NAN;
                    }
                }
                my_s++;

                /* Early exit when all picks consumed. */
                if (use_sampling && pick_idx >= n_picks)
                    break;
            }
            s++;
        } else {
            depth++;
            combo[depth] = combo[depth - 1];
        }
    }

    free(picks);
    free(combo);
    free(edge_map);

    int64_t *flat = do_hist ? dress_hist_flatten(&hist, hist_size) : NULL;
    if (do_hist) dress_hist_free(&hist);
    return flat;
}
