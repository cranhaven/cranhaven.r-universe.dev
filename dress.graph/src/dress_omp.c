/*
 * dress_omp.c — OpenMP-parallel dress_fit.
 *
 * Parallelises the per-vertex (Phase 1) and per-edge (Phase 2) loops
 * within each iteration.  Suitable for large single graphs.
 *
 * Build: compile with OpenMP enabled (-fopenmp / /openmp).
 */

#include "dress/dress.h"

#ifdef _OPENMP
#include <omp.h>
#endif

#include <math.h>
#include <stdlib.h>

/* ------------------------------------------------------------------ */
/*  Duplicated helpers from dress.c (static, not exported)             */
/* ------------------------------------------------------------------ */

#define KBN_STACK_LIMIT 4096

static int dbl_cmp_omp(const void *a, const void *b) {
    double x = *(const double *)a;
    double y = *(const double *)b;
    return (x > y) - (x < y);
}

static double kbn_sorted_sum_omp(double *buf, int n) {
    if (n == 0) return 0.0;
    qsort(buf, n, sizeof(double), dbl_cmp_omp);
    double sum  = buf[0];
    double comp = 0.0;
    for (int i = 1; i < n; i++) {
        double t = sum + buf[i];
        if (fabs(sum) >= fabs(buf[i]))
            comp += (sum - t) + buf[i];
        else
            comp += (buf[i] - t) + sum;
        sum = t;
    }
    return sum + comp;
}

/* ── fit_impl helpers (merge-walk + intercept) ───────────────────── */

static inline double fit_finalize_omp(p_dress_graph_t g, int e,
                                      int u, int v, double denominator,
                                      double *buf, double *stack_buf, int n)
{
    double uv = g->edge_weight[e] * g->edge_dress[e];
    double nw_u = g->NW[u];
    double nw_v = g->NW[v];
    if (g->variant == DRESS_VARIANT_FORWARD || g->variant == DRESS_VARIANT_BACKWARD) {
        buf[n++] = 4.0 * nw_u + uv;
    } else {
        buf[n++] = 4.0 * nw_u + 4.0 * nw_v + 2.0 * uv;
    }
    double numerator = kbn_sorted_sum_omp(buf, n);
    if (buf != stack_buf) free(buf);
    double dress_uv = denominator > 0.0 ? numerator / denominator : 0.0;
    g->edge_dress_next[e] = dress_uv;
    return dress_uv;
}

static double fit_impl_intercept_omp(p_dress_graph_t g, int e) {
    int u = g->U[e], v = g->V[e];
    double denominator = g->vertex_dress[u] * g->vertex_dress[v];
    int max_terms = g->intercept_offset[e + 1] - g->intercept_offset[e] + 1;
    double stack_buf[KBN_STACK_LIMIT];
    double *buf = (max_terms <= KBN_STACK_LIMIT)
                ? stack_buf : (double *)malloc(max_terms * sizeof(double));
    int n = 0;
    int off = g->intercept_offset[e];
    int end = g->intercept_offset[e + 1];
    for (int k = off; k < end; k++) {
        int eu = g->intercept_edge_ux[k];
        int ev = g->intercept_edge_vx[k];
        buf[n++] = g->edge_weight[eu] * g->edge_dress[eu]
                 + g->edge_weight[ev] * g->edge_dress[ev];
    }
    return fit_finalize_omp(g, e, u, v, denominator, buf, stack_buf, n);
}

static double fit_impl_merge_omp(p_dress_graph_t g, int e) {
    int u = g->U[e], v = g->V[e];
    double denominator = g->vertex_dress[u] * g->vertex_dress[v];
    int deg_u = g->adj_offset[u + 1] - g->adj_offset[u];
    int deg_v = g->adj_offset[v + 1] - g->adj_offset[v];
    int max_terms = (deg_u < deg_v ? deg_u : deg_v) + 1;
    double stack_buf[KBN_STACK_LIMIT];
    double *buf = (max_terms <= KBN_STACK_LIMIT)
                ? stack_buf : (double *)malloc(max_terms * sizeof(double));
    int n = 0;
    int iu = g->adj_offset[u], iu_end = g->adj_offset[u + 1];
    int iv = g->adj_offset[v], iv_end = g->adj_offset[v + 1];
    while (iu < iu_end && iv < iv_end) {
        int x = g->adj_target[iu], y = g->adj_target[iv];
        if (x == y) {
            int eu = g->adj_edge_idx[iu];
            int ev = g->adj_edge_idx[iv];
            buf[n++] = g->edge_weight[eu] * g->edge_dress[eu]
                     + g->edge_weight[ev] * g->edge_dress[ev];
        }
        iu += (x <= y);
        iv += (x >= y);
    }
    return fit_finalize_omp(g, e, u, v, denominator, buf, stack_buf, n);
}

/* ------------------------------------------------------------------ */
/*  dress_fit_omp — OpenMP-parallel iterative fitting                  */
/* ------------------------------------------------------------------ */

void dress_fit_omp(p_dress_graph_t g, int max_iterations, double epsilon,
                   int *iterations, double *delta)
{
    int iter;

    for (iter = 0; iter < max_iterations; ++iter) {
        double max_delta = 0.0;
        int u, e;

        /* Phase 1: per-vertex dress norm */
#ifdef _OPENMP
        #pragma omp parallel for
#endif
        for (u = 0; u < g->N; u++) {
            int base = g->adj_offset[u];
            int end  = g->adj_offset[u + 1];
            int deg  = end - base;
            double stack_buf[KBN_STACK_LIMIT];
            double *buf = (deg + 1 <= KBN_STACK_LIMIT)
                        ? stack_buf
                        : (double *)malloc((deg + 1) * sizeof(double));
            buf[0] = 4.0 * g->NW[u];
            for (int i = 0; i < deg; i++) {
                int ei = g->adj_edge_idx[base + i];
                buf[i + 1] = g->edge_weight[ei] * g->edge_dress[ei];
            }
            g->vertex_dress[u] = sqrt(kbn_sorted_sum_omp(buf, deg + 1));
            if (buf != stack_buf) free(buf);
        }

        /* Phase 2: per-edge dress update */
        if (g->precompute_intercepts) {
#ifdef _OPENMP
            #pragma omp parallel for reduction(max:max_delta)
#endif
            for (e = 0; e < g->E; e++) {
                double prev = g->edge_dress[e];
                double next = fit_impl_intercept_omp(g, e);
                double d    = fabs(prev - next);
                if (d > max_delta) max_delta = d;
            }
        } else {
#ifdef _OPENMP
            #pragma omp parallel for reduction(max:max_delta)
#endif
            for (e = 0; e < g->E; e++) {
                double prev = g->edge_dress[e];
                double next = fit_impl_merge_omp(g, e);
                double d    = fabs(prev - next);
                if (d > max_delta) max_delta = d;
            }
        }

        /* Phase 3: swap double-buffer */
        double *tmp        = g->edge_dress;
        g->edge_dress      = g->edge_dress_next;
        g->edge_dress_next = tmp;

        /* Phase 4: convergence check */
        if (delta) *delta = max_delta;
        if (max_delta < epsilon) {
            if (iterations) *iterations = iter;
            return;
        }
    }

    if (iterations) *iterations = max_iterations;
}
