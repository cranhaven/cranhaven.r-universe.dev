#include "dress/dress.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ------------------------------------------------------------------ */
/*  Internal types                                                     */
/* ------------------------------------------------------------------ */

// Temporary adjacency entry used during graph construction.
// Carries the neighbor id, original edge index, and edge weight.
typedef struct __adj_edge_t {
    int    x;      // neighbor vertex id
    int    e_idx;  // index into the input edge list
    double w;      // edge weight (possibly doubled for undirected)
} adj_edge_t, *p_adj_edge_t;

/* ------------------------------------------------------------------ */
/*  Static helpers                                                     */
/* ------------------------------------------------------------------ */

// Comparator for sorting adj_edge_t entries by neighbor id (ascending).
static int edge_cmp(const void *a, const void *b)
{
    const adj_edge_t *ea = (const adj_edge_t *)a;
    const adj_edge_t *eb = (const adj_edge_t *)b;
    return (ea->x > eb->x) - (ea->x < eb->x);
}

// Comparator for sorting doubles (ascending) — used for KBN summation.
static int dbl_cmp(const void *a, const void *b)
{
    double da = *(const double *)a;
    double db = *(const double *)b;
    return (da > db) - (da < db);
}

/* ------------------------------------------------------------------ */
/*  Label-independent summation: sort + Kahan-Babuška-Neumaier (KBN)   */
/* ------------------------------------------------------------------ */

// Maximum degree for stack-allocated buffer.
// Beyond this, we fall back to heap allocation.
// 4096 doubles = 32 KB — safe even for OMP thread stacks.
#define KBN_STACK_LIMIT 4096

// Sort an array of doubles in-place, then sum with KBN compensation.
// Returns the compensated sum.
static double kbn_sorted_sum(double *buf, int n)
{
    if (n == 0) return 0.0;
    qsort(buf, n, sizeof(double), dbl_cmp);
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

// Find edge u->v in raw adjacency (u's segment is sorted by neighbor id).
// Returns index in raw_data if found, -1 otherwise.
static int find_raw_edge(int u, int v, const int *raw_offset, const adj_edge_t *raw_data)
{
    int lo = raw_offset[u];
    int hi = raw_offset[u + 1] - 1;
    while (lo <= hi) {
        int mid = lo + (hi - lo) / 2;
        int x = raw_data[mid].x;
        if (x == v) {
            return mid;
        }
        if (x < v) {
            lo = mid + 1;
        } else {
            hi = mid - 1;
        }
    }
    return -1;
}

/* ------------------------------------------------------------------ */
/*  Graph construction                                                 */
/* ------------------------------------------------------------------ */

// Build the raw (input-level) adjacency in a flat CSR-like layout.
//
// For UNDIRECTED, each edge (u,v) produces two entries: v in N(u) and u in N(v).
// For directed variants, only the out-edge u→v is stored.
//
// Output:
//   *out_offset : [N+1] prefix-sum array of per-vertex degrees
//   *out_data   : flat array of adj_edge_t, sorted by neighbor id per vertex
//
// Both output arrays are heap-allocated; caller takes ownership.
static void build_raw_adjacency(p_dress_graph_t g,
                                int **out_offset, adj_edge_t **out_data,
                                double *W)
{
    int N = g->N, E = g->E, i;
    dress_variant_t variant = g->variant;
    int *U = g->U, *V = g->V;
    int *cnt = (int *)calloc(N, sizeof(int));

    // Count per-vertex degree
    for (i = 0; i < E; i++) {
        int u = U[i], v = V[i];
        if (variant == DRESS_VARIANT_UNDIRECTED) { cnt[u]++; cnt[v]++; }
        else                                      { cnt[u]++; }
    }

    // Compute CSR offsets via prefix sum
    int *offset = (int *)malloc((N + 1) * sizeof(int));
    offset[0] = 0;
    for (i = 0; i < N; i++)
        offset[i + 1] = offset[i] + cnt[i];

    // Single flat allocation for all adjacency entries
    adj_edge_t *data = (adj_edge_t *)malloc(offset[N] * sizeof(adj_edge_t));

    // Scatter edges into their respective vertex segments
    memset(cnt, 0, N * sizeof(int));
    for (i = 0; i < E; i++) {
        int    u = U[i], v = V[i];
        double w = (W == NULL) ? 1.0 : W[i];

        if (variant == DRESS_VARIANT_UNDIRECTED) {
            int pu = offset[u] + cnt[u]++;
            int pv = offset[v] + cnt[v]++;
            data[pu].x = v;  data[pu].e_idx = i;  data[pu].w = w;
            data[pv].x = u;  data[pv].e_idx = i;  data[pv].w = w;
        } else {
            int pu = offset[u] + cnt[u]++;
            data[pu].x = v;  data[pu].e_idx = i;  data[pu].w = w;
        }
    }
    free(cnt);
    // W ownership is retained by the graph (g->input_weight) and freed
    // in dress_free_graph — do NOT free it here.
    
    // Sort each vertex segment by neighbor id for binary-search access
#ifdef _OPENMP
    #pragma omp parallel for
#endif
    for (i = 0; i < N; i++) {
        int sz = offset[i + 1] - offset[i];
        if (sz > 1)
            qsort(&data[offset[i]], sz, sizeof(adj_edge_t), edge_cmp);
    }

    *out_offset = offset;
    *out_data   = data;
}

// Build the variant-specific adjacency and write it into the graph's
// final CSR arrays (adj_offset, adj_target, adj_edge_idx, edge_weight).
//
// The variant adjacency differs from the raw adjacency:
//   UNDIRECTED:  N[u] = N(u), weight doubled (w_uv = w_in + w_out = 2w)
//   DIRECTED:    N[u] = in(u) ∪ out(u), weight = w(u→v) + w(v→u) if both exist
//   FORWARD:     N[u] = out(u)
//   BACKWARD:    N[u] = in(u)
//
// Takes ownership of raw_offset and raw_data (freed internally).
static void build_variant_adjacency(p_dress_graph_t g,
                                    int *raw_offset, adj_edge_t *raw_data)
{
    int N = g->N, E = g->E, i, e, S;
    dress_variant_t variant = g->variant;
    int *U = g->U, *V = g->V;

    // Per-vertex degree counters for the variant adjacency.
    // For DIRECTED, N[u] = out(u) U in(u) with one entry per neighbor.
    int *node_out_degree = (int *)calloc(N + 1, sizeof(int));
    int *node_in_degree  = (int *)calloc(N + 1, sizeof(int));

    // Count variant-specific per-vertex degrees
    for (e = 0; e < E; e++) {
        int u = U[e], v = V[e];
        if (variant == DRESS_VARIANT_UNDIRECTED) {
            node_out_degree[u]++;
            node_out_degree[v]++;
        } else if (variant == DRESS_VARIANT_DIRECTED) {
            // Out-neighbor for u always appears in N[u].
            node_out_degree[u]++;

            // Add incoming neighbor for v only if reciprocal v->u is absent.
            // If v->u exists, u is already an out-neighbor of v.
            if (find_raw_edge(v, u, raw_offset, raw_data) < 0) {
                node_in_degree[v]++;
            }
        } else if (variant == DRESS_VARIANT_FORWARD) {
            node_out_degree[u]++;
        } else if (variant == DRESS_VARIANT_BACKWARD) {
            node_out_degree[v]++;
        }
    }

    // Compute CSR offsets for the temporary variant adjacency
    int *tmp_offset = (int *)malloc((N + 1) * sizeof(int));
    tmp_offset[0] = 0;
    int max_deg = 0;
    for (i = 0; i < N; i++) {
        if (variant == DRESS_VARIANT_DIRECTED) {
            // Merge in-degree into out-degree to get total adjacency size
            node_out_degree[i] = node_out_degree[i] + node_in_degree[i];
        }
        if (node_out_degree[i] > max_deg) max_deg = node_out_degree[i];
        tmp_offset[i + 1] = tmp_offset[i] + node_out_degree[i];
    }
    g->max_degree = max_deg;
    S = tmp_offset[N];

    free(node_out_degree);
    free(node_in_degree);
    adj_edge_t *tmp_data = (adj_edge_t *)malloc(S * sizeof(adj_edge_t));
    int *tmp_count = (int *)calloc(N, sizeof(int));

    // Populate the variant adjacency by iterating over each vertex's raw
    // neighbors and applying variant-specific rules.
    for (int u = 0; u < N; u++) {
        int raw_start = raw_offset[u];
        int raw_end   = raw_offset[u + 1];
        for (i = raw_start; i < raw_end; i++) {
            int    v   = raw_data[i].x;
            int    eid = raw_data[i].e_idx;
            double w   = raw_data[i].w;

            if (variant == DRESS_VARIANT_UNDIRECTED) {
                int pu = tmp_offset[u] + tmp_count[u]++;
                tmp_data[pu].x = v;  tmp_data[pu].e_idx = eid;  tmp_data[pu].w = 2.0 * w;
            } else if (variant == DRESS_VARIANT_DIRECTED) {
                // Directed: outgoing neighbor always in N[u].
                // Its weight is w(u->v) + w(v->u) when reciprocal exists.
                int reciprocal = find_raw_edge(v, u, raw_offset, raw_data);

                int pu = tmp_offset[u] + tmp_count[u]++;
                tmp_data[pu].x = v;  tmp_data[pu].e_idx = eid;  tmp_data[pu].w = w;
                if (reciprocal >= 0) {
                    tmp_data[pu].w += raw_data[reciprocal].w;
                }

                // Incoming-only neighbor for v if v has no out-edge to u.
                if (reciprocal < 0) {
                    int pv = tmp_offset[v] + tmp_count[v]++;
                    tmp_data[pv].x = u;  tmp_data[pv].e_idx = eid;  tmp_data[pv].w = w;
                }
            } else if (variant == DRESS_VARIANT_FORWARD) {
                // Forward: only outgoing edges
                int pu = tmp_offset[u] + tmp_count[u]++;
                tmp_data[pu].x = v;  tmp_data[pu].e_idx = eid;  tmp_data[pu].w = w;
            } else if (variant == DRESS_VARIANT_BACKWARD) {
                // Backward: only incoming edges
                int pv = tmp_offset[v] + tmp_count[v]++;
                tmp_data[pv].x = u;  tmp_data[pv].e_idx = eid;  tmp_data[pv].w = w;
            }
        }
    }

    // Raw adjacency no longer needed
    free(raw_offset);
    free(raw_data);
    free(tmp_count);

    // Transfer tmp_offset as the final CSR offset array (same layout)
    g->adj_offset   = tmp_offset;
    g->adj_target   = (int *)malloc(S * sizeof(int));
    g->adj_edge_idx = (int *)malloc(S * sizeof(int));
    g->edge_weight  = (double *)malloc(E * sizeof(double));

    // Sort each vertex segment by neighbor id, then split the adj_edge_t
    // struct-of-arrays into the final CSR arrays
#ifdef _OPENMP
    #pragma omp parallel for
#endif
    for (i = 0; i < N; i++) {
        int base = tmp_offset[i];
        int sz   = tmp_offset[i + 1] - tmp_offset[i];
        if (sz > 1)
            qsort(&tmp_data[base], sz, sizeof(adj_edge_t), edge_cmp);
        for (int j = 0; j < sz; j++) {
            g->adj_target[base + j]   = tmp_data[base + j].x;
            g->adj_edge_idx[base + j] = tmp_data[base + j].e_idx;
            g->edge_weight[tmp_data[base + j].e_idx] = tmp_data[base + j].w;
        }
    }

    free(tmp_data);
}

// Precompute neighborhood intercepts for every edge.
//
// For each edge e = (u,v), finds the set of common neighbors
// X = N[u] ∩ N[v] using a sorted-merge walk over the CSR adjacency.
// Stores the edge indices for (u,x) and (v,x) for each x ∈ X in flat
// arrays indexed by intercept_offset[e] .. intercept_offset[e+1].
//
// This trades O(∑|N[u]∩N[v]|) extra memory for reducing the per-edge
// iteration cost from O(deg_u + deg_v) to O(|N[u] ∩ N[v]|).
static void compute_intercepts(p_dress_graph_t g)
{
    int E = g->E, e, T;
    int *U = g->U, *V = g->V;

    g->intercept_offset = (int *)malloc((E + 1) * sizeof(int));

    // First pass: count |N[u] ∩ N[v]| for each edge
#ifdef _OPENMP
    #pragma omp parallel for
#endif
    for (e = 0; e < E; e++) {
        int u  = U[e], v = V[e];
        int iu = g->adj_offset[u], iu_end = g->adj_offset[u + 1];
        int iv = g->adj_offset[v], iv_end = g->adj_offset[v + 1];
        int cnt = 0;

        while (iu < iu_end && iv < iv_end) {
            int x = g->adj_target[iu], y = g->adj_target[iv];
            cnt += (x == y);
            iu += (x <= y);
            iv += (x >= y);
        }
        g->intercept_offset[e + 1] = cnt;
    }

    // Convert per-edge counts into a prefix sum (CSR offsets)
    g->intercept_offset[0] = 0;
    for (e = 0; e < E; e++)
        g->intercept_offset[e + 1] += g->intercept_offset[e];

    T = g->intercept_offset[E];
    g->intercept_edge_ux = (int *)malloc(T * sizeof(int));
    g->intercept_edge_vx = (int *)malloc(T * sizeof(int));

    // Second pass: record edge indices for each common neighbor
#ifdef _OPENMP
    #pragma omp parallel for
#endif
    for (e = 0; e < E; e++) {
        int u  = U[e], v = V[e];
        int iu = g->adj_offset[u], iu_end = g->adj_offset[u + 1];
        int iv = g->adj_offset[v], iv_end = g->adj_offset[v + 1];
        int off = g->intercept_offset[e];

        while (iu < iu_end && iv < iv_end) {
            int x = g->adj_target[iu], y = g->adj_target[iv];
            if (x == y) {
                g->intercept_edge_ux[off] = g->adj_edge_idx[iu];
                g->intercept_edge_vx[off] = g->adj_edge_idx[iv];
                off++;
            }
            iu += (x <= y);
            iv += (x >= y);
        }
    }
}

/* ------------------------------------------------------------------ */
/*  Public API: initialization                                         */
/* ------------------------------------------------------------------ */

// Construct a dress graph from an input edge list.
//
// 1. Builds raw adjacency (flat CSR) from the input edges.
// 2. Transforms it into the variant-specific adjacency (flat CSR).
// 3. Initializes all dress values to 2.0 (fixed-point identity).
// 4. Optionally precomputes intercepts for faster iteration.
//
// Takes ownership of U, V, and W (all freed by dress_free_graph).
// W may be NULL for unweighted graphs.
p_dress_graph_t dress_init_graph(int N, int E, int *U, int *V,
                                 double *W, double *NW,
                                 dress_variant_t variant,
                                 int precompute_intercepts)
{
    p_dress_graph_t g = (p_dress_graph_t)malloc(sizeof(dress_graph_t));
    int e;

    g->N = N;
    g->E = E;
    g->U = U;
    g->V = V;
    g->variant = variant;
    g->precompute_intercepts = precompute_intercepts;
    g->vertex_dress = (double *)malloc(N * sizeof(double));

    // Take ownership of W. delta_dress needs the raw input weights to
    // construct subgraphs correctly (edge_weight stores variant-specific
    // weights that differ from the raw input). Freed in dress_free_graph.
    g->W = W;  // NULL when unweighted

    // Take ownership of NW (vertex weights). If NULL, allocate all-1.0
    // so the hot loop avoids a branch per edge.
    if (NW != NULL) {
        g->NW = NW;
    } else {
        g->NW = (double *)malloc(N * sizeof(double));
        for (int i = 0; i < N; i++) g->NW[i] = 1.0;
    }

    // Build raw then variant adjacency (both use flat CSR internally)
    int *raw_offset;
    adj_edge_t *raw_data;
    build_raw_adjacency(g, &raw_offset, &raw_data, W);
    build_variant_adjacency(g, raw_offset, raw_data);
    // raw_offset and raw_data are freed inside build_variant_adjacency

    // Initialize dress double-buffer to 2.0 (the fixed-point starting value)
    g->edge_dress      = (double *)malloc(E * sizeof(double));
    g->edge_dress_next = (double *)malloc(E * sizeof(double));
    for (e = 0; e < E; e++) {
        g->edge_dress[e]      = 1.0;
        g->edge_dress_next[e] = 1.0;
    }

    if (g->precompute_intercepts)
        compute_intercepts(g);

    return g;
}

/* ------------------------------------------------------------------ */
/*  dress computation                                                  */
/* ------------------------------------------------------------------ */

// Compute the dress value for a single edge e = (u,v).
//
// dress(u,v) = [ ∑_{x ∈ N[u]∩N[v]} (w_ux · d_ux + w_vx · d_vx)
//                + 2 · w_uv · d_uv + 8 ]
//              / (‖u‖ · ‖v‖)
//
// where ‖u‖ = sqrt( 4 + ∑_x w_ux · d_ux )  (self-loop contributes 4),
// the numerator constant 8 accounts for both self-loop × self-loop terms,
// and the +2·w_uv·d_uv term adds the u-v edge's own contribution from
// both sides (it also appears in the intercept but is handled separately
// since the intercept walk excludes it when u and v are not mutual neighbors
// via other paths).
// In the directed variants, only one of u or v has the other as a neighbor, so
// only one self-loop contributes and the constant is 4 instead of 8, and the
// cross-term is w_uv·d_uv instead of 2·w_uv·d_uv.
//
// Writes the result into edge_dress_next[e] (double-buffer).

// Shared tail: add self-loop/cross term, KBN sum, normalize, store.
static inline double fit_finalize(p_dress_graph_t g, int e,
                                  int u, int v, double denominator,
                                  double *buf, double *stack_buf,
                                  int n)
{
    double uv = g->edge_weight[e] * g->edge_dress[e];
    double nw_u = g->NW[u];
    double nw_v = g->NW[v];
    if (g->variant == DRESS_VARIANT_FORWARD || g->variant == DRESS_VARIANT_BACKWARD) {
        buf[n++] = 4.0 * nw_u + uv;
    } else {
        buf[n++] = 4.0 * nw_u + 4.0 * nw_v + 2.0 * uv;
    }

    double numerator = kbn_sorted_sum(buf, n);
    if (buf != stack_buf) free(buf);

    double dress_uv = denominator > 0.0 ? numerator / denominator : 0.0;
    g->edge_dress_next[e] = dress_uv;
    return dress_uv;
}

// ---- Intercept path (precomputed common-neighbor tables) ----
static double fit_impl_intercept(p_dress_graph_t g, int e)
{
    int    u = g->U[e], v = g->V[e];
    double denominator = g->vertex_dress[u] * g->vertex_dress[v];
    int max_terms = g->intercept_offset[e + 1] - g->intercept_offset[e] + 1;

    double stack_buf[KBN_STACK_LIMIT];
    double *buf = (max_terms <= KBN_STACK_LIMIT)
                ? stack_buf
                : (double *)malloc(max_terms * sizeof(double));
    int n = 0;

    int off = g->intercept_offset[e];
    int end = g->intercept_offset[e + 1];
    for (int k = off; k < end; k++) {
        int eu = g->intercept_edge_ux[k];
        int ev = g->intercept_edge_vx[k];
        buf[n++] = g->edge_weight[eu] * g->edge_dress[eu]
                 + g->edge_weight[ev] * g->edge_dress[ev];
    }

    return fit_finalize(g, e, u, v, denominator, buf, stack_buf, n);
}

// ---- Merge-walk path (no precomputed intercepts) ----
static double fit_impl_merge(p_dress_graph_t g, int e)
{
    int    u = g->U[e], v = g->V[e];
    double denominator = g->vertex_dress[u] * g->vertex_dress[v];
    int deg_u = g->adj_offset[u + 1] - g->adj_offset[u];
    int deg_v = g->adj_offset[v + 1] - g->adj_offset[v];
    int max_terms = (deg_u < deg_v ? deg_u : deg_v) + 1;

    double stack_buf[KBN_STACK_LIMIT];
    double *buf = (max_terms <= KBN_STACK_LIMIT)
                ? stack_buf
                : (double *)malloc(max_terms * sizeof(double));
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

    return fit_finalize(g, e, u, v, denominator, buf, stack_buf, n);
}

/* ------------------------------------------------------------------ */
/*  Iterative fitting                                                  */
/* ------------------------------------------------------------------ */

// Run dress iterative fixed-point fitting (sequential).
//
// Each iteration:
//   1. Recompute vertex_dress[u] = sqrt(4*nw + ∑ w_ux · d_ux) for all u.
//   2. Recompute edge_dress_next[e] = fit_impl(e) for all e.
//   3. Swap edge_dress ↔ edge_dress_next (double-buffer).
//   4. Stop if max |d_old - d_new| < epsilon.
//
// On return:
//   *iterations = number of iterations performed (or max_iterations)
//   *delta      = final maximum per-edge change (only set on early stop)
void dress_fit(p_dress_graph_t g, int max_iterations, double epsilon,
               int *iterations, double *delta)
{
    int iter;

    for (iter = 0; iter < max_iterations; ++iter) {
        double max_delta = 0.0;
        int u, e;

        // Phase 1: compute per-vertex dress norm (sort+KBN for bitwise reproducibility)
        for (u = 0; u < g->N; u++) {
            int base = g->adj_offset[u];
            int end  = g->adj_offset[u + 1];
            int deg  = end - base;

            double stack_buf[KBN_STACK_LIMIT];
            double *buf = (deg + 1 <= KBN_STACK_LIMIT)
                        ? stack_buf
                        : (double *)malloc((deg + 1) * sizeof(double));

            buf[0] = 4.0 * g->NW[u]; // self-loop contribution
            for (int i = 0; i < deg; i++) {
                int ei = g->adj_edge_idx[base + i];
                buf[i + 1] = g->edge_weight[ei] * g->edge_dress[ei];
            }
            g->vertex_dress[u] = sqrt(kbn_sorted_sum(buf, deg + 1));

            if (buf != stack_buf) free(buf);
        }

        // Phase 2: compute next dress value for every edge
        if (g->precompute_intercepts) {
            for (e = 0; e < g->E; e++) {
                double prev = g->edge_dress[e];
                double next = fit_impl_intercept(g, e);
                double d    = fabs(prev - next);
                if (d > max_delta) max_delta = d;
            }
        } else {
            for (e = 0; e < g->E; e++) {
                double prev = g->edge_dress[e];
                double next = fit_impl_merge(g, e);
                double d    = fabs(prev - next);
                if (d > max_delta) max_delta = d;
            }
        }

        // Phase 3: swap double-buffer pointers
        double *tmp        = g->edge_dress;
        g->edge_dress      = g->edge_dress_next;
        g->edge_dress_next = tmp;

        // Phase 4: convergence check
        if (delta)      *delta      = max_delta;

        if (max_delta < epsilon) {
            if (iterations) *iterations = iter;
            return;
        }
    }

    if (iterations) *iterations = max_iterations;
}

/* ------------------------------------------------------------------ */
/*  Cleanup                                                            */
/* ------------------------------------------------------------------ */

// Free all heap memory owned by the dress graph, including U and V.
void dress_free_graph(p_dress_graph_t g)
{
    // CSR adjacency
    free(g->adj_offset);
    free(g->adj_target);
    free(g->adj_edge_idx);

    // Per-edge arrays
    free(g->W);
    free(g->edge_weight);
    free(g->edge_dress);
    free(g->edge_dress_next);

    // Per-vertex arrays
    free(g->vertex_dress);
    free(g->NW);

    // Precomputed intercepts (conditional)
    if (g->precompute_intercepts) {
        free(g->intercept_offset);
        free(g->intercept_edge_ux);
        free(g->intercept_edge_vx);
    }

    // Input edge list (ownership transferred at init)
    free(g->U);
    free(g->V);
    free(g);
}

/* ------------------------------------------------------------------ */
/*  Virtual-edge DRESS query                                           */
/* ------------------------------------------------------------------ */

// Query the DRESS value for any vertex pair (u, v).
// See dress.h for full documentation.
double dress_get(const p_dress_graph_t g, int u, int v,
                 int max_iterations, double epsilon,
                 double edge_weight)
{
    // Binary search for v in u's sorted adjacency list.
    int left  = g->adj_offset[u];
    int right = g->adj_offset[u + 1] - 1;
    while (left <= right) {
        int mid = left + (right - left) / 2;
        int nb  = g->adj_target[mid];
        if (nb == v)
            return g->edge_dress[g->adj_edge_idx[mid]];
        if (nb < v)
            left = mid + 1;
        else
            right = mid - 1;
    }

    // Virtual edge: merge-join neighbour lists to sum common-neighbour
    // contributions from the frozen steady state (sort+KBN for consistency).
    int deg_u = g->adj_offset[u + 1] - g->adj_offset[u];
    int deg_v = g->adj_offset[v + 1] - g->adj_offset[v];
    int max_cn = (deg_u < deg_v ? deg_u : deg_v) + 1;  // +1 for self-loop
    double cn_stack[KBN_STACK_LIMIT];
    double *cn_buf = (max_cn <= KBN_STACK_LIMIT)
                   ? cn_stack
                   : (double *)malloc(max_cn * sizeof(double));
    int cn_n = 0;
    {
        int iu = g->adj_offset[u], iu_end = g->adj_offset[u + 1];
        int iv = g->adj_offset[v], iv_end = g->adj_offset[v + 1];
        while (iu < iu_end && iv < iv_end) {
            int x = g->adj_target[iu], y = g->adj_target[iv];
            if (x == y) {
                int eu = g->adj_edge_idx[iu];
                int ev = g->adj_edge_idx[iv];
                cn_buf[cn_n++] = g->edge_weight[eu] * g->edge_dress[eu]
                               + g->edge_weight[ev] * g->edge_dress[ev];
            }
            iu += (x <= y);
            iv += (x >= y);
        }
    }

    // Self-loop constant: 4·nw(u)+4·nw(v) for UNDIRECTED/DIRECTED, 4·nw(u) for FORWARD/BACKWARD.
    double nw_u = g->NW[u];
    double nw_v = g->NW[v];
    double self_loop = (g->variant == DRESS_VARIANT_FORWARD ||
                        g->variant == DRESS_VARIANT_BACKWARD)
                     ? 4.0 * nw_u : 4.0 * nw_u + 4.0 * nw_v;

    cn_buf[cn_n++] = self_loop;
    double intercept = kbn_sorted_sum(cn_buf, cn_n);
    if (cn_buf != cn_stack) free(cn_buf);

    double cross_factor = (g->variant == DRESS_VARIANT_FORWARD ||
                           g->variant == DRESS_VARIANT_BACKWARD)
                        ? 1.0 : 2.0;

    // Combined edge weight c(u,v).
    double cw = (g->variant == DRESS_VARIANT_UNDIRECTED)
              ? 2.0 * edge_weight
              : edge_weight;

    double A   = intercept;
    double Du2 = g->vertex_dress[u] * g->vertex_dress[u];
    double Dv2 = g->vertex_dress[v] * g->vertex_dress[v];

    if (Du2 <= 0.0 || Dv2 <= 0.0)
        return 0.0;

    // Local fixed-point iteration: only the virtual edge's self-consistent
    // contribution is iterated; all other values stay frozen.
    double d_uv = 1.0;
    for (int iter = 0; iter < max_iterations; ++iter) {
        double wd   = cw * d_uv;
        double denom = sqrt(Du2 + wd) * sqrt(Dv2 + wd);
        if (denom <= 0.0)
            return 0.0;
        double next = (A + cross_factor * wd) / denom;
        if (fabs(next - d_uv) <= epsilon) {
            d_uv = next;
            break;
        }
        d_uv = next;
    }

    return d_uv;
}
