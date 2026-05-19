/**
 * dress_cuda.cu — CUDA implementation of dress_fit() for libdress.
 *
 * Exact algorithmic match with the CPU fit() in dress.c:
 *
 *   Per iteration:
 *     Phase 1 — vertex_dress[u] = sqrt(4 + Σ edge_weight[e]*edge_dress[e])
 *               over all half-edges in u's CSR segment.
 *               → One thread per vertex.
 *
 *     Phase 2 — For each edge e = (u,v), compute dress(u,v):
 *               ● With intercepts: walk intercept_edge_ux/vx arrays.
 *               ● Without intercepts: sorted-merge on adj_target.
 *               Add variant-specific self-loop/cross-term:
 *                 FORWARD|BACKWARD: 4 + w·d
 *                 UNDIRECTED|DIRECTED: 8 + 2·w·d
 *               Divide by vertex_dress[u]*vertex_dress[v].
 *               Write to edge_dress_next[e].
 *               Track max |d_old − d_new| via atomicMax.
 *               → One thread per edge.
 *
 *     Phase 3 — Swap edge_dress ↔ edge_dress_next (pointer swap on host).
 *     Phase 4 — Check convergence (download scalar max_delta from GPU).
 *
 * After the loop, download edge_dress[] and vertex_dress[] to the host graph.
 */

#include "dress/cuda/dress_cuda.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cuda_runtime.h>

/* ------------------------------------------------------------------ */
/*  Error checking                                                     */
/* ------------------------------------------------------------------ */

#define CUDA_CHECK(call)                                                 \
    do {                                                                 \
        cudaError_t _err = (call);                                       \
        if (_err != cudaSuccess) {                                       \
            fprintf(stderr, "CUDA error %s:%d — %s\n",                  \
                    __FILE__, __LINE__, cudaGetErrorString(_err));        \
            exit(EXIT_FAILURE);                                          \
        }                                                                \
    } while (0)

/* ------------------------------------------------------------------ */
/*  Host-side batch planning                                           */
/* ------------------------------------------------------------------ */

typedef struct __work_batch_plan_t {
    int      batch_count;
    int     *batch_start;
    int     *batch_size;
    size_t  *batch_workspace;
    size_t   max_workspace;
    size_t  *d_item_offset;
} work_batch_plan_t;

static void init_work_batch_plan(work_batch_plan_t *plan)
{
    memset(plan, 0, sizeof(*plan));
}

static void free_work_batch_plan(work_batch_plan_t *plan)
{
    free(plan->batch_start);
    free(plan->batch_size);
    free(plan->batch_workspace);
    if (plan->d_item_offset) cudaFree(plan->d_item_offset);
    init_work_batch_plan(plan);
}

static void build_work_batch_plan(const int *requirement,
                                  int item_count,
                                  size_t workspace_budget_doubles,
                                  work_batch_plan_t *plan)
{
    init_work_batch_plan(plan);
    if (item_count <= 0) return;

    plan->batch_start = (int *)malloc((size_t)item_count * sizeof(int));
    plan->batch_size = (int *)malloc((size_t)item_count * sizeof(int));
    plan->batch_workspace = (size_t *)malloc((size_t)item_count * sizeof(size_t));
    size_t *item_offset = (size_t *)malloc((size_t)item_count * sizeof(size_t));
    if (!plan->batch_start || !plan->batch_size || !plan->batch_workspace || !item_offset) {
        fprintf(stderr, "Host allocation failed while planning CUDA work batches\n");
        exit(EXIT_FAILURE);
    }

    int batch_idx = 0;
    int batch_start = 0;
    size_t batch_used = 0;

    for (int item = 0; item < item_count; item++) {
        size_t need = (size_t)requirement[item];
        if (need == 0) need = 1;

        if (need > workspace_budget_doubles) {
            fprintf(stderr,
                    "CUDA error: largest per-item workspace (%zu doubles) exceeds the configured batch budget (%zu doubles)\n",
                    need, workspace_budget_doubles);
            exit(EXIT_FAILURE);
        }

        if (batch_used > 0 && batch_used + need > workspace_budget_doubles) {
            plan->batch_start[batch_idx] = batch_start;
            plan->batch_size[batch_idx] = item - batch_start;
            plan->batch_workspace[batch_idx] = batch_used;
            if (batch_used > plan->max_workspace) plan->max_workspace = batch_used;
            batch_idx++;
            batch_start = item;
            batch_used = 0;
        }

        item_offset[item] = batch_used;
        batch_used += need;
    }

    plan->batch_start[batch_idx] = batch_start;
    plan->batch_size[batch_idx] = item_count - batch_start;
    plan->batch_workspace[batch_idx] = batch_used;
    if (batch_used > plan->max_workspace) plan->max_workspace = batch_used;
    plan->batch_count = batch_idx + 1;

    CUDA_CHECK(cudaMalloc(&plan->d_item_offset, (size_t)item_count * sizeof(size_t)));
    CUDA_CHECK(cudaMemcpy(plan->d_item_offset, item_offset,
                          (size_t)item_count * sizeof(size_t), cudaMemcpyHostToDevice));

    free(item_offset);
}

/* ------------------------------------------------------------------ */
/*  Double-precision atomicMax via CAS                                 */
/* ------------------------------------------------------------------ */

static __device__ __forceinline__
void atomicMaxDouble(unsigned long long *addr, double val)
{
    unsigned long long val_bits = __double_as_longlong(val);
    unsigned long long old = *addr;
    unsigned long long assumed;
    do {
        assumed = old;
        if (__longlong_as_double(assumed) >= val) return;
        old = atomicCAS(addr, assumed, val_bits);
    } while (assumed != old);
}

/* ------------------------------------------------------------------ */
/*  Label-independent summation: sort + KBN on GPU                     */
/* ------------------------------------------------------------------ */

// Insertion sort for small partitions (n ≤ 16).
static __device__ __forceinline__
void insertion_sort_d(double *arr, int lo, int hi)
{
    for (int i = lo + 1; i <= hi; i++) {
        double key = arr[i];
        int j = i - 1;
        while (j >= lo && arr[j] > key) {
            arr[j + 1] = arr[j];
            j--;
        }
        arr[j + 1] = key;
    }
}

// Median-of-three pivot selection.
static __device__ __forceinline__
int median3(double *arr, int lo, int hi)
{
    int mid = lo + (hi - lo) / 2;
    if (arr[lo] > arr[mid]) { double t = arr[lo]; arr[lo] = arr[mid]; arr[mid] = t; }
    if (arr[lo] > arr[hi])  { double t = arr[lo]; arr[lo] = arr[hi];  arr[hi]  = t; }
    if (arr[mid] > arr[hi]) { double t = arr[mid]; arr[mid] = arr[hi]; arr[hi] = t; }
    return mid;
}

// Iterative quicksort with median-of-three pivot and insertion sort cutoff.
// Stack depth ≤ 2·log₂(n) — 64 entries handles n up to 2^32.
static __device__ __forceinline__
void sort_d(double *arr, int n)
{
    if (n <= 16) { insertion_sort_d(arr, 0, n - 1); return; }

    int stack[64];
    int sp = 0;
    stack[sp++] = 0;
    stack[sp++] = n - 1;

    while (sp > 0) {
        int hi = stack[--sp];
        int lo = stack[--sp];

        if (hi - lo < 16) {
            insertion_sort_d(arr, lo, hi);
            continue;
        }

        // Median-of-three pivot, place at hi-1
        int m = median3(arr, lo, hi);
        double t = arr[m]; arr[m] = arr[hi - 1]; arr[hi - 1] = t;
        double pivot = arr[hi - 1];

        // Hoare-style partition
        int i = lo, j = hi - 1;
        for (;;) {
            while (arr[++i] < pivot) {}
            while (arr[--j] > pivot) {}
            if (i >= j) break;
            t = arr[i]; arr[i] = arr[j]; arr[j] = t;
        }
        // Restore pivot
        arr[hi - 1] = arr[i]; arr[i] = pivot;

        // Push larger partition first (limits stack depth to log₂(n))
        int left_size  = i - lo;
        int right_size = hi - i;
        if (left_size > right_size) {
            if (left_size > 1)  { stack[sp++] = lo;    stack[sp++] = i - 1; }
            if (right_size > 1) { stack[sp++] = i + 1; stack[sp++] = hi;    }
        } else {
            if (right_size > 1) { stack[sp++] = i + 1; stack[sp++] = hi;    }
            if (left_size > 1)  { stack[sp++] = lo;    stack[sp++] = i - 1; }
        }
    }
}

// KBN compensated sum of a pre-sorted array.
static __device__ __forceinline__
double kbn_sum_d(const double *arr, int n)
{
    if (n == 0) return 0.0;
    double sum  = arr[0];
    double comp = 0.0;
    for (int i = 1; i < n; i++) {
        double t = sum + arr[i];
        if (fabs(sum) >= fabs(arr[i]))
            comp += (sum - t) + arr[i];
        else
            comp += (arr[i] - t) + sum;
        sum = t;
    }
    return sum + comp;
}

/* ------------------------------------------------------------------ */
/*  Kernel 1: vertex_dress                                               */
/*                                                                     */
/*  vertex_dress[u] = sqrt( 4 + Σ edge_weight[ei] * edge_dress[ei] )    */
/*  where the sum runs over all half-edges in u's CSR row.             */
/*                                                                     */
/*  Each thread gets an exact slice of d_work via work_offset[tid].    */
/*  Batches are planned on the host so the shared scratch fits VRAM.   */
/* ------------------------------------------------------------------ */

__global__ void
kernel_node_dress(const int    item_start,
                  const int    item_count,
                  const int   * __restrict__ adj_offset,
                  const int   * __restrict__ adj_edge_idx,
                  const double* __restrict__ edge_weight,
                  const double* __restrict__ edge_dress,
                  double      * __restrict__ vertex_dress,
                  const double* __restrict__ NW,
                  const size_t* __restrict__ work_offset,
                  double      * __restrict__ d_work)
{
    int tid = blockIdx.x * blockDim.x + threadIdx.x;
    if (tid >= item_count) return;

    int u = item_start + tid;

    int base = adj_offset[u];
    int end  = adj_offset[u + 1];
    int deg  = end - base;

    double *buf = d_work + work_offset[tid];

    buf[0] = 4.0 * (NW ? NW[u] : 1.0);  /* self-loop contribution */
    for (int i = 0; i < deg; i++) {
        int ei = adj_edge_idx[base + i];
        buf[i + 1] = edge_weight[ei] * edge_dress[ei];
    }

    int n = deg + 1;
    sort_d(buf, n);
    vertex_dress[u] = sqrt(kbn_sum_d(buf, n));
}

/* ------------------------------------------------------------------ */
/*  Shared tail: self-loop term, sort+KBN, normalize, store, track     */
/* ------------------------------------------------------------------ */

static __device__ __forceinline__
void edge_finalize(int e, int u, int v, int variant,
                   const double* __restrict__ edge_weight,
                   const double* __restrict__ edge_dress,
                   const double* __restrict__ vertex_dress,
                   const double* __restrict__ NW,
                   double* __restrict__ edge_dress_next,
                   unsigned long long* __restrict__ d_max_delta,
                   double* buf, int n)
{
    double uv = edge_weight[e] * edge_dress[e];
    double nw_u = NW ? NW[u] : 1.0;
    double nw_v = NW ? NW[v] : 1.0;
    if (variant == 2 /* FORWARD */ || variant == 3 /* BACKWARD */) {
        buf[n++] = 4.0 * nw_u + uv;
    } else {
        buf[n++] = 4.0 * nw_u + 4.0 * nw_v + 2.0 * uv;
    }

    sort_d(buf, n);
    double numerator = kbn_sum_d(buf, n);

    double denom = vertex_dress[u] * vertex_dress[v];
    double dress_uv = (denom > 0.0) ? (numerator / denom) : 0.0;
    edge_dress_next[e] = dress_uv;

    double diff = fabs(edge_dress[e] - dress_uv);
    atomicMaxDouble(d_max_delta, diff);
}

/* ------------------------------------------------------------------ */
/*  Kernel 2a: edge_dress — WITH precomputed intercepts                */
/*                                                                     */
/*  For edge e = (u,v):                                                */
/*    num  = Σ_{k ∈ intercepts(e)} (w_ux·d_ux + w_vx·d_vx)            */
/*         + self_term(variant, w_e, d_e)                              */
/*    edge_dress_next[e] = num / (vertex_dress[u] * vertex_dress[v])       */
/* ------------------------------------------------------------------ */

__global__ void
kernel_edge_dress_intercept(
    const int     item_start,
    const int     item_count,
        const int     variant,           /* dress_variant_t as int */
        const int   * __restrict__ U,
        const int   * __restrict__ V,
        const double* __restrict__ edge_weight,
        const double* __restrict__ edge_dress,
        const double* __restrict__ vertex_dress,
        const double* __restrict__ NW,
        const int   * __restrict__ intercept_offset,
        const int   * __restrict__ intercept_edge_ux,
        const int   * __restrict__ intercept_edge_vx,
        double      * __restrict__ edge_dress_next,
        unsigned long long * __restrict__ d_max_delta,
        const size_t* __restrict__ work_offset,
        double      * __restrict__ d_work)
{
    int tid = blockIdx.x * blockDim.x + threadIdx.x;
    if (tid >= item_count) return;

    int e = item_start + tid;

    int u = U[e], v = V[e];

    double *buf = d_work + work_offset[tid];

    /* Collect numerator terms into workspace for sort+KBN */
    int off = intercept_offset[e];
    int end = intercept_offset[e + 1];
    int n = 0;

    for (int k = off; k < end; k++) {
        int eu = intercept_edge_ux[k];
        int ev = intercept_edge_vx[k];
        buf[n++] = edge_weight[eu] * edge_dress[eu]
                 + edge_weight[ev] * edge_dress[ev];
    }

    edge_finalize(e, u, v, variant, edge_weight, edge_dress,
                  vertex_dress, NW, edge_dress_next, d_max_delta, buf, n);
}

/* ------------------------------------------------------------------ */
/*  Kernel 2b: edge_dress — WITHOUT intercepts (sorted-merge walk)     */
/*                                                                     */
/*  Same formula, but common neighbors are found via a sorted-merge    */
/*  walk on the adj_target arrays — matching the CPU non-intercept     */
/*  code path in dress.c exactly.                                      */
/* ------------------------------------------------------------------ */

__global__ void
kernel_edge_dress_merge(
    const int     item_start,
    const int     item_count,
        const int     variant,
        const int   * __restrict__ U,
        const int   * __restrict__ V,
        const double* __restrict__ edge_weight,
        const double* __restrict__ edge_dress,
        const double* __restrict__ vertex_dress,
        const double* __restrict__ NW,
        const int   * __restrict__ adj_offset,
        const int   * __restrict__ adj_target,
        const int   * __restrict__ adj_edge_idx,
        double      * __restrict__ edge_dress_next,
        unsigned long long * __restrict__ d_max_delta,
        const size_t* __restrict__ work_offset,
        double      * __restrict__ d_work)
{
    int tid = blockIdx.x * blockDim.x + threadIdx.x;
    if (tid >= item_count) return;

    int e = item_start + tid;

    int u = U[e], v = V[e];

    double *buf = d_work + work_offset[tid];
    int n = 0;

    /* Collect numerator terms for sort+KBN */
    int iu     = adj_offset[u], iu_end = adj_offset[u + 1];
    int iv     = adj_offset[v], iv_end = adj_offset[v + 1];

    /* Sorted-merge walk — branchless pointer advance */
    while (iu < iu_end && iv < iv_end) {
        int x = adj_target[iu], y = adj_target[iv];
        if (x == y) {
            int eu = adj_edge_idx[iu];
            int ev = adj_edge_idx[iv];
            buf[n++] = edge_weight[eu] * edge_dress[eu]
                     + edge_weight[ev] * edge_dress[ev];
        }
        iu += (x <= y);
        iv += (x >= y);
    }

    edge_finalize(e, u, v, variant, edge_weight, edge_dress,
                  vertex_dress, NW, edge_dress_next, d_max_delta, buf, n);
}

/* ------------------------------------------------------------------ */
/*  dress_fit_cuda — host-side iteration loop                          */
/* ------------------------------------------------------------------ */

void dress_fit_cuda(p_dress_graph_t g, int max_iterations, double epsilon,
                    int *iterations, double *delta)
{
    const int N = g->N;
    const int E = g->E;
    const int S = g->adj_offset[N];       /* total half-edges */
    const int variant = (int)g->variant;
    const int use_intercepts = g->precompute_intercepts;
    int T = 0;
    if (use_intercepts)
        T = g->intercept_offset[E];       /* total intercept entries */

    /* ── Upload arrays to GPU ──────────────────────────────────── */

    /* CSR adjacency (needed for both kernel paths) */
    int *d_adj_offset, *d_adj_edge_idx;
    CUDA_CHECK(cudaMalloc(&d_adj_offset,   (N + 1) * sizeof(int)));
    CUDA_CHECK(cudaMalloc(&d_adj_edge_idx, S * sizeof(int)));
    CUDA_CHECK(cudaMemcpy(d_adj_offset,   g->adj_offset,
                          (N + 1) * sizeof(int), cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_adj_edge_idx, g->adj_edge_idx,
                          S * sizeof(int), cudaMemcpyHostToDevice));

    /* adj_target — only needed when NOT using intercepts */
    int *d_adj_target = NULL;
    if (!use_intercepts) {
        CUDA_CHECK(cudaMalloc(&d_adj_target, S * sizeof(int)));
        CUDA_CHECK(cudaMemcpy(d_adj_target, g->adj_target,
                              S * sizeof(int), cudaMemcpyHostToDevice));
    }

    /* Intercept arrays — only when using intercepts */
    int *d_intercept_offset = NULL;
    int *d_intercept_edge_ux = NULL;
    int *d_intercept_edge_vx = NULL;
    if (use_intercepts) {
        CUDA_CHECK(cudaMalloc(&d_intercept_offset, (E + 1) * sizeof(int)));
        CUDA_CHECK(cudaMemcpy(d_intercept_offset, g->intercept_offset,
                              (E + 1) * sizeof(int), cudaMemcpyHostToDevice));
        if (T > 0) {
            CUDA_CHECK(cudaMalloc(&d_intercept_edge_ux, T * sizeof(int)));
            CUDA_CHECK(cudaMalloc(&d_intercept_edge_vx, T * sizeof(int)));
            CUDA_CHECK(cudaMemcpy(d_intercept_edge_ux, g->intercept_edge_ux,
                                  T * sizeof(int), cudaMemcpyHostToDevice));
            CUDA_CHECK(cudaMemcpy(d_intercept_edge_vx, g->intercept_edge_vx,
                                  T * sizeof(int), cudaMemcpyHostToDevice));
        }
    }

    /* Edge arrays */
    int    *d_U, *d_V;
    double *d_edge_weight, *d_edge_dress, *d_edge_dress_next;
    CUDA_CHECK(cudaMalloc(&d_U,               E * sizeof(int)));
    CUDA_CHECK(cudaMalloc(&d_V,               E * sizeof(int)));
    CUDA_CHECK(cudaMalloc(&d_edge_weight,     E * sizeof(double)));
    CUDA_CHECK(cudaMalloc(&d_edge_dress,      E * sizeof(double)));
    CUDA_CHECK(cudaMalloc(&d_edge_dress_next, E * sizeof(double)));

    CUDA_CHECK(cudaMemcpy(d_U,           g->U,           E * sizeof(int),    cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_V,           g->V,           E * sizeof(int),    cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_edge_weight, g->edge_weight, E * sizeof(double), cudaMemcpyHostToDevice));
    CUDA_CHECK(cudaMemcpy(d_edge_dress,  g->edge_dress,  E * sizeof(double), cudaMemcpyHostToDevice));

    /* vertex array */
    double *d_node_dress;
    CUDA_CHECK(cudaMalloc(&d_node_dress, N * sizeof(double)));

    /* vertex weights (optional) */
    double *d_NW = NULL;
    if (g->NW != NULL) {
        CUDA_CHECK(cudaMalloc(&d_NW, N * sizeof(double)));
        CUDA_CHECK(cudaMemcpy(d_NW, g->NW, N * sizeof(double), cudaMemcpyHostToDevice));
    }

    /* Convergence scalar */
    unsigned long long *d_max_delta;
    CUDA_CHECK(cudaMalloc(&d_max_delta, sizeof(unsigned long long)));

    /* ── Batched per-thread workspaces for sort+KBN ──────────────── */

    int *node_requirement = (int *)malloc((size_t)N * sizeof(int));
    int *edge_requirement = (int *)malloc((size_t)E * sizeof(int));
    if ((N > 0 && !node_requirement) || (E > 0 && !edge_requirement)) {
        fprintf(stderr, "Host allocation failed while preparing CUDA workspace requirements\n");
        exit(EXIT_FAILURE);
    }

    size_t max_requirement = 1;
    for (int u = 0; u < N; u++) {
        int need = g->adj_offset[u + 1] - g->adj_offset[u] + 1;
        node_requirement[u] = need;
        if ((size_t)need > max_requirement) max_requirement = (size_t)need;
    }

    for (int e = 0; e < E; e++) {
        int need;
        if (use_intercepts) {
            need = g->intercept_offset[e + 1] - g->intercept_offset[e] + 1;
        } else {
            int u = g->U[e];
            int v = g->V[e];
            int deg_u = g->adj_offset[u + 1] - g->adj_offset[u];
            int deg_v = g->adj_offset[v + 1] - g->adj_offset[v];
            need = (deg_u < deg_v ? deg_u : deg_v) + 1;
        }
        edge_requirement[e] = need;
        if ((size_t)need > max_requirement) max_requirement = (size_t)need;
    }

    size_t free_mem = 0, total_mem = 0;
    CUDA_CHECK(cudaMemGetInfo(&free_mem, &total_mem));

    const size_t reserve_bytes = 64ULL * 1024ULL * 1024ULL;
    const size_t offset_bytes = ((size_t)N + (size_t)E) * sizeof(size_t);
    const size_t max_requirement_bytes = max_requirement * sizeof(double);

    size_t remaining_bytes = (free_mem > offset_bytes) ? (free_mem - offset_bytes) : 0;
    size_t workspace_budget_bytes = 0;
    if (remaining_bytes > reserve_bytes) {
        workspace_budget_bytes = ((remaining_bytes - reserve_bytes) * 9) / 10;
    }
    if (workspace_budget_bytes < max_requirement_bytes) {
        if (remaining_bytes < max_requirement_bytes) {
            fprintf(stderr,
                    "CUDA error: insufficient memory for the largest DRESS neighborhood scratch buffer (%zu bytes required, %zu bytes available after metadata)\n",
                    max_requirement_bytes, remaining_bytes);
            exit(EXIT_FAILURE);
        }
        workspace_budget_bytes = max_requirement_bytes;
    }

    size_t workspace_budget_doubles = workspace_budget_bytes / sizeof(double);
    if (workspace_budget_doubles < max_requirement) {
        workspace_budget_doubles = max_requirement;
    }

    work_batch_plan_t node_plan, edge_plan;
    build_work_batch_plan(node_requirement, N, workspace_budget_doubles, &node_plan);
    build_work_batch_plan(edge_requirement, E, workspace_budget_doubles, &edge_plan);
    free(node_requirement);
    free(edge_requirement);

    double *d_work = NULL;
    size_t workspace_doubles = node_plan.max_workspace > edge_plan.max_workspace
                             ? node_plan.max_workspace
                             : edge_plan.max_workspace;
    if (workspace_doubles > 0) {
        CUDA_CHECK(cudaMalloc(&d_work, workspace_doubles * sizeof(double)));
    }

    /* ── Kernel launch config ──────────────────────────────────── */

    const int BLOCK = 256;

    /* ── Iteration loop ────────────────────────────────────────── */

    for (int iter = 0; iter < max_iterations; iter++) {

        /* Phase 1: compute vertex_dress */
        for (int batch = 0; batch < node_plan.batch_count; batch++) {
            int item_start = node_plan.batch_start[batch];
            int item_count = node_plan.batch_size[batch];
            int grid = (item_count + BLOCK - 1) / BLOCK;
            kernel_node_dress<<<grid, BLOCK>>>(
                item_start, item_count,
                d_adj_offset, d_adj_edge_idx,
                d_edge_weight, d_edge_dress,
                d_node_dress,
                d_NW,
                node_plan.d_item_offset + item_start,
                d_work);
        }

        /* Reset max_delta to 0 */
        unsigned long long zero = 0ULL;
        CUDA_CHECK(cudaMemcpy(d_max_delta, &zero,
                              sizeof(zero), cudaMemcpyHostToDevice));

        /* Phase 2: compute edge_dress_next */
        for (int batch = 0; batch < edge_plan.batch_count; batch++) {
            int item_start = edge_plan.batch_start[batch];
            int item_count = edge_plan.batch_size[batch];
            int grid = (item_count + BLOCK - 1) / BLOCK;
            if (use_intercepts) {
                kernel_edge_dress_intercept<<<grid, BLOCK>>>(
                    item_start, item_count, variant,
                    d_U, d_V,
                    d_edge_weight, d_edge_dress, d_node_dress,
                    d_NW,
                    d_intercept_offset,
                    d_intercept_edge_ux, d_intercept_edge_vx,
                    d_edge_dress_next,
                    d_max_delta,
                    edge_plan.d_item_offset + item_start,
                    d_work);
            } else {
                kernel_edge_dress_merge<<<grid, BLOCK>>>(
                    item_start, item_count, variant,
                    d_U, d_V,
                    d_edge_weight, d_edge_dress, d_node_dress,
                    d_NW,
                    d_adj_offset, d_adj_target, d_adj_edge_idx,
                    d_edge_dress_next,
                    d_max_delta,
                    edge_plan.d_item_offset + item_start,
                    d_work);
            }
        }

        /* Phase 3: swap double buffers */
        double *tmp        = d_edge_dress;
        d_edge_dress       = d_edge_dress_next;
        d_edge_dress_next  = tmp;

        /* Phase 4: convergence check */
        unsigned long long max_delta_bits;
        CUDA_CHECK(cudaMemcpy(&max_delta_bits, d_max_delta,
                              sizeof(max_delta_bits), cudaMemcpyDeviceToHost));
        double max_delta;
        memcpy(&max_delta, &max_delta_bits, sizeof(double));

        if (delta) *delta = max_delta;

        if (max_delta < epsilon) {
            if (iterations) *iterations = iter;
            goto cleanup;
        }
    }

    if (iterations) *iterations = max_iterations;

    /* ── Download results + free GPU memory ────────────────────── */

cleanup:
    /* Download edge_dress (from whichever buffer is "current" after swap) */
    CUDA_CHECK(cudaMemcpy(g->edge_dress, d_edge_dress,
                          E * sizeof(double), cudaMemcpyDeviceToHost));

    /* Download vertex_dress as-is from the last Phase 1 computation.
     * This matches CPU fit() behavior exactly: vertex_dress is computed
     * from the edge_dress values BEFORE the final Phase 2 + swap. */
    CUDA_CHECK(cudaMemcpy(g->vertex_dress, d_node_dress,
                          N * sizeof(double), cudaMemcpyDeviceToHost));

    /* Free device memory */
    cudaFree(d_adj_offset);
    cudaFree(d_adj_edge_idx);
    if (d_adj_target)          cudaFree(d_adj_target);
    if (d_intercept_offset)    cudaFree(d_intercept_offset);
    if (d_intercept_edge_ux)   cudaFree(d_intercept_edge_ux);
    if (d_intercept_edge_vx)   cudaFree(d_intercept_edge_vx);
    cudaFree(d_U);
    cudaFree(d_V);
    cudaFree(d_edge_weight);
    cudaFree(d_edge_dress);
    cudaFree(d_edge_dress_next);
    cudaFree(d_node_dress);
    if (d_NW) cudaFree(d_NW);
    cudaFree(d_max_delta);
    if (d_work) cudaFree(d_work);
    free_work_batch_plan(&node_plan);
    free_work_batch_plan(&edge_plan);
}
