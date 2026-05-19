#ifndef DRESS_H
#define DRESS_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// Histogram pair for Δ^k-DRESS results.
// Represents a (value, count) entry in the sparse histogram.
typedef struct __dress_hist_pair_t {
    double  value;
    int64_t count;
} dress_hist_pair_t;

// dress graph variant: determines how adjacency lists are constructed
// from the input edge list.
//
//   UNDIRECTED: N[u] = N(u)           (symmetric neighborhood)
//   DIRECTED:   N[u] = in(u) + out(u) (union of in- and out-neighbors)
//   FORWARD:    N[u] = out(u)         (only outgoing neighbors)
//   BACKWARD:   N[u] = in(u)          (only incoming neighbors)
typedef enum __dress_variant_t {
    DRESS_VARIANT_UNDIRECTED = 0,
    DRESS_VARIANT_DIRECTED   = 1,
    DRESS_VARIANT_FORWARD    = 2,
    DRESS_VARIANT_BACKWARD   = 3
} dress_variant_t;



// dress graph structure.
//
// Stores the augmented variant adjacency in CSR (Compressed Sparse Row)
// format, with per-edge weight and similarity arrays. An optional set of
// precomputed neighborhood intercepts accelerates iterative fitting from
// O(deg_u + deg_v) to O(|N[u] ∩ N[v]|) per edge.
//
// Memory layout (S = total half-edges, T = total intercept entries):
//   Permanent:  12N + 52E + 8T  bytes  (with intercepts)
//               12N + 48E       bytes  (without intercepts)
typedef struct __dress_graph_t {
    dress_variant_t variant;       // graph variant (determines adjacency)
    int      N;                    // number of vertices
    int      E;                    // number of input edges

    int     *U;                    // [E]   input edge sources (owned)
    int     *V;                    // [E]   input edge targets (owned)

    // CSR variant adjacency — sorted per-vertex neighbor lists stored flat.
    // Neighbors of vertex u are adj_target[adj_offset[u] .. adj_offset[u+1]).
    int     *adj_offset;           // [N+1] CSR row offsets
    int     *adj_target;           // [S]   neighbor vertex ids
    int     *adj_edge_idx;         // [S]   maps half-edge to input edge index
    int      max_degree;            // max(deg(u)) over all vertices

    // Per-edge arrays — indexed by edge id 0..E-1.
    double  *W;                    // [E]   raw input weight (NULL when unweighted)
    double  *edge_weight;          // [E]   variant-specific edge weight
    double  *edge_dress;           // [E]   current dress values
    double  *edge_dress_next;      // [E]   next-iteration dress values (double-buffer)

    // Per-vertex arrays — indexed by vertex id 0..N-1.
    double  *NW;                   // [N]   per-vertex weight (NULL → all 1.0)
    double  *vertex_dress;           // [N]   sqrt of weighted dress sum for node

    // Precomputed intercepts (allocated only when precompute_intercepts == 1).
    // For edge e = (u,v), common neighbors are stored at
    // intercept_edge_ux/vx[intercept_offset[e] .. intercept_offset[e+1]).
    int      precompute_intercepts;
    int     *intercept_offset;     // [E+1] CSR-style offsets into intercept arrays
    int     *intercept_edge_ux;    // [T]   edge index for (u,x) per common neighbor x
    int     *intercept_edge_vx;    // [T]   edge index for (v,x) per common neighbor x
} dress_graph_t, *p_dress_graph_t;

// Construct a dress graph from an edge list.
// Takes ownership of U, V, W, NW (all freed by dress_free_graph).
// W may be NULL (unweighted).  NW may be NULL (all vertex weights = 1.0).
p_dress_graph_t dress_init_graph(int N, int E, int *U, int *V,
                                 double *W, double *NW,
                                 dress_variant_t variant,
                                 int precompute_intercepts);

// Run iterative dress fitting for at most max_iterations, stopping early
// when the maximum per-edge change falls below epsilon.
// On return, *iterations and *delta (if non-NULL) hold the iteration
// count and final max delta.
void   dress_fit(p_dress_graph_t g, int max_iterations, double epsilon,
                int *iterations, double *delta);

// Free all memory associated with the dress graph (including U, V).
void   dress_free_graph(p_dress_graph_t g);

// Query the DRESS value for any vertex pair (u, v).
//
// If edge (u,v) exists in the graph, returns its converged edge_dress value.
// If the edge does not exist (virtual edge), estimates what the DRESS value
// *would be* using a local fixed-point iteration against the frozen
// steady-state vertex/edge values.  Useful for link prediction.
//
// The graph must have been fitted (dress_fit) before calling this.
// edge_weight is the hypothetical weight of the virtual edge (ignored
// when the edge already exists).  Pass 1.0 for unweighted graphs.
double dress_get(const p_dress_graph_t g, int u, int v,
                 int max_iterations, double epsilon,
                 double edge_weight);

// ---- Δ^k-DRESS ----------------------------------------------------------

// Compute the Δ^k-DRESS histogram of a graph.
//
// Enumerates all C(N, k) subsets of k vertices, removes each subset
// from the graph, runs DRESS on the resulting subgraph, and accumulates
// every converged edge value into a single histogram.
//
// The deletion subsets are generated by an iterative (stack-based) DFS
// over k-combinations, avoiding recursion.
//
// Parameters:
//   g          - input dress graph (used for topology only: N, E, U, V,
//                variant).  Not modified.
//   k          - deletion depth: number of vertices removed per subset.
//                k = 0 runs DRESS on the original graph (Δ^0).
//   iterations - maximum DRESS iterations per subgraph.
//   epsilon    - convergence tolerance for DRESS and histogram bin width.
//                The histogram has floor(dmax / epsilon) + 1 bins
//                (dmax = 2 for unweighted graphs; larger for weighted).
//   hist_size  - [out] if non-NULL, set to floor(dmax / epsilon) + 1 on return.
//   keep_multisets - if non-zero, allocate and fill a flat matrix of
//                per-subgraph edge DRESS values.
//   multisets  - [out] on return, *multisets points to a heap-allocated
//                flat array of size C(N,k) * E.  Row r, edge e is at
//                (*multisets)[r * E + e].  Removed edges are NAN.
//                The caller must free(*multisets).  Ignored (and set to
//                NULL) when keep_multisets is 0.  May be NULL itself.
//   num_subgraphs - [out] if non-NULL, set to C(N,k) on return.
//
// Returns:
//   A heap-allocated array of dress_hist_pair_t of length *hist_size.
//   Each entry holds a (value, count) pair.  The array is sorted by
//   value.  The caller must free() the returned pointer.
//
// Complexity:
//   O( C(N,k) * iterations * E * d_max )
//   where d_max is the maximum degree.  Each subgraph is independent;
//   the outer loop is embarrassingly parallel (not parallelised here).
dress_hist_pair_t *dress_delta_fit(p_dress_graph_t g, int k, int iterations,
                                   double epsilon,
                                   int n_samples, unsigned int seed,
                                   int *hist_size,
                                   int keep_multisets, double **multisets,
                                   int64_t *num_subgraphs);

// Flat variant — returns packed [bits, count, ...] int64_t array.
// hist_size is set to the total length (2 * distinct_count).
int64_t *dress_delta_fit_flat(p_dress_graph_t g, int k, int iterations,
                              double epsilon,
                              int n_samples, unsigned int seed,
                              int *hist_size,
                              int keep_multisets, double **multisets,
                              int64_t *num_subgraphs);

// Strided variant of dress_delta_fit for distributed computation.
//
// Processes only the subgraphs whose sequential index satisfies
// index % stride == offset.  With offset=0, stride=1 this is
// identical to dress_delta_fit (all subgraphs).
//
// Intended for MPI distribution: each rank calls with
// offset=rank, stride=nprocs, then the per-rank histograms are
// summed via MPI_Allreduce.
dress_hist_pair_t *dress_delta_fit_strided(p_dress_graph_t g, int k, int iterations,
                                           double epsilon,
                                           int n_samples, unsigned int seed,
                                           int *hist_size,
                                           int keep_multisets, double **multisets,
                                           int64_t *num_subgraphs,
                                           int offset, int stride);

// Flat strided variant — returns packed [bits, count, ...] int64_t array.
int64_t *dress_delta_fit_strided_flat(p_dress_graph_t g, int k, int iterations,
                                      double epsilon,
                                      int n_samples, unsigned int seed,
                                      int *hist_size,
                                      int keep_multisets, double **multisets,
                                      int64_t *num_subgraphs,
                                      int offset, int stride);

// ---- ∇^k-DRESS (Nabla-DRESS) -----------------------------------------------

// Compute the ∇^k-DRESS histogram of a graph.
//
// Enumerates all P(N, k) ordered k-tuples of vertices, marks each
// tuple with distinct generic vertex weights (sqrt of successive primes),
// runs DRESS on the resulting weighted graph, and accumulates every
// converged edge value into a single histogram.
//
// Parameters:
//   g          - input dress graph (used for topology: N, E, U, V,
//                variant).  Not modified.
//   k          - individualization depth: number of vertices marked.
//                k = 0 runs DRESS on the unmarked graph (∇^0).
//   iterations - maximum DRESS iterations per marked graph.
//   epsilon    - convergence tolerance for DRESS.
//   n_samples  - if > 0, randomly sample this many tuples instead of
//                enumerating all P(N,k).  0 = full enumeration.
//   seed       - random seed for sampling reproducibility.
//   hist_size  - [out] set to number of distinct histogram entries.
//   keep_multisets - if non-zero, allocate and fill a flat matrix of
//                per-tuple edge DRESS values.
//   multisets  - [out] on return, *multisets points to a heap-allocated
//                flat array of size P(N,k) * E.  Row r, edge e is at
//                (*multisets)[r * E + e].
//                The caller must free(*multisets).
//   num_tuples - [out] if non-NULL, set to P(N,k) on return.
//
// Returns:
//   A heap-allocated array of dress_hist_pair_t of length *hist_size.
//   The caller must free() the returned pointer.
//
// Complexity:
//   O( P(N,k) * iterations * E * d_max )
dress_hist_pair_t *dress_nabla_fit(p_dress_graph_t g, int k, int iterations,
                                   double epsilon,
                                   int n_samples, unsigned int seed,
                                   int *hist_size,
                                   int keep_multisets, double **multisets,
                                   int64_t *num_tuples);

// Flat variant — returns packed [bits, count, ...] int64_t array.
int64_t *dress_nabla_fit_flat(p_dress_graph_t g, int k, int iterations,
                              double epsilon,
                              int n_samples, unsigned int seed,
                              int *hist_size,
                              int keep_multisets, double **multisets,
                              int64_t *num_tuples);

// Strided variant for distributed computation (MPI).
int64_t *dress_nabla_fit_strided_flat(p_dress_graph_t g, int k,
                                      int iterations, double epsilon,
                                      int n_samples, unsigned int seed,
                                      int *hist_size,
                                      int keep_multisets, double **multisets,
                                      int64_t *num_tuples,
                                      int offset, int stride);

#ifdef __cplusplus
}
#endif

#endif // DRESS_H
