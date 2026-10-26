// utils_gabow_tarjan.cpp
// Module A: Cost-length & 1-feasibility utilities for Gabow-Tarjan algorithm
// PATCHED VERSION with diagnostic error reporting

#include "utils_gabow_tarjan.h"
#include <Rcpp.h>
#include "../core/lap_utils_rcpp.h"
#include <set>
#include <utility>
#include <queue>
#include <map>
#include <algorithm>
#include <functional>
#include <limits>
// stdexcept removed - use LAP_ERROR() instead
// #include <iostream>  // Removed for CRAN compliance (no std::cerr allowed)

#ifdef COUPLR_GT_DEBUG
#define DEBUG_ASSERT_FEASIBLE(cost, cm, yu, yv, msg)                           \
    do {                                                                        \
        if (!check_one_feasible((cost), (cm), (yu), (yv))) {                    \
            LAP_ERROR(msg);                                                     \
        }                                                                       \
    } while (0)
#else
#define DEBUG_ASSERT_FEASIBLE(cost, cm, yu, yv, msg) ((void)0)
#endif

// ---------------------------------------------------------------------------
// Optional profiling instrumentation. Enable by building with
//   PKG_CPPFLAGS += -DCOUPLR_GT_PROFILE
// and the package will print a per-solve breakdown to stdout. Release builds
// compile every PROF_* call to a no-op. Counters are process-scoped (not
// thread-local); the R single-threaded execution model makes that fine.
// ---------------------------------------------------------------------------
#ifdef COUPLR_GT_PROFILE
#include <chrono>
namespace {
struct GtProfile {
    long long total_ns           = 0;
    long long step1_ns           = 0;
    long long step2_ns           = 0;
    long long enqueue_ns         = 0;
    long long bucket_loop_ns     = 0;
    long long eq_build_ns        = 0;
    long long eq_update_ns       = 0;
    long long step1_calls        = 0;
    long long step1_paths        = 0;
    long long step2_calls        = 0;
    long long step2_enqueue_edges= 0;
    long long step2_bucket_pops  = 0;
    long long eq_builds          = 0;
    long long eq_updates         = 0;
    long long match_gt_calls     = 0;
    long long scale_match_calls  = 0;

    void reset() { *this = GtProfile{}; }
    void print() const {
        auto ms = [](long long ns) { return ns / 1e6; };
        Rcpp::Rcout << "[GT_PROFILE]"
                    << "  total=" << ms(total_ns) << " ms"
                    << "  step1=" << ms(step1_ns) << " (" << step1_calls
                    << " calls, " << step1_paths << " paths)"
                    << "  step2=" << ms(step2_ns) << " (" << step2_calls
                    << " calls)"
                    << "    enqueue=" << ms(enqueue_ns) << " ("
                    << step2_enqueue_edges << " edges)"
                    << "    bucket=" << ms(bucket_loop_ns) << " ("
                    << step2_bucket_pops << " pops)"
                    << "  eq_build=" << ms(eq_build_ns) << " ("
                    << eq_builds << ")"
                    << "  eq_update=" << ms(eq_update_ns) << " ("
                    << eq_updates << ")"
                    << "  match_gt=" << match_gt_calls
                    << "  scale_match=" << scale_match_calls
                    << "\n";
    }
};
inline GtProfile& gt_prof() {
    static GtProfile p;
    return p;
}
struct PrTimer {
    long long& target;
    std::chrono::steady_clock::time_point t0;
    explicit PrTimer(long long& tgt)
        : target(tgt), t0(std::chrono::steady_clock::now()) {}
    ~PrTimer() {
        auto t1 = std::chrono::steady_clock::now();
        target += std::chrono::duration_cast<std::chrono::nanoseconds>(t1 - t0)
                      .count();
    }
};
}  // namespace
#define PROF_TIMER(field) PrTimer pr_timer_##field(gt_prof().field)
#define PROF_INC(field, by) (gt_prof().field += (by))
#define PROF_RESET() (gt_prof().reset())
#define PROF_PRINT() (gt_prof().print())
#else
#define PROF_TIMER(field) ((void)0)
#define PROF_INC(field, by) ((void)0)
#define PROF_RESET() ((void)0)
#define PROF_PRINT() ((void)0)
#endif

// Step 2 bucket-array silent drop is built into Gabow & Tarjan 1989.
// Page 9 of the paper, on the Q[1..bn] array (b = a + 2 = 5 in the paper's
// scaling notation): "an edge vw with v in V0 cap F, w not in F does not
// get entered in this data structure if cl(vw) - y(v) - y(w) + A(v) > bn."
// Inequality (3) on p.7 proves Delta cannot reach a value larger than bn
// within one Hungarian search, so any edge with r > bn is unreachable in
// this search by construction; not entering it is correct, not a wart.
//
// Phase 5's earlier fuzz (1000 instances, n in [3,200], max_cost up to 1e9)
// measured ~86% drop rate with 0 cost mismatches vs JV. That observation
// is consistent with the paper -- the drops are unreachable edges that
// become eligible in later bit-scaling phases. dev_notes/phase5_fuzz.R
// stays as the regression check for "drops never cause cost mismatch".
//
// Phase 7's bucket_bound = max(1, 6*n + 2) is slightly looser than the
// paper's bn = 5n; the extra n + 2 is a conservative safety margin and
// still drops only edges the paper would also drop.

namespace {

// O(m) check: is every column unmatched?
bool gt_matching_is_empty(const MatchVec& col_match) {
    for (int i : col_match) {
        if (i != NIL) return false;
    }
    return true;
}

// Canonical 1-feasible dual init for an empty matching:
//   y_u[i] = 0
//   y_v[j] = min over rows of (c(i, j) + 1) for finite edges, or 0 if all forbidden
//
// This is the tightest 1-feasible setting when no edge is matched. It is the
// only setup that is robust to costs with c < -1 (where the zero duals fail
// the upper bound y_u + y_v <= c + 1). Called once at the start of match_gt
// / hungarian_step_one_feasible whenever the matching arrives empty; caller-
// supplied duals are overwritten because there is no matched edge whose value
// they would be carrying. Cost: O(n m), runs once per entry, not per Step 2.
void gt_init_empty_duals(const CostMatrix& cost,
                         DualVec& y_u,
                         DualVec& y_v) {
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;
    for (int i = 0; i < n; ++i) {
        y_u[i] = 0;
    }
    for (int j = 0; j < m; ++j) {
        long long min_val = BIG_INT;
        for (int i = 0; i < n; ++i) {
            long long c_ij = cost[i][j];
            if (c_ij < BIG_INT) {
                long long val = c_ij + 1;
                if (val < min_val) min_val = val;
            }
        }
        y_v[j] = (min_val < BIG_INT) ? min_val : 0;
    }
}

}  // namespace

// ============================================================================
// Row capacity
// ============================================================================

std::vector<int> row_degrees(const MatchVec& col_match, int n_rows) {
    std::vector<int> deg(n_rows > 0 ? static_cast<size_t>(n_rows) : 0, 0);
    for (int i : col_match) {
        if (i >= 0 && i < n_rows) ++deg[static_cast<size_t>(i)];
    }
    return deg;
}

// ============================================================================
// Module A: Cost-length & 1-feasibility utilities
// ============================================================================

/**
 * Compute cost-length cl(e) for edge with cost c_ij
 * 
 * @param c_ij Edge cost
 * @param in_matching Whether edge is in the matching
 * @return cl(e) = c(e) if in matching, c(e) + 1 otherwise
 */
long long cost_length(long long c_ij, bool in_matching) {
    return in_matching ? c_ij : (c_ij + 1);
}

/**
 * Check if an edge is eligible (tight in the 1-feasibility constraint)
 * 
 * @param c_ij Edge cost
 * @param in_matching Whether edge is in the matching
 * @param yu Dual variable for row vertex
 * @param yv Dual variable for column vertex
 * @return true if yu + yv == cl(e)
 */
bool is_eligible(long long c_ij, bool in_matching, 
                 long long yu, long long yv) {
    return yu + yv == cost_length(c_ij, in_matching);
}

/**
 * Check 1-feasibility conditions for a matching and duals
 * 
 * Verifies:
 * 1. For all finite edges (i,j): y_u[i] + y_v[j] <= c(i,j) + 1
 * 2. For matched edges (i,j): y_u[i] + y_v[j] >= c(i,j)
 * 
 * @param cost Cost matrix (BIG_INT indicates forbidden edge)
 * @param col_match Matching from columns (col_match[j] = i or NIL)
 * @param y_u Dual variables for rows
 * @param y_v Dual variables for columns
 * @return true if all 1-feasibility conditions satisfied
 */
bool check_one_feasible(const CostMatrix& cost,
                        const MatchVec& col_match,
                        const DualVec& y_u,
                        const DualVec& y_v) {
    int n = static_cast<int>(cost.size());
    if (n == 0) return true;
    
    int m = static_cast<int>(cost[0].size());
    
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            long long c_ij = cost[i][j];
            
            // Skip forbidden edges
            if (c_ij >= BIG_INT) continue;
            
            long long sum_duals = y_u[i] + y_v[j];
            bool matched = (col_match[j] == i);
            
            // Condition 1: y_u[i] + y_v[j] <= c(i,j) + 1
            if (sum_duals > c_ij + 1) {
                return false;
            }
            
            // Condition 2: for matched edges, y_u[i] + y_v[j] >= c(i,j)
            if (matched && sum_duals < c_ij) {
                return false;
            }
        }
    }
    
    return true;
}

// ============================================================================
// Module B: Equality graph construction
// ============================================================================

/**
 * Build equality graph (eligible edges) as adjacency lists
 * 
 * For each row i, returns list of columns j where edge (i,j) is eligible,
 * meaning y_u[i] + y_v[j] == cl(i,j)
 * 
 * @param cost Cost matrix (BIG_INT indicates forbidden edge)
 * @param col_match Matching from columns (col_match[j] = i or NIL)
 * @param y_u Dual variables for rows
 * @param y_v Dual variables for columns
 * @return eq_graph[i] = list of eligible column indices for row i
 */
std::vector<std::vector<int>>
build_equality_graph(const CostMatrix& cost,
                     const MatchVec& col_match,
                     const DualVec& y_u,
                     const DualVec& y_v)
{
    PROF_TIMER(eq_build_ns);
    PROF_INC(eq_builds, 1);
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;

    std::vector<std::vector<int>> eq_graph(n);

    for (int i = 0; i < n; ++i) {
        eq_graph[i].clear();
        for (int j = 0; j < m; ++j) {
            long long c_ij = cost[i][j];
            
            // Skip forbidden edges
            if (c_ij >= BIG_INT) {
                continue;
            }

            bool in_matching = (col_match[j] == i);
            if (is_eligible(c_ij, in_matching, y_u[i], y_v[j])) {
                eq_graph[i].push_back(j);
            }
        }
    }
    
    return eq_graph;
}

/**
 * Incrementally update equality graph after dual variable changes
 *
 * Handles two callers:
 *   - Step 1 augment + col-decrement: only y_v on path cols changes. Pass
 *     affected_rows = {} and affected_cols = path cols. Each existing eligible
 *     edge to an affected col is rechecked; no new edges become eligible
 *     because y_v only decreases (and for rows not on the path, cl is
 *     unchanged), so the routine performs deletions only on those columns.
 *   - Step 2 Hungarian search: y_u changes on rows in S, y_v on cols in T,
 *     and row_match changes along the augmenting path (all path rows in S,
 *     all path cols in T). For rows in S, every edge (i, *) may have flipped
 *     eligibility, so eq_graph[i] is rebuilt from scratch over all m cols.
 *     For rows not in S, cl(i, j) is unchanged for every j (row_match[i] is
 *     untouched, and col_match[j] only moves between rows in S, never to a
 *     row outside S), so it suffices to prune edges to cols in T whose
 *     y_v dropped, same as the Step 1 case.
 *
 * @param eq_graph Existing equality graph to update (modified in place)
 * @param cost Cost matrix
 * @param col_match Current matching, read from the column side
 * @param y_u Dual variables for rows
 * @param y_v Dual variables for columns
 * @param affected_rows Row indices whose y_u changed (or whose row_match
 *                      changed). Their adjacency lists are rebuilt entirely.
 * @param affected_cols Column indices whose y_v decreased. For non-affected
 *                      rows, edges to these cols are pruned if no longer
 *                      eligible.
 */
void update_equality_graph_incremental(std::vector<std::vector<int>>& eq_graph,
                                        const CostMatrix& cost,
                                        const MatchVec& col_match,
                                        const DualVec& y_u,
                                        const DualVec& y_v,
                                        const std::vector<int>& affected_rows,
                                        const std::vector<int>& affected_cols)
{
    PROF_TIMER(eq_update_ns);
    PROF_INC(eq_updates, 1);
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;

    if (affected_rows.empty() && affected_cols.empty()) return;

    std::vector<bool> in_affected_rows(n, false);
    for (int i : affected_rows) {
        if (i >= 0 && i < n) in_affected_rows[i] = true;
    }

    // Full rebuild for rows whose y_u (and possibly row_match) changed.
    for (int i : affected_rows) {
        if (i < 0 || i >= n) continue;
        auto& adj_list = eq_graph[i];
        adj_list.clear();
        for (int j = 0; j < m; ++j) {
            long long c_ij = cost[i][j];
            if (c_ij >= BIG_INT) continue;
            bool in_matching = (col_match[j] == i);
            if (is_eligible(c_ij, in_matching, y_u[i], y_v[j])) {
                adj_list.push_back(j);
            }
        }
    }

    if (affected_cols.empty()) return;

    std::vector<bool> affected_col_mask(m, false);
    for (int j : affected_cols) {
        if (j >= 0 && j < m) affected_col_mask[j] = true;
    }

    // Prune eligible edges to affected cols for rows that were not rebuilt.
    for (int i = 0; i < n; ++i) {
        if (in_affected_rows[i]) continue;
        auto& adj_list = eq_graph[i];
        adj_list.erase(
            std::remove_if(adj_list.begin(), adj_list.end(),
                           [&](int j) {
                               if (j < 0 || j >= m || !affected_col_mask[j]) {
                                   return false;
                               }
                               long long c_ij = cost[i][j];
                               if (c_ij >= BIG_INT) {
                                   return true;
                               }
                               bool in_matching = (col_match[j] == i);
                               return !is_eligible(c_ij, in_matching, y_u[i], y_v[j]);
                           }),
            adj_list.end());
    }
}

// ============================================================================
// Module C: Augment matching along a path
// ============================================================================

/**
 * Apply symmetric difference of current matching M with edge set P (path edges)
 * 
 * Given an augmenting path represented as a list of edges, updates the matching
 * by taking M' = M Δ P (symmetric difference). This flips matched/unmatched
 * status of all edges in the path.
 * 
 * @param edges List of (row, col) pairs forming the augmenting path
 * @param row_match Matching from rows (modified in place)
 * @param col_match Matching from columns (modified in place)
 */
void augment_along_path(const std::vector<std::pair<int,int>>& edges,
                        MatchVec& row_match,
                        MatchVec& col_match,
                        int last_row_cap)
{
    // OPTIMIZED: Direct in-place flip - O(path_length) instead of O(n)
    // Paper (page 6): "Augmenting along P means M ← M ⊕ P"
    // This is just flipping matched/unmatched status along the path
    //
    // A column the path takes is released by whoever held it, which the
    // overwrite of col_match does on its own. row_match follows for the
    // ordinary rows; a capacitated row holds several columns and none of them
    // would stand for the others, so it keeps NIL there.
    const int cap_row = capacitated_row(static_cast<int>(row_match.size()),
                                        last_row_cap);

    for (const auto& edge : edges) {
        int i = edge.first;
        int j = edge.second;
        if (i != cap_row) {
            row_match[i] = j;
        }
        col_match[j] = i;
    }
}

// ============================================================================
// Module D: Maximal set of augmenting paths on equality graph
// ============================================================================

/**
 * Find a maximal set of vertex-disjoint shortest augmenting paths in the
 * equality graph (Hopcroft-Karp style).
 *
 * Two-phase Hopcroft-Karp structure:
 *   1. BFS from all free rows assigns levels. Rows at even levels, cols at
 *      odd levels. Cols reach via eligible edges; rows reach via matched
 *      edges (col_match[j] -> next_row at col_level + 1). BFS stops one
 *      layer past the first free col discovered, so every leveled vertex
 *      lies on a shortest augmenting path.
 *   2. DFS from free rows along level-increasing edges only, using
 *      visited_col + next_edge[i] to keep total DFS work O(E) per call.
 *      Each row that fails to find a path has its level demoted (`row_level
 *      = INF`) so later DFS roots don't redescend it.
 *
 * Restricting Step 1 to shortest paths bounds the number of times match_gt
 * loops Step 1 / Step 2 per scaling phase — the paper's O(sqrt(n)) HK round
 * count, modulo dual changes from Step 2 reshaping the eligibility graph.
 *
 * A capacitated row starts as many paths as it has slots left, which is what
 * its dummy copies would each have started in the padded square. It carries
 * one BFS level for all of them, so it is a root while a slot is free and an
 * interior vertex only once it is full.
 *
 * @param eq_graph Equality graph (adjacency lists of eligible edges)
 * @param col_match Current matching from columns
 * @param last_row_cap Columns the last row may hold at once
 * @return List of paths, where each path is a list of edges
 */
std::vector<std::vector<std::pair<int,int>>>
find_maximal_augmenting_paths(const std::vector<std::vector<int>>& eq_graph,
                              const MatchVec& col_match,
                              int last_row_cap)
{
    const int n = static_cast<int>(eq_graph.size());
    const int m = static_cast<int>(col_match.size());

    constexpr int INF_LEVEL = std::numeric_limits<int>::max();
    std::vector<int> row_level(n, INF_LEVEL);
    std::vector<int> col_level(m, INF_LEVEL);

    std::vector<int> row_deg = row_degrees(col_match, n);
    auto free_slots = [&](int i) {
        return row_capacity(i, n, last_row_cap) - row_deg[i];
    };

    // ---- Phase 1: BFS to assign levels. ------------------------------------
    std::queue<int> bfs_q;
    for (int i = 0; i < n; ++i) {
        if (free_slots(i) > 0) {
            row_level[i] = 0;
            bfs_q.push(i);
        }
    }

    int free_col_level = INF_LEVEL;  // Shortest augmenting-path length (cols).
    while (!bfs_q.empty()) {
        int i = bfs_q.front();
        bfs_q.pop();

        // Don't expand beyond the shortest free-col layer.
        if (row_level[i] >= free_col_level) continue;

        for (int j : eq_graph[i]) {
            if (j < 0 || j >= m) continue;
            if (col_level[j] != INF_LEVEL) continue;

            col_level[j] = row_level[i] + 1;
            if (col_match[j] == NIL) {
                // Free col reached. Record the layer and keep draining the
                // queue at this level so all shortest paths are discoverable.
                if (free_col_level == INF_LEVEL) {
                    free_col_level = col_level[j];
                }
            } else {
                int next_row = col_match[j];
                if (next_row >= 0 && next_row < n &&
                    row_level[next_row] == INF_LEVEL) {
                    row_level[next_row] = col_level[j] + 1;
                    bfs_q.push(next_row);
                }
            }
        }
    }

    std::vector<std::vector<std::pair<int,int>>> all_paths;
    if (free_col_level == INF_LEVEL) {
        // No augmenting path exists at this point in the eligibility graph.
        return all_paths;
    }

    // ---- Phase 2: DFS along level-increasing edges. -----------------------
    std::vector<bool> visited_col(m, false);
    std::vector<size_t> next_edge(n, 0);

    // One descent from a free root. Returns true when it reached a free column
    // and recorded the path. next_edge and the demotions persist across calls,
    // which is what keeps the whole phase O(E).
    auto descend_from = [&](int root) -> bool {
        std::vector<int> path_rows;
        std::vector<std::pair<int,int>> path_edges;
        path_rows.push_back(root);
        bool found = false;

        while (!path_rows.empty()) {
            int i = path_rows.back();
            bool advanced = false;

            while (next_edge[i] < eq_graph[i].size()) {
                int j = eq_graph[i][next_edge[i]++];
                if (j < 0 || j >= m) continue;
                // Level filter: only follow edges into the next layer.
                if (col_level[j] != row_level[i] + 1) continue;
                if (visited_col[j]) continue;

                visited_col[j] = true;
                path_edges.emplace_back(i, j);

                if (col_match[j] == NIL) {
                    all_paths.push_back(path_edges);
                    path_rows.clear();
                    path_edges.clear();
                    advanced = true;
                    found = true;
                    break;
                }

                int next_row = col_match[j];
                if (next_row >= 0 && next_row < n &&
                    row_level[next_row] == col_level[j] + 1) {
                    path_rows.push_back(next_row);
                    advanced = true;
                    break;
                }

                // Matched edge doesn't ascend to the right level (target row
                // either past the path-length cap or already exhausted by a
                // prior failed DFS). Back out and try the next edge.
                path_edges.pop_back();
            }

            if (advanced) {
                if (found) break;
                continue;
            }

            // No further progress from row i. Demote it so later DFS roots
            // don't redescend through here.
            row_level[i] = INF_LEVEL;
            if (path_edges.empty()) break;
            path_edges.pop_back();
            path_rows.pop_back();
        }

        return found;
    };

    for (int root = 0; root < n; ++root) {
        if (row_level[root] != 0) continue;

        // A capacitated root stands for that many free copies of itself, and
        // the copies would have been separate roots in the padded square.
        for (int slot = free_slots(root); slot > 0; --slot) {
            if (row_level[root] != 0) break;  // demoted by a failed descent
            if (!descend_from(root)) break;
        }
    }

    return all_paths;
}

// ============================================================================
// Module E: Hungarian-style search on cost-length (Step 2 core)
// ============================================================================

/**
 * Hungarian/Dijkstra search on cost-length (computed inline from cost + matching).
 *
 * Performs a Hungarian search to find an augmenting path, maintaining
 * 1-feasibility. Updates duals and matching if a path is found.
 *
 * cl(i, j) is derived on the fly:
 *   cl(i, j) = cost[i][j] + 1   if (i, j) is not in the matching
 *   cl(i, j) = cost[i][j]       if (i, j) is in the matching
 * Forbidden edges (cost[i][j] >= BIG_INT) are skipped.
 *
 * @param cost Cost matrix (BIG_INT for forbidden edges)
 * @param row_match Current matching from rows (modified in place)
 * @param col_match Current matching from columns (modified in place)
 * @param y_u Dual variables for rows (modified in place)
 * @param y_v Dual variables for columns (modified in place)
 * @return true if augmenting path found and applied, false otherwise
 */
bool hungarian_search_cl(const CostMatrix& cost,
                         MatchVec& row_match,
                         MatchVec& col_match,
                         DualVec& y_u,
                         DualVec& y_v,
                         std::vector<int>* affected_rows_out,
                         std::vector<int>* affected_cols_out,
                         int last_row_cap)
{
    PROF_TIMER(step2_ns);
    PROF_INC(step2_calls, 1);
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;
    if (n == 0 || m == 0) return false;
    if (affected_rows_out) affected_rows_out->clear();
    if (affected_cols_out) affected_cols_out->clear();

    const int cap_row = capacitated_row(n, last_row_cap);

    // Inline cost-length helper: cl(i, j) for a known cost[i][j] value.
    // Caller must have already checked c_ij < BIG_INT.
    auto cl_of = [&](int i, int j, long long c_ij) -> long long {
        return (col_match[j] == i) ? c_ij : (c_ij + 1);
    };

    // Invariant: every matched edge entering Step 2 is tight, i.e. y_u[i] +
    // y_v[j] == cost[i][j]. Step 1's augment+col-decrement maintains this; the
    // augment loop below maintains it across Step 2 by decrementing y_v on
    // every newly-matched col, mirroring Step 1. Under COUPLR_GT_DEBUG we
    // assert it; in release we rely on it (the old defensive y_u -= 1 fix-up
    // would have masked stale eq_graph entries on rows that PRE-STEPed but did
    // not enter S, breaking the Phase 2 incremental update).
#ifdef COUPLR_GT_DEBUG
    for (int j = 0; j < m; ++j) {
        int i = col_match[j];
        if (i == NIL || i < 0 || i >= n) continue;
        long long c_ij = cost[i][j];
        if (c_ij >= BIG_INT) continue;
        if (y_u[i] + y_v[j] != c_ij) {
            LAP_ERROR("Matched edge not tight entering hungarian_search_cl "
                      "(y_u[i] + y_v[j] != cost[i][j])");
        }
    }
#endif

    // ---------------------------------------------------------------------
    // Paper Step 2: Hungarian forest with lazy dual offset A and bucket array Q.
    // When a row v enters the forest, save y(v) and A(v). An edge vw with
    // w outside the forest becomes eligible when
    //   A = cl(vw) - y(v) - y(w) + A(v).
    // Buckets Q[r] store exactly those edge candidates.
    // ---------------------------------------------------------------------

    // Phase 7 (flat bucket): the bucket structure used to be
    //   std::vector<std::vector<BucketEdge>> Q[bucket_bound+1]
    // i.e. ~1500 separate inner vectors, each with its own heap allocation,
    // pushed to in random-by-r order. Profile (dev_notes/phase7_profile.R)
    // showed ~57% of total time was in enqueue_edges_from_row's push_back.
    // Flat-bucket: one contiguous arena `entries` plus an intrusive linked
    // list keyed by r. bucket_head[r] is the index in `entries` of the most
    // recently pushed edge for r; each entry's `next` is the index of the
    // prior push (or -1). Push = append to entries + relink bucket_head[r].
    // Pop = follow bucket_head[r] into entries. Eliminates per-bucket heap
    // allocations; A/B over n in {64..384} measured 11-19% speedup on dense.
    struct BucketEdge {
        int row;
        int col;
        int next;  // index in `entries` of prior push for same r, or -1
    };

    // The bound scales with the size of a saturating matching, which is one
    // edge per column, not one per row: a capacitated row carries as many
    // edges as it has slots. The array holding the buckets grows to the
    // largest r a search actually reaches, which on a wide instance is far
    // short of the bound.
    const long long bucket_bound = std::max(1, 6 * m + 2);
    std::vector<int> bucket_head(
        static_cast<size_t>(std::min<long long>(bucket_bound + 1, 1024)), -1);
    std::vector<BucketEdge> entries;
    // Reserve a starting capacity that holds a moderate Step 2 without
    // realloc. Anything larger grows by std::vector's normal doubling, which
    // is amortized O(1) and trivial in absolute cost (memcpy of an entire
    // 13.5K-entry arena is ~30 microseconds).
    entries.reserve(static_cast<size_t>(std::max(64, n + m)));

    long long A = 0;
    long long next_bucket = 0;
    bool has_free_root = false;

    std::vector<bool> in_S(n, false);
    std::vector<bool> in_T(m, false);
    std::vector<int> parent_row(n, NIL);
    std::vector<int> reached_by_row(m, NIL);

    std::vector<long long> saved_y_u(n, 0);
    std::vector<long long> saved_y_v(m, 0);
    std::vector<long long> enter_A_u(n, 0);
    std::vector<long long> enter_A_v(m, 0);
    std::vector<int> entered_by_col(n, NIL);

    // Append to the arena; link the new entry as the new head of bucket r.
    auto bucket_push = [&](long long r, int i, int j) {
        if (r > bucket_bound) {
            // Paper p.9: edges with r > bn are not entered into Q.
            // Inequality (3) bounds Delta within one Hungarian search by
            // bn, so any r > bn edge is unreachable in this search.
            return;
        }
        if (r >= static_cast<long long>(bucket_head.size())) {
            bucket_head.resize(static_cast<size_t>(r) + 1, -1);
        }
        int idx = static_cast<int>(entries.size());
        entries.push_back({i, j, bucket_head[static_cast<size_t>(r)]});
        bucket_head[static_cast<size_t>(r)] = idx;
    };

    auto enqueue_edges_from_row = [&](int i) {
        PROF_TIMER(enqueue_ns);
        for (int j = 0; j < m; ++j) {
            if (in_T[j]) {
                continue;
            }
            long long c_ij = cost[i][j];
            if (c_ij >= BIG_INT) {
                continue;
            }
            long long ccl = cl_of(i, j, c_ij);

            long long r = ccl - saved_y_u[i] - y_v[j] + enter_A_u[i];
            if (r < A) {
                r = A;
            }
            bucket_push(r, i, j);
            PROF_INC(step2_enqueue_edges, 1);
        }
    };

    auto materialize_forest_duals = [&]() {
        for (int i = 0; i < n; ++i) {
            if (in_S[i]) {
                y_u[i] = saved_y_u[i] + (A - enter_A_u[i]);
            }
        }
        for (int j = 0; j < m; ++j) {
            if (in_T[j]) {
                y_v[j] = saved_y_v[j] - (A - enter_A_v[j]);
            }
        }
    };

    {
        std::vector<int> row_deg = row_degrees(col_match, n);
        for (int i = 0; i < n; ++i) {
            if (row_deg[i] < row_capacity(i, n, last_row_cap)) {
                has_free_root = true;
                in_S[i] = true;
                parent_row[i] = NIL;
                entered_by_col[i] = NIL;
                saved_y_u[i] = y_u[i];
                enter_A_u[i] = A;
                enqueue_edges_from_row(i);
            }
        }
    }

    if (!has_free_root) {
        return false;
    }

    while (true) {
        while (true) {
            while (next_bucket < static_cast<long long>(bucket_head.size()) &&
                   bucket_head[static_cast<size_t>(next_bucket)] == -1) {
                ++next_bucket;
            }
            if (next_bucket >= static_cast<long long>(bucket_head.size())) {
                return false;
            }

            A = next_bucket;
            int head_idx = bucket_head[static_cast<size_t>(next_bucket)];
            BucketEdge popped = entries[static_cast<size_t>(head_idx)];
            bucket_head[static_cast<size_t>(next_bucket)] = popped.next;
            int i = popped.row;
            int j = popped.col;
            PROF_INC(step2_bucket_pops, 1);

            if (i < 0 || i >= n || j < 0 || j >= m || !in_S[i] || in_T[j]) {
                continue;
            }
            long long c_ij = cost[i][j];
            if (c_ij >= BIG_INT) {
                continue;
            }
            long long ccl = cl_of(i, j, c_ij);

            long long current_y_i = saved_y_u[i] + (A - enter_A_u[i]);
            long long reduced = ccl - current_y_i - y_v[j];
            if (reduced > 0) {
                bucket_push(A + reduced, i, j);
                continue;
            }

            in_T[j] = true;
            reached_by_row[j] = i;
            saved_y_v[j] = y_v[j];
            enter_A_v[j] = A;

            if (col_match[j] == NIL) {
                materialize_forest_duals();

                // Augment along the path back to the free row. For every
                // newly-matched col, decrement y_v by 1: materialize leaves
                // sum(path edge) = cl_pre = c + 1 for an edge that was
                // unmatched before, but cl_new = c after augment. Decrementing
                // y_v by 1 restores tightness (sum = c) on the matched edge,
                // and preserves 1-feasibility on every other edge incident
                // to the same col (those are all unmatched, so the upper
                // bound y_u + y_v <= c + 1 can only get easier when y_v
                // drops). Same pattern as Step 1's apply_step1 y_v -= 1 pass.
                int col = j;
                int row = reached_by_row[col];
                while (row != NIL) {
                    // The column that admitted this row into the forest is the
                    // one it gives up; a root gave up nothing and ends the walk.
                    int prev_col = entered_by_col[row];
                    int prev_row = parent_row[row];
                    if (row != cap_row) {
                        row_match[row] = col;
                    }
                    col_match[col] = row;
                    y_v[col] -= 1;
                    if (prev_col == NIL) {
                        break;
                    }
                    col = prev_col;
                    row = prev_row;
                }

                // Emit S and T for the caller's incremental equality-graph
                // update. Every row in S had y_u changed; every col in T had
                // y_v changed. Path rows/cols are subsets of S/T. The free
                // col we just augmented to was marked in_T just above.
                if (affected_rows_out) {
                    for (int ii = 0; ii < n; ++ii) {
                        if (in_S[ii]) affected_rows_out->push_back(ii);
                    }
                }
                if (affected_cols_out) {
                    for (int jj = 0; jj < m; ++jj) {
                        if (in_T[jj]) affected_cols_out->push_back(jj);
                    }
                }
                return true;
            }

            // A capacitated row can be reached through several of its columns.
            // It joins the forest through the first of them: it entered at the
            // smallest A, so its edges are already in the buckets at the
            // earliest r any later entry could give them.
            int next_row = col_match[j];
            if (next_row != NIL && !in_S[next_row]) {
                in_S[next_row] = true;
                parent_row[next_row] = i;
                entered_by_col[next_row] = j;
                saved_y_u[next_row] = y_u[next_row];
                enter_A_u[next_row] = A;
                enqueue_edges_from_row(next_row);
            }

            break;
        }
    }
}
bool hungarian_step_one_feasible(const CostMatrix& cost,
                                 MatchVec& row_match,
                                 MatchVec& col_match,
                                 DualVec& y_u,
                                 DualVec& y_v,
                                 std::vector<int>* affected_rows_out,
                                 std::vector<int>* affected_cols_out,
                                 int last_row_cap)
{
    const int n = static_cast<int>(cost.size());
    if (n == 0) return false;
    const int m = static_cast<int>(cost[0].size());
    if (m == 0) return false;

    // Empty-matching entry path: caller's duals carry no matched-edge value;
    // initialize to the canonical 1-feasible y_u = 0, y_v = min_i(c + 1). This
    // is the only init step kept from the old defensive block; it costs O(nm)
    // once and is required for correctness when c contains values < -1. The
    // 20-iter dual-repair loop for non-empty matchings is gone — caller is
    // expected to deliver a 1-feasible state.
    const bool was_empty = gt_matching_is_empty(col_match);
    if (was_empty) {
        gt_init_empty_duals(cost, y_u, y_v);
    }

    DEBUG_ASSERT_FEASIBLE(cost, col_match, y_u, y_v,
        "1-feasibility violated entering hungarian_step_one_feasible");

    bool ok = hungarian_search_cl(cost, row_match, col_match, y_u, y_v,
                                  affected_rows_out, affected_cols_out,
                                  last_row_cap);

    // gt_init_empty_duals rewrote every y_v, so the equality graph must be
    // rebuilt for every row regardless of which rows entered S. Signal this
    // to the caller by populating affected_rows_out with all rows.
    if (ok && was_empty && affected_rows_out) {
        affected_rows_out->clear();
        affected_rows_out->reserve(static_cast<size_t>(n));
        for (int i = 0; i < n; ++i) affected_rows_out->push_back(i);
    }

    return ok;
}

// ============================================================================
// Module F: match_gt - Inner Gabow-Tarjan matching algorithm
// ============================================================================

/**
 * Check if the matching saturates every row
 *
 * A row holds one column, or last_row_cap of them when it is the capacitated
 * one; the matching is perfect once every row holds as many as it can.
 *
 * @param col_match Current matching from columns
 * @param n_rows Rows in the instance
 * @param last_row_cap Columns the last row may hold at once
 * @return true if every row is saturated
 */
bool is_perfect(const MatchVec& col_match, int n_rows, int last_row_cap) {
    // No augmenting step ever hands a row more columns than its capacity, so
    // counting the matched columns says as much as checking each row.
    const long long capacity =
        static_cast<long long>(n_rows) + static_cast<long long>(last_row_cap) - 1;
    long long matched = 0;
    for (int i : col_match) {
        if (i >= 0 && i < n_rows) ++matched;
    }
    return matched >= capacity;
}

/**
 * Apply Step 1 of Gabow-Tarjan algorithm
 *
 * Finds maximal set of vertex-disjoint augmenting paths on eligible edges
 * and updates matching and duals.
 *
 * Steps:
 * 1. Build equality graph of eligible edges (or use provided one for incremental updates)
 * 2. Find maximal set of vertex-disjoint augmenting paths
 * 3. Augment along each path
 * 4. For each column on any path, decrease y_v[j] by 1
 *
 * @param cost Cost matrix
 * @param row_match Current matching from rows (modified in place)
 * @param col_match Current matching from columns (modified in place)
 * @param y_u Dual variables for rows (modified in place)
 * @param y_v Dual variables for columns (modified in place)
 * @param eq_graph Optional pointer to existing equality graph (for incremental updates)
 * @param affected_cols_out Optional pointer to store list of columns whose y_v decreased
 * @return true if augmenting paths were found, false otherwise
 */
bool apply_step1(const CostMatrix& cost,
                MatchVec& row_match,
                MatchVec& col_match,
                DualVec& y_u,
                DualVec& y_v,
                std::vector<std::vector<int>>* eq_graph,
                std::vector<int>* affected_cols_out,
                int last_row_cap)
{
    PROF_TIMER(step1_ns);
    PROF_INC(step1_calls, 1);
    // 1. Build equality graph of eligible edges (or use provided one)
    std::vector<std::vector<int>> local_eq_graph;
    std::vector<std::vector<int>>& graph_ref = eq_graph ? *eq_graph : local_eq_graph;

    if (!eq_graph) {
        // No existing graph provided: build from scratch
        local_eq_graph = build_equality_graph(cost, col_match, y_u, y_v);
    }

    // 2. Find maximal set of vertex-disjoint augmenting paths
    auto paths = find_maximal_augmenting_paths(graph_ref, col_match, last_row_cap);

    if (paths.empty()) {
        return false;
    }
    PROF_INC(step1_paths, static_cast<long long>(paths.size()));

    // 3. Augment paths individually and track columns used
    // OPTIMIZED: Augment each path individually for efficiency
    // Old approach: collected all edges into a set (O(n log n)), then augmented
    // New approach: augment each path directly (O(total_path_length))
    std::vector<bool> col_used(y_v.size(), false);

    for (const auto& path : paths) {
        // Augment this path directly
        augment_along_path(path, row_match, col_match, last_row_cap);

        // Mark columns as used
        for (const auto& e : path) {
            int j = e.second;
            if (j >= 0 && j < static_cast<int>(col_used.size())) {
                col_used[j] = true;
            }
        }
    }

    // 4. Decrease y_v[j] for all columns on any path (V1 vertices in paper)
    // Also collect affected columns for incremental updates
    if (affected_cols_out) {
        affected_cols_out->clear();
    }

    for (int j = 0; j < static_cast<int>(col_used.size()); ++j) {
        if (col_used[j]) {
            y_v[j] -= 1;
            if (affected_cols_out) {
                affected_cols_out->push_back(j);
            }
        }
    }

    return true;
}

/**
 * Gabow-Tarjan inner matching algorithm
 * 
 * Finds a 1-optimal perfect matching by alternating between:
 * - Step 1: Maximal augmenting paths on eligible edges
 * - Step 2: Hungarian search to find one augmenting path
 * 
 * Assumes:
 * - Integer costs with c(i,j) >= -1
 * - A perfect matching exists with cost <= an (where a is small constant)
 * 
 * @param cost Cost matrix (BIG_INT for forbidden edges)
 * @param row_match Matching from rows (modified in place, or initialized)
 * @param col_match Matching from columns (modified in place, or initialized)
 * @param y_u Dual variables for rows (modified in place, or initialized)
 * @param y_v Dual variables for columns (modified in place, or initialized)
 * @param max_iters Maximum iterations to prevent infinite loops
 * @param check_feasible If true, check 1-feasibility at each step (debug only)
 * @throws std::runtime_error if no perfect matching exists or max_iters exceeded
 */
void match_gt(const CostMatrix& cost,
             MatchVec& row_match,
             MatchVec& col_match,
             DualVec& y_u,
             DualVec& y_v,
             int max_iters,
             bool check_feasible,
             int last_row_cap)
{
    PROF_INC(match_gt_calls, 1);
    const int n = static_cast<int>(cost.size());
    const int m = n > 0 ? static_cast<int>(cost[0].size()) : 0;
    
    // Initialize vectors if needed
    if (static_cast<int>(row_match.size()) != n) {
        row_match.assign(n, NIL);
    }
    if (static_cast<int>(col_match.size()) != m) {
        col_match.assign(m, NIL);
    }
    if (static_cast<int>(y_u.size()) != n) {
        y_u.assign(n, 0);
    }
    if (static_cast<int>(y_v.size()) != m) {
        y_v.assign(m, 0);
    }
    
    // -------------------------------------------------------------------------
    // NORMALIZATION: rebuild col_match from row_match (O(n+m)).
    // The caller is responsible for delivering a 1-feasible (cost, duals,
    // matching) triple; that invariant is asserted under COUPLR_GT_DEBUG. The
    // previous O(nm) check_one_feasible + discard/restart block has been
    // removed (Phase 1 of gt-speedup.md).
    // -------------------------------------------------------------------------
    // A capacitated row holds its columns in col_match alone, so those entries
    // are the ones kept; the rest are rebuilt from row_match.
    const int cap_row = capacitated_row(n, last_row_cap);
    for (int j = 0; j < m; ++j) {
        if (col_match[j] != cap_row) {
            col_match[j] = NIL;
        }
    }

    for (int i = 0; i < n; ++i) {
        if (i == cap_row) {
            row_match[i] = NIL;
            continue;
        }
        int j = row_match[i];
        if (j != NIL && j >= 0 && j < m) {
            if (col_match[j] != NIL) {
                // Conflict: column j already claimed by another row. Drop this match.
                row_match[i] = NIL;
            } else {
                col_match[j] = i;
            }
        } else if (j != NIL) {
            row_match[i] = NIL;
        }
    }

    // Empty-matching entry path: canonical 1-feasible dual init (O(nm) once,
    // required for correctness when c contains values < -1). For non-empty
    // matchings the caller must supply 1-feasible duals.
    if (gt_matching_is_empty(col_match)) {
        gt_init_empty_duals(cost, y_u, y_v);
    }

    DEBUG_ASSERT_FEASIBLE(cost, col_match, y_u, y_v,
        "1-feasibility violated entering match_gt");

    if (is_perfect(col_match, n, last_row_cap)) {
        return;
    }


    // Build initial equality graph once. From here on it is maintained
    // incrementally: Step 1 mutates only y_v on path cols; Step 2 mutates
    // y_u on rows that entered S and y_v on cols that entered T. The
    // post-Step-2 full O(nm) rebuild has been replaced (Phase 2 of
    // gt-speedup.md).
    std::vector<std::vector<int>> eq_graph = build_equality_graph(cost, col_match, y_u, y_v);
    std::vector<int> affected_rows;
    std::vector<int> affected_cols;
    const std::vector<int> no_affected_rows;

    int it = 0;
    while (!is_perfect(col_match, n, last_row_cap)) {
        ++it;
        if (it > max_iters) {
            LAP_ERROR("match_gt exceeded max_iters");
        }

        // Optional: Check 1-feasibility before Step 1 (debug only)
        if (check_feasible) {
            if (!check_one_feasible(cost, col_match, y_u, y_v)) {
                LAP_ERROR("1-feasibility violated before Step 1");
            }
        }

        // Step 1: maximal vertex-disjoint augmenting paths on eligible edges.
        bool found_paths = apply_step1(cost, row_match, col_match, y_u, y_v,
                                       &eq_graph, &affected_cols, last_row_cap);

        if (found_paths && !affected_cols.empty()) {
            update_equality_graph_incremental(eq_graph, cost, col_match, y_u, y_v,
                                              no_affected_rows, affected_cols);
        }

        if (is_perfect(col_match, n, last_row_cap)) {
            break;
        }

        // Step 2: only when Step 1 found no paths. One Hungarian search to
        // extract an augmenting path on the cost-length graph; emits the
        // forest's S/T as affected_rows/affected_cols for the eq-graph patch.
        if (!found_paths) {
            if (!hungarian_step_one_feasible(cost, row_match, col_match, y_u, y_v,
                                             &affected_rows, &affected_cols,
                                             last_row_cap)) {
                LAP_ERROR("No augmenting path in Step 2 (no perfect matching)");
            }

            update_equality_graph_incremental(eq_graph, cost, col_match, y_u, y_v,
                                              affected_rows, affected_cols);
        }

        // Optional: Check 1-feasibility after Step 2 (debug only)
        if (check_feasible) {
            if (!check_one_feasible(cost, col_match, y_u, y_v)) {
                LAP_ERROR("1-feasibility violated after Step 2");
            }
        }
    }
}

// ============================================================================
// Module G: scale_match - Wrapper for bit-scaling outer loop
// ============================================================================

/**
 * Gabow-Tarjan scale_match wrapper for bit-scaling outer loop
 * 
 * This function transforms the cost matrix by subtracting global duals,
 * runs match_gt on the transformed costs to obtain a 1-optimal matching
 * with local duals, then updates the global duals and matching.
 *
 * 1-optimality is a statement about a matching that saturates both sides, so
 * the instance must be one every column can be matched in: the row capacities
 * have to add up to the number of columns. solve_gabow_tarjan_inner() arranges
 * that once, before the first scale, by giving the last row the capacity of
 * the dummy side.
 *
 * Algorithm:
 * 1. Build c'(i,j) = c(i,j) - y_u[i] - y_v[j]
 * 2. Run match_gt on c' starting from current matching to get local duals y'
 * 3. Update global duals: y_u[i] += y'_u[i], y_v[j] += y'_v[j]
 * 4. Update global matching to the one found by match_gt
 * 
 * @param cost Original cost matrix (BIG_INT indicates forbidden edge)
 *             Can be n×m with n != m
 * @param row_match Current matching from rows; updated in-place
 * @param col_match Current matching from columns; updated in-place
 * @param y_u Global dual variables for rows; updated in-place
 * @param y_v Global dual variables for columns; updated in-place
 */
void scale_match(const CostMatrix& cost,
                MatchVec& row_match,
                MatchVec& col_match,
                DualVec& y_u,
                DualVec& y_v,
                bool enable_6n_prune,
                int last_row_cap) {
    PROF_INC(scale_match_calls, 1);
    const int n = static_cast<int>(cost.size());
    const int m = (n > 0 ? static_cast<int>(cost[0].size()) : 0);

    const long long total_capacity =
        static_cast<long long>(n) + static_cast<long long>(last_row_cap) - 1;
    if (total_capacity != m) {
        LAP_ERROR("scale_match requires an instance every column can be "
                  "matched in; got " + std::to_string(n) + " rows of total "
                  "capacity " + std::to_string(total_capacity) + " and " +
                  std::to_string(m) + " columns");
    }

    // Ensure dual vectors are properly sized
    if (static_cast<int>(y_u.size()) != n) {
        y_u.resize(n, 0);
    }
    if (static_cast<int>(y_v.size()) != m) {
        y_v.resize(m, 0);
    }

    const int n_work = n;
    const int m_work = m;

    // 1. Build c'(i,j) = c(i,j) - y_u[i] - y_v[j]
    //    Forbidden edges (cost >= BIG_INT) remain BIG_INT
    //
    //    Paper p.9 pruning heuristic: any edge whose c'(i,j) exceeds the cost
    //    of a whole saturating matching cannot be in the optimum matching for
    //    this scale, provided scale_match's input obeys the paper's entry
    //    conditions (every edge cost >= -1; a saturating matching exists at a
    //    cost of at most 3 per edge, so at most 3m over its m edges, one for
    //    each column). The bit-scaling outer loop in
    //    solve_gabow_tarjan_inner guarantees this -- between scales, Step 1's
    //    dual update yields y(v) + y(w) >= c(vw) - 3 on matched edges and the
    //    (n+1) scaling keeps cost values aligned. Standalone callers
    //    (e.g. tests passing raw costs) cannot guarantee the precondition, so
    //    enable_6n_prune defaults to false. Setting cost_prime cells to
    //    BIG_INT excludes them from build_equality_graph,
    //    update_equality_graph_incremental, and enqueue_edges_from_row
    //    (all already have BIG_INT skips).
    const long long prune_threshold = 6LL * static_cast<long long>(m_work);
    CostMatrix cost_prime(n_work, std::vector<long long>(m_work, BIG_INT));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            long long c_ij = cost[i][j];
            if (c_ij >= BIG_INT) {
                // Forbidden edge stays forbidden
                cost_prime[i][j] = BIG_INT;
            } else {
                long long cp = c_ij - y_u[i] - y_v[j];
                cost_prime[i][j] =
                    (enable_6n_prune && cp >= prune_threshold) ? BIG_INT : cp;
            }
        }
    }

    // 2. Local matching and duals for match_gt on cost_prime.
    //
    //    Phase 1 note: match_gt no longer carries an O(nm) discard/restart
    //    block. With y_u_loc = y_v_loc = 0 a non-empty inherited matching is
    //    not generally 1-feasible on cost_prime (matched-edge lower bound
    //    fails when reduced cost > 0), so we start match_gt from an empty
    //    matching here. The previous code path also discarded the matching
    //    every phase via match_gt's check_one_feasible; this just makes that
    //    explicit and cheap. Phase 4 (drop cost_prime) restores a real
    //    bit-scaling warm start.
    MatchVec row_loc(n_work, NIL);
    MatchVec col_loc(m_work, NIL);
    DualVec y_u_loc(n_work, 0);
    DualVec y_v_loc(m_work, 0);

    // Every iteration of match_gt augments at least one path, and each path
    // matches one more column, so the column count is the iteration ceiling.
    match_gt(cost_prime, row_loc, col_loc, y_u_loc, y_v_loc,
             /*max_iters=*/m_work + 1,
             /*check_feasible=*/false,
             last_row_cap);
    
    // 3. Update global duals: y <- y + y'
    for (int i = 0; i < n; ++i) {
        y_u[i] += y_u_loc[i];
    }
    for (int j = 0; j < m; ++j) {
        y_v[j] += y_v_loc[j];
    }

    // 4. Update global matching to the result from match_gt
    row_match = row_loc;
    col_match = col_loc;
}

// ============================================================================
// Module H: Gabow-Tarjan bit-scaling outer loop
// ============================================================================

/**
 * Find maximum finite cost in the cost matrix
 * 
 * @param cost Cost matrix (BIG_INT for forbidden edges)
 * @return Maximum finite cost value (0 if all edges are forbidden)
 */
long long find_max_cost(const CostMatrix& cost) {
    long long max_cost = 0;
    for (const auto& row : cost) {
        for (long long c : row) {
            if (c < BIG_INT && c > max_cost) {
                max_cost = c;
            }
        }
    }
    return max_cost;
}

namespace {

/**
 * Gabow-Tarjan bit-scaling loop over an instance every column can be matched in
 *
 * The row capacities add up to the number of columns: n - 1 ordinary rows of
 * one column each and a last row of last_row_cap. Returns the minimum-cost
 * saturating matching and its duals.
 *
 * Algorithm:
 * 1. Shift costs to non-negative: c'(e) = c(e) - min_cost
 * 2. Scale by K = gap_bound + 1: c^(e) = K * c'(e)
 * 3. Determine number of bits k for c^_max
 * 4. Build costs bit-by-bit from MSB to LSB
 * 5. At each scale s:
 *    a. Update costs: c(e) <- 2c(e) + (bit s of c^(e))
 *    b. Update duals: y(v) <- 2y(v) - 1
 *    c. Run scale_match from the current matching to get 1-optimal solution
 * 6. Adjust duals back for original costs
 *
 * The K scaling is what turns 1-optimality into optimality: a 1-optimal
 * matching costs at most gap_bound more than the optimum, every scaled cost
 * is a multiple of K, and a saturating matching spends the same number of
 * edges either way, so a gap of one unit of the original cost would already
 * exceed what 1-optimality allows.
 *
 * @param cost Cost matrix (BIG_INT for forbidden edges)
 * @param last_row_cap Columns the last row holds at once
 * @param gap_bound Largest cost a 1-optimal matching can be above the optimum
 * @param row_match Output: optimal matching from rows (0-based)
 * @param col_match Output: optimal matching from columns (0-based)
 * @param y_u Output: optimal dual variables for rows
 * @param y_v Output: optimal dual variables for columns
 */
void gt_bit_scale(const CostMatrix& cost,
                  int last_row_cap,
                  long long gap_bound,
                  MatchVec& row_match,
                  MatchVec& col_match,
                  DualVec& y_u,
                  DualVec& y_v) {
    const int n = static_cast<int>(cost.size());
    const int m = (n > 0 ? static_cast<int>(cost[0].size()) : 0);

    // Step 1: Find minimum cost to shift everything to non-negative
    long long min_cost = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (cost[i][j] < BIG_INT && cost[i][j] < min_cost) {
                min_cost = cost[i][j];
            }
        }
    }
    
    // Step 2: CRITICAL - Multiply by K = gap_bound + 1 as per paper
    // Create c^(e) = K * (c(e) - min_cost)
    const long long K = gap_bound + 1;
    CostMatrix scaled_cost(n, std::vector<long long>(m));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (cost[i][j] < BIG_INT) {
                scaled_cost[i][j] = K * (cost[i][j] - min_cost);
            } else {
                scaled_cost[i][j] = BIG_INT;
            }
        }
    }
    
    // Step 3: Find maximum scaled cost to determine number of bits
    long long C_max = 0;
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (scaled_cost[i][j] < BIG_INT && scaled_cost[i][j] > C_max) {
                C_max = scaled_cost[i][j];
            }
        }
    }
    
    if (C_max == 0) {
        // All allowed edges have equal (zero) scaled cost, so any perfect
        // matching over the allowed edges is optimal. Find a maximum-cardinality
        // matching via augmenting paths rather than assuming the diagonal is
        // available -- a forbidden diagonal would otherwise leave units
        // unmatched even when a perfect matching exists.
        //
        // The capacitated row takes whatever the ordinary rows leave, which
        // costs the same wherever it lands, so it is filled last and needs no
        // search of its own.
        const int cap_row = capacitated_row(n, last_row_cap);
        row_match.assign(n, NIL);
        col_match.assign(m, NIL);
        y_u.assign(n, min_cost);
        y_v.assign(m, 0);

        std::vector<char> seen(m);
        std::function<bool(int)> try_augment = [&](int u) -> bool {
            for (int v = 0; v < m; ++v) {
                if (cost[u][v] < BIG_INT && !seen[v]) {
                    seen[v] = 1;
                    if (col_match[v] == NIL || try_augment(col_match[v])) {
                        row_match[u] = v;
                        col_match[v] = u;
                        return true;
                    }
                }
            }
            return false;
        };

        for (int u = 0; u < n; ++u) {
            if (u == cap_row) continue;
            std::fill(seen.begin(), seen.end(), 0);
            try_augment(u);
        }

        if (cap_row != NIL) {
            for (int v = 0; v < m; ++v) {
                if (col_match[v] == NIL && cost[cap_row][v] < BIG_INT) {
                    col_match[v] = cap_row;
                }
            }
        }
        return;
    }
    
    // Step 4: Determine number of bits k
    int k = 0;
    long long temp = C_max;
    while (temp > 0) {
        temp >>= 1;
        ++k;
    }
    
    // Step 5: Initialize current costs to 0 (will build bit by bit)
    CostMatrix c_current(n, std::vector<long long>(m, 0));
    for (int i = 0; i < n; ++i) {
        for (int j = 0; j < m; ++j) {
            if (scaled_cost[i][j] >= BIG_INT) {
                c_current[i][j] = BIG_INT;
            }
        }
    }
    
    // Initialize duals to 0
    y_u.assign(n, 0);
    y_v.assign(m, 0);
    
    // Step 6: Bit-scaling loop (from MSB to LSB)
    for (int s = k - 1; s >= 0; --s) {
        // Step 1 of paper: Update costs and duals
        
        // Update costs: c(e) ← 2c(e) + (bit s of scaled_cost)
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                if (scaled_cost[i][j] < BIG_INT) {
                    // Double current cost (use multiplication to avoid UB on negative values)
                    c_current[i][j] = c_current[i][j] * 2;

                    // Add bit s
                    long long bit_s = (scaled_cost[i][j] >> s) & 1LL;
                    c_current[i][j] += bit_s;
                }
            }
        }
        
        // Update duals: y(v) ← 2y(v) - 1
        // Use multiplication instead of bit shift to avoid UB with negative values
        for (int i = 0; i < n; ++i) {
            y_u[i] = 2 * y_u[i] - 1;
        }
        for (int j = 0; j < m; ++j) {
            y_v[j] = 2 * y_v[j] - 1;
        }
        
        // Step 2: Find 1-optimal matching with scale_match.
        // Carrying the previous phase's matching is the scaling warm start.
        // enable_6n_prune=true: paper p.9 heuristic. The bit-scaling loop
        // guarantees the paper's entry conditions (perfect matching of cost
        // <= 3n in scale_match's transformed costs), so the >= 6n pruning is
        // safe and strips edges that cannot belong to any optimum for this
        // scale.
        scale_match(c_current, row_match, col_match, y_u, y_v,
                    /*enable_6n_prune=*/true, last_row_cap);
    }

    // Step 7: Adjust duals back for original costs
    // The duals maintain: y_u[i] + y_v[j] = K * (original[i][j] - min_cost) for matched edges
    // We need to adjust back to original scale and add back min_cost shift
    for (int i = 0; i < n; ++i) {
        y_u[i] = y_u[i] / K + min_cost;
    }
    for (int j = 0; j < m; ++j) {
        y_v[j] = y_v[j] / K;
    }
}

}  // namespace

/**
 * Gabow-Tarjan solver over an instance of any shape
 *
 * The 1-optimality bound compares the matching found against an optimal one
 * through the duals, and the column terms cancel only when both matchings use
 * every column. On a square instance they do. On a rectangular one they pay
 * for different column sets, the bound does not hold, and the matching that
 * comes back can be far from optimal.
 *
 * A dummy side restores it. A dummy may take any partner for nothing, so a
 * minimum-cost saturating matching over the real rows plus m - n dummy rows
 * costs what the rectangular optimum costs, and the real rows in it are that
 * optimum. Those dummy rows are copies of one node, so the solver carries them
 * as one row of capacity m - n and the instance stays n + 1 rows tall.
 *
 * With the dummy side collapsed the bound tightens as well. Write y_d for the
 * dummy row's dual. Every column obeys y_d + y_v[j] <= 1, and every column the
 * dummy holds obeys y_d + y_v[j] >= 0. Take the matching M found and any
 * matching M* covering the same rows, and let t be the number of columns M
 * uses that M* does not. Summing the 1-feasibility bounds over the real edges
 * costs n, the columns in M and not M* contribute at most t(1 - y_d), the
 * columns in M* and not M at most t * y_d, y_d cancels, and cost(M) is within
 * n + t <= 2n of cost(M*). The square instance's bound is its own size, which
 * for a wide problem is m.
 *
 * A tall instance is the same problem read the other way round, so it is
 * transposed rather than given a second implementation.
 *
 * @param cost Original cost matrix (BIG_INT for forbidden edges)
 * @param row_match Output: optimal matching from rows (0-based)
 * @param col_match Output: optimal matching from columns (0-based)
 * @param y_u Output: optimal dual variables for rows
 * @param y_v Output: optimal dual variables for columns
 */
void solve_gabow_tarjan_inner(const CostMatrix& cost,
                              MatchVec& row_match,
                              MatchVec& col_match,
                              DualVec& y_u,
                              DualVec& y_v) {
    PROF_RESET();
    PROF_TIMER(total_ns);
    const int n = static_cast<int>(cost.size());
    const int m = (n > 0 ? static_cast<int>(cost[0].size()) : 0);

    if (n == 0 || m == 0) {
        row_match.assign(n, NIL);
        col_match.assign(m, NIL);
        y_u.assign(n, 0);
        y_v.assign(m, 0);
        return;
    }

    if (n > m) {
        CostMatrix flipped(m, std::vector<long long>(n));
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                flipped[j][i] = cost[i][j];
            }
        }

        MatchVec flipped_row_match, flipped_col_match;
        DualVec flipped_y_u, flipped_y_v;
        solve_gabow_tarjan_inner(flipped, flipped_row_match, flipped_col_match,
                                 flipped_y_u, flipped_y_v);

        row_match = flipped_col_match;
        col_match = flipped_row_match;
        y_u = flipped_y_v;
        y_v = flipped_y_u;
        return;
    }

    const int dummy_cap = m - n;
    if (dummy_cap == 0) {
        gt_bit_scale(cost, /*last_row_cap=*/1, /*gap_bound=*/n,
                     row_match, col_match, y_u, y_v);
    } else {
        // One extra row standing for the whole dummy side, zero cost to every
        // column. At m = n + 1 it stands for a single dummy and the instance
        // is square again, so it carries no capacity of its own.
        CostMatrix padded(cost);
        padded.emplace_back(m, 0);

        MatchVec padded_row_match;
        DualVec padded_y_u;
        gt_bit_scale(padded, /*last_row_cap=*/dummy_cap, /*gap_bound=*/2LL * n,
                     padded_row_match, col_match, padded_y_u, y_v);

        // A column the dummy row holds is a column no real unit was paired with.
        const int dummy_row = n;
        row_match.assign(n, NIL);
        for (int j = 0; j < m; ++j) {
            if (col_match[j] == dummy_row) {
                col_match[j] = NIL;
            } else if (col_match[j] != NIL) {
                row_match[col_match[j]] = j;
            }
        }
        y_u.assign(padded_y_u.begin(), padded_y_u.begin() + n);
    }

#ifdef COUPLR_GT_PROFILE
    // PROF_TIMER(total_ns) destructor hasn't run yet; sample the elapsed time
    // for the print, then let the destructor finalize total_ns.
    {
        auto& p = gt_prof();
        // Approximate: print the running totals (total_ns finalized just after).
        // We use a print here so the line appears once per solve in benchmarks.
        long long saved_total = p.total_ns;
        p.total_ns = std::chrono::duration_cast<std::chrono::nanoseconds>(
                         std::chrono::steady_clock::now() -
                         pr_timer_total_ns.t0)
                         .count();
        p.print();
        p.total_ns = saved_total;
    }
#endif
}
