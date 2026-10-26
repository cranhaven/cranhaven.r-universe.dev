#ifndef COUPLR_NS_INIT_H
#define COUPLR_NS_INIT_H

#include "ns_types.h"
#include "ns_graph.h"
#include <algorithm>
#include <queue>

namespace couplr {
namespace ns {

// An arc cost is "allowed" if it is finite and below the BIG forbidden sentinel.
// The R wrapper substitutes Inf for forbidden cells; the C++ entry point that
// goes through prepare_for_solve substitutes BIG = 1e100. Both must be rejected.
inline bool ns_cost_allowed(double c) {
    // Reject NaN, +/-Inf, and the forbidden sentinel (BIG = 1e100). BIG_M = 1e15
    // is a safe threshold: real assignment costs in the test suite are O(1)-O(1e3),
    // well below 1e15, while forbidden cells are >= 1e100 or non-finite.
    return std::isfinite(c) && c < BIG_M;
}

// Initialize with a feasible spanning tree.
// For assignment: the network is source -> rows -> cols -> sink
// Each node has supply/demand: source = +n, rows = 0 (transshipment),
// cols = 0 (transshipment), sink = -n.
//
// Flow conservation requires every row to be matched to exactly one col in the
// initial basis (otherwise the source->row arc has flow 1 with no outgoing flow,
// violating conservation and giving an infeasible starting tree that pivots
// cannot necessarily repair).
//
// We build the matching in two phases:
//   1. Greedy: for each row, pick the cheapest unmatched allowed col.
//   2. Augmenting paths: for any row left unmatched by greedy, find an
//      augmenting alternating path in the bipartite graph of allowed edges
//      and flip it. This guarantees a perfect matching whenever one exists.

// Run a BFS-based augmenting path search starting from unmatched row `start_row`
// in the bipartite graph restricted to allowed (finite, non-forbidden) edges.
// On success, flip the augmenting path so `start_row` becomes matched and
// returns true. Returns false if no augmenting path exists (infeasible).
inline bool augment_match(NSState& state,
                          int start_row,
                          std::vector<int>& row_match,
                          std::vector<int>& col_match) {
    int n = state.n_rows;
    int m = state.n_cols;

    // BFS layers, starting from start_row on the row side.
    // prev_row[col] = the row from which `col` was reached.
    // row_visited[row] = already explored.
    std::vector<int> prev_row(m, -1);
    std::vector<bool> row_visited(n, false);

    std::queue<int> q;
    q.push(start_row);
    row_visited[start_row] = true;

    int found_col = -1;
    while (!q.empty() && found_col < 0) {
        int r = q.front();
        q.pop();
        for (int j = 0; j < m; ++j) {
            if (prev_row[j] >= 0) continue;  // col already discovered
            int arc = state.row_to_col_arc(r, j);
            if (!ns_cost_allowed(state.arc_cost[arc])) continue;
            prev_row[j] = r;
            int paired = col_match[j];
            if (paired < 0) {
                // Found unmatched col -> augmenting path complete.
                found_col = j;
                break;
            }
            if (!row_visited[paired]) {
                row_visited[paired] = true;
                q.push(paired);
            }
        }
    }

    if (found_col < 0) return false;

    // Flip the alternating path: walk backwards via prev_row, reassigning.
    int j = found_col;
    while (j >= 0) {
        int r = prev_row[j];
        int prev_j = row_match[r];  // the col r was previously matched to (or -1)
        row_match[r] = j;
        col_match[j] = r;
        j = prev_j;
    }
    return true;
}

// Returns the number of rows the matching covers. A value below n_rows is a
// maximum-cardinality result, so it certifies that no assignment covers every
// row; the caller decides what to do with that, and the spanning tree is not
// built, because its flow assumes one unit leaving every row node.
inline int initialize_spanning_tree_greedy(NSState& state) {
    int n = state.n_rows;
    int m = state.n_cols;
    int source = state.source_node();
    int sink = state.sink_node();

    // Track which rows and cols are matched
    std::vector<int> row_match(n, -1);  // row i matched to col row_match[i]
    std::vector<int> col_match(m, -1);  // col j matched to row col_match[j]

    // Phase 1 — Greedy: for each row, find cheapest unmatched allowed col.
    for (int i = 0; i < n; ++i) {
        int best_j = -1;
        double best_cost = BIG_M;
        for (int j = 0; j < m; ++j) {
            if (col_match[j] < 0) {
                int arc = state.row_to_col_arc(i, j);
                double c = state.arc_cost[arc];
                if (ns_cost_allowed(c) && c < best_cost) {
                    best_cost = c;
                    best_j = j;
                }
            }
        }
        if (best_j >= 0) {
            row_match[i] = best_j;
            col_match[best_j] = i;
        }
    }

    // Phase 2 - Augmenting paths: extend the partial matching to a maximum
    // matching. One attempt per unmatched row suffices: by Berge's augmenting
    // path theorem a row with no augmenting path against the current matching
    // has none against any matching a later augmentation can produce, so the
    // matching left here is of maximum cardinality.
    for (int i = 0; i < n; ++i) {
        if (row_match[i] < 0) {
            (void)augment_match(state, i, row_match, col_match);
        }
    }

    int matched_rows = 0;
    for (int i = 0; i < n; ++i) {
        if (row_match[i] >= 0) ++matched_rows;
    }

    // Uncovered rows in a maximum matching mean the allowed subgraph violates
    // Hall's condition, so no basis of this network carries a unit out of every
    // row node. Report the cardinality instead of building a tree whose flow
    // would not conserve.
    if (matched_rows < n) return matched_rows;

    // Reset all arcs to lower bound (flow = 0)
    for (int a = 0; a < state.num_arcs; ++a) {
        state.arc_state[a] = STATE_LOWER;
        state.arc_flow[a] = 0;
    }

    // Build spanning tree structure:
    // The spanning tree must have exactly (num_nodes - 1) arcs
    // For assignment with n rows and m cols: num_nodes = n + m + 2
    // Tree arcs needed = n + m + 1
    //
    // Tree structure:
    //   - n source->row arcs (all rows connect to source)
    //   - 1 row->col arc per col (connect each col to exactly one row)  [m arcs]
    //   - 1 col->sink arc (sink connects to one col)                    [1 arc]
    // Total: n + m + 1 tree arcs

    // Source is root
    state.parent[source] = NO_NODE;
    state.parent_arc[source] = NO_ARC;
    state.depth[source] = 0;

    // All source->row arcs are in tree with flow = 1
    // (Source supplies 1 unit to each row)
    for (int i = 0; i < n; ++i) {
        int row = state.row_node(i);
        int arc = state.source_to_row_arc(i);

        state.parent[row] = source;
        state.parent_arc[row] = arc;
        state.arc_to_parent_up[row] = false;  // Arc points from parent (source) to child (row)
        state.depth[row] = 1;

        state.arc_state[arc] = STATE_TREE;
        state.arc_flow[arc] = 1;
    }

    // For each col, pick parent row:
    // - If col is matched, parent = matched row, flow = 1
    // - If col is unmatched, parent = row 0, flow = 0
    for (int j = 0; j < m; ++j) {
        int col = state.col_node(j);
        int parent_row_idx;
        int flow;

        if (col_match[j] >= 0) {
            parent_row_idx = col_match[j];
            flow = 1;
        } else {
            parent_row_idx = 0;  // Unmatched cols connect to row 0
            flow = 0;
        }

        int arc = state.row_to_col_arc(parent_row_idx, j);
        state.parent[col] = state.row_node(parent_row_idx);
        state.parent_arc[col] = arc;
        state.arc_to_parent_up[col] = false;
        state.depth[col] = 2;

        state.arc_state[arc] = STATE_TREE;
        state.arc_flow[arc] = flow;
    }

    // Sink connects to ONE col via col->sink arc
    // Choose the first matched col (or col 0 if none matched)
    int sink_col = 0;
    for (int j = 0; j < m; ++j) {
        if (col_match[j] >= 0) {
            sink_col = j;
            break;
        }
    }

    // The col->sink tree arc carries flow = n (all matched units exit through it in tree)
    // But this is wrong for network simplex! The tree only tracks basis structure.
    //
    // CRITICAL FIX: In network simplex for assignment problems:
    // - Tree arcs can have any flow value (they're basis variables)
    // - Non-tree arcs are at their bounds (0 for LOWER, capacity for UPPER)
    //
    // For the TREE structure: sink has one parent col, that arc is in tree
    // For FLOW: each matched col j must have col->sink flow = 1
    //           but only ONE col->sink arc is in the TREE
    //           other col->sink arcs with flow=1 must be STATE_UPPER
    {
        int arc = state.col_to_sink_arc(sink_col);
        state.parent[sink] = state.col_node(sink_col);
        state.parent_arc[sink] = arc;
        state.arc_to_parent_up[sink] = false;
        state.depth[sink] = 3;

        state.arc_state[arc] = STATE_TREE;
        // Flow on this tree arc = 1 if this col is matched, else 0
        state.arc_flow[arc] = (col_match[sink_col] >= 0) ? 1 : 0;
    }

    // Set flows on non-tree col->sink arcs
    // Matched cols (other than sink_col) have flow = 1 -> STATE_UPPER
    // Unmatched cols have flow = 0 -> STATE_LOWER (already set)
    for (int j = 0; j < m; ++j) {
        if (j == sink_col) continue;  // Skip tree arc
        int arc = state.col_to_sink_arc(j);

        if (col_match[j] >= 0) {
            // Matched col: flow = 1 (at upper bound)
            state.arc_flow[arc] = 1;
            state.arc_state[arc] = STATE_UPPER;
        }
        // Unmatched cols stay at STATE_LOWER with flow = 0
    }

    return n;
}

// Compute potentials from the spanning tree
// For tree arc e from u to v: reduced_cost = c_uv - pi_u + pi_v = 0
// Therefore: pi_v = pi_u - c_uv
inline void compute_potentials(NSState& state) {
    int source = state.source_node();
    state.potential[source] = 0.0;

    // Process nodes by depth (BFS order)
    std::vector<std::pair<int, int>> depth_node;
    for (int i = 0; i < state.num_nodes; ++i) {
        if (state.depth[i] >= 0) {
            depth_node.emplace_back(state.depth[i], i);
        }
    }
    std::sort(depth_node.begin(), depth_node.end());

    for (const auto& dn : depth_node) {
        int node = dn.second;
        if (node == source) continue;

        int arc = state.parent_arc[node];
        int par = state.parent[node];

        if (arc == NO_ARC || par == NO_NODE) continue;

        // For tree arc from u to v: rc = c - pi_u + pi_v = 0  =>  pi_v = pi_u - c
        // If arc goes parent -> node (u=par, v=node): pi_node = pi_par - cost
        // If arc goes node -> parent (u=node, v=par): pi_par = pi_node - cost  =>  pi_node = pi_par + cost

        if (state.arc_source[arc] == par) {
            // Arc: parent -> node
            state.potential[node] = state.potential[par] - state.arc_cost[arc];
        } else {
            // Arc: node -> parent
            state.potential[node] = state.potential[par] + state.arc_cost[arc];
        }
    }
}

} // namespace ns
} // namespace couplr

#endif // COUPLR_NS_INIT_H
