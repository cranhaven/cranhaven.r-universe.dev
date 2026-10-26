#ifndef COUPLR_NS_PIVOT_H
#define COUPLR_NS_PIVOT_H

#include "ns_types.h"
#include "ns_graph.h"
#include <algorithm>

namespace couplr {
namespace ns {

// Find entering arc using block search pricing.
// Returns an arc index whose effective reduced cost is below -EPSILON, or
// NO_ARC. The block-size early exit only fires once a candidate has been found,
// so NO_ARC is returned only after all num_arcs arcs have been priced: it is
// the optimality condition over the whole arc set, not over one block.
inline int find_entering_arc(NSState& state) {
    int num_arcs = state.num_arcs;
    int block_size = state.block_size;

    int best_arc = NO_ARC;
    double best_rc = -EPSILON;

    int cnt = 0;
    int arc = state.next_arc;

    while (cnt < num_arcs) {
        if (state.arc_state[arc] != STATE_TREE) {
            double rc = reduced_cost(state, arc);
            if (state.arc_state[arc] == STATE_LOWER && rc < best_rc) {
                best_rc = rc;
                best_arc = arc;
            } else if (state.arc_state[arc] == STATE_UPPER && -rc < best_rc) {
                best_rc = -rc;
                best_arc = arc;
            }
        }
        ++arc;
        if (arc >= num_arcs) arc = 0;
        ++cnt;
        if (cnt % block_size == 0 && best_arc != NO_ARC) break;
    }

    state.next_arc = (best_arc != NO_ARC) ? best_arc : arc;
    return best_arc;
}

// Find the LCA (join node) of two nodes in the spanning tree.
inline int find_join(const NSState& state, int u, int v) {
    while (state.depth[u] > state.depth[v]) u = state.parent[u];
    while (state.depth[v] > state.depth[u]) v = state.parent[v];
    while (u != v) { u = state.parent[u]; v = state.parent[v]; }
    return u;
}

// Find leaving arc and compute delta (flow change).
inline PivotInfo find_leaving_arc(NSState& state, int entering_arc) {
    PivotInfo info;
    info.entering_arc = entering_arc;
    info.u_in = state.arc_source[entering_arc];
    info.v_in = state.arc_target[entering_arc];

    bool entering_forward = (state.arc_state[entering_arc] == STATE_LOWER);

    info.join_node = find_join(state, info.u_in, info.v_in);

    info.delta = 1;
    info.leaving_arc = entering_arc;
    info.first_side = true;

    // Trace cycle from u_in to join
    int node = info.u_in;
    while (node != info.join_node) {
        int arc = state.parent_arc[node];
        int par = state.parent[node];

        bool arc_points_to_parent = (state.arc_source[arc] == node);
        int residual;
        if (entering_forward) {
            residual = arc_points_to_parent ? state.arc_flow[arc]
                                            : 1 - state.arc_flow[arc];
        } else {
            residual = arc_points_to_parent ? 1 - state.arc_flow[arc]
                                            : state.arc_flow[arc];
        }

        if (residual < info.delta) {
            info.delta = residual;
            info.leaving_arc = arc;
            info.first_side = true;
            info.u_out = node;
            info.v_out = par;
        }
        node = par;
    }

    // Trace cycle from v_in to join
    node = info.v_in;
    while (node != info.join_node) {
        int arc = state.parent_arc[node];
        int par = state.parent[node];

        bool arc_points_to_parent = (state.arc_source[arc] == node);
        int residual;
        if (entering_forward) {
            residual = arc_points_to_parent ? 1 - state.arc_flow[arc]
                                            : state.arc_flow[arc];
        } else {
            residual = arc_points_to_parent ? state.arc_flow[arc]
                                            : 1 - state.arc_flow[arc];
        }

        if (residual < info.delta) {
            info.delta = residual;
            info.leaving_arc = arc;
            info.first_side = false;
            info.u_out = node;
            info.v_out = par;
        }
        node = par;
    }

    if (info.leaving_arc == entering_arc) {
        info.u_out = info.u_in;
        info.v_out = info.v_in;
    }

    return info;
}

// Update flow along the cycle.
inline void augment_flow(NSState& state, const PivotInfo& info) {
    if (info.delta == 0) return;

    int entering_arc = info.entering_arc;
    bool entering_forward = (state.arc_state[entering_arc] == STATE_LOWER);

    if (entering_forward) state.arc_flow[entering_arc] += info.delta;
    else                  state.arc_flow[entering_arc] -= info.delta;

    int node = state.arc_source[entering_arc];  // u_in
    while (node != info.join_node) {
        int arc = state.parent_arc[node];
        int par = state.parent[node];
        bool arc_points_to_parent = (state.arc_source[arc] == node);
        if (entering_forward) {
            if (arc_points_to_parent) state.arc_flow[arc] -= info.delta;
            else                      state.arc_flow[arc] += info.delta;
        } else {
            if (arc_points_to_parent) state.arc_flow[arc] += info.delta;
            else                      state.arc_flow[arc] -= info.delta;
        }
        node = par;
    }

    node = state.arc_target[entering_arc];  // v_in
    while (node != info.join_node) {
        int arc = state.parent_arc[node];
        int par = state.parent[node];
        bool arc_points_to_parent = (state.arc_source[arc] == node);
        if (entering_forward) {
            if (arc_points_to_parent) state.arc_flow[arc] += info.delta;
            else                      state.arc_flow[arc] -= info.delta;
        } else {
            if (arc_points_to_parent) state.arc_flow[arc] -= info.delta;
            else                      state.arc_flow[arc] += info.delta;
        }
        node = par;
    }
}

// Rebuild depth[] and potential[] from parent[]/parent_arc[] via a single O(n) BFS.
// Uses a linked-list children structure to avoid per-pivot heap allocation.
inline void rebuild_from_parent(NSState& state) {
    int nn = state.num_nodes;
    int source = state.source_node();

    // Build singly-linked children lists using two flat arrays.
    // head[p] = first child of p (-1 if none); next_sibling[c] = next child of same parent.
    std::vector<int> head(nn, -1);
    std::vector<int> next_sib(nn, -1);
    for (int i = nn - 1; i >= 0; --i) {
        int p = state.parent[i];
        if (p != NO_NODE) {
            next_sib[i] = head[p];
            head[p] = i;
        }
    }

    // BFS from source — naturally visits nodes in depth order.
    state.depth[source]     = 0;
    state.potential[source] = 0.0;

    // Reuse head[] as the BFS queue (write queue end into head after we no longer need it).
    // Simple approach: use a separate small queue vector (nn entries, stack-allocated via reserve).
    std::vector<int> queue;
    queue.reserve(nn);
    queue.push_back(source);

    for (int qi = 0; qi < static_cast<int>(queue.size()); ++qi) {
        int node = queue[qi];
        int child = head[node];
        while (child != -1) {
            int arc = state.parent_arc[child];
            state.depth[child] = state.depth[node] + 1;
            if (state.arc_source[arc] == node) {
                // Arc: parent -> child  =>  pi_child = pi_parent - cost
                state.potential[child] = state.potential[node] - state.arc_cost[arc];
            } else {
                // Arc: child -> parent  =>  pi_child = pi_parent + cost
                state.potential[child] = state.potential[node] + state.arc_cost[arc];
            }
            queue.push_back(child);
            child = next_sib[child];
        }
    }
}

// Incremental tree update after a pivot — O(path_length) parent-pointer reversal.
//
// When entering arc (u_in -> v_in) is added and leaving arc (u_out child of v_out) is removed:
//   T_u = subtree containing u_out after the removal (the part that moves).
//   The entering arc endpoint in T_u becomes the new root of T_u, connected to T_v.
//
// We reverse parent pointers along the path from that endpoint to u_out, then call
// rebuild_from_parent() to update depth[] and potential[] in O(n).
inline void update_tree(NSState& state, const PivotInfo& info) {
    int entering_arc = info.entering_arc;
    int leaving_arc  = info.leaving_arc;

    state.arc_state[entering_arc] = STATE_TREE;
    state.arc_state[leaving_arc]  = (state.arc_flow[leaving_arc] == 0)
                                        ? STATE_LOWER : STATE_UPPER;

    if (entering_arc == leaving_arc) return;  // degenerate: no structural change

    int u_out = info.u_out;

    // Determine which endpoint of the entering arc is inside T_u.
    // first_side=true  => leaving arc is on the u_in path  => u_in (arc_source) is in T_u
    // first_side=false => leaving arc is on the v_in path  => v_in (arc_target) is in T_u
    int new_child  = info.first_side ? state.arc_source[entering_arc]
                                     : state.arc_target[entering_arc];
    int new_parent = info.first_side ? state.arc_target[entering_arc]
                                     : state.arc_source[entering_arc];

    // Reverse the path from new_child up to u_out in the old tree.
    // After the loop:  parent[new_child]=new_parent (set below), and the path
    // u_out -> ... -> new_child has all parent pointers pointing toward new_child.
    {
        int prev_n = NO_NODE;
        int prev_a = NO_ARC;
        int curr   = new_child;

        while (true) {
            int next_n = state.parent[curr];
            int next_a = state.parent_arc[curr];
            state.parent[curr]     = prev_n;
            state.parent_arc[curr] = prev_a;
            if (curr == u_out) break;
            prev_n = curr;
            prev_a = next_a;
            curr   = next_n;
        }
    }

    // Connect new_child to the T_v side via the entering arc.
    state.parent[new_child]     = new_parent;
    state.parent_arc[new_child] = entering_arc;

    // Recompute depth[] and potential[] for all nodes via BFS from source.
    rebuild_from_parent(state);
}

// Full pivot: augment flow, update tree structure, rebuild depths and potentials.
inline void do_pivot(NSState& state, const PivotInfo& info) {
    augment_flow(state, info);
    update_tree(state, info);   // also calls rebuild_from_parent internally
    ++state.pivot_count;
}

// Extract assignment from the flow solution.
//
// `termination` is the pivot loop's own stopping reason and is the sole source
// of the status: "optimal" requires that pricing reached the simplex optimality
// condition, which all-rows-matched does not test. Rows the basis leaves without
// a column are counted, not interpreted; the caller holds the feasibility
// information needed to say what an unmatched row means.
inline NSResult extract_assignment(const NSState& state, Termination termination) {
    NSResult result;
    result.assignment.assign(state.n_rows, -1);
    result.total_cost  = 0.0;
    result.pivot_count = state.pivot_count;
    result.n_unmatched = 0;

    for (int i = 0; i < state.n_rows; ++i) {
        for (int j = 0; j < state.n_cols; ++j) {
            int arc = state.row_to_col_arc(i, j);
            if (state.arc_flow[arc] == 1) {
                result.assignment[i] = j;
                result.total_cost += state.arc_cost[arc];
                break;
            }
        }
        if (result.assignment[i] < 0) ++result.n_unmatched;
    }

    result.status = (termination == Termination::Optimality) ? "optimal"
                                                             : "iteration_limit";
    return result;
}

} // namespace ns
} // namespace couplr

#endif // COUPLR_NS_PIVOT_H
