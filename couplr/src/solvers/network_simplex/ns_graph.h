#ifndef COUPLR_NS_GRAPH_H
#define COUPLR_NS_GRAPH_H

#include "ns_types.h"
#include "../../core/lap_error.h"
#include <cmath>
#include <limits>

namespace couplr {
namespace ns {

// Build the assignment network from a cost matrix
// Network structure:
//   Node 0: source (supply = n_rows)
//   Nodes 1..n_rows: row nodes
//   Nodes n_rows+1..n_rows+n_cols: column nodes
//   Node n_rows+n_cols+1: sink (demand = n_rows)
//
// Arcs:
//   source -> row_i: cost=0, cap=1 (n_rows arcs)
//   row_i -> col_j: cost=C[i,j], cap=1 (n_rows*n_cols arcs)
//   col_j -> sink: cost=0, cap=1 (n_cols arcs)

inline void build_assignment_network(NSState& state,
                                      const double* cost_matrix,
                                      int n_rows, int n_cols) {
    state.n_rows = n_rows;
    state.n_cols = n_cols;
    state.num_nodes = n_rows + n_cols + 2;  // source + rows + cols + sink

    // Arc count is n_rows + n_rows*n_cols + n_cols. The row->col block alone is
    // O(n*m), which overflows a 32-bit int (num_arcs and every arc index) around
    // n*m ~ 2^31. Compute in 64-bit and reject before the wrap silently corrupts
    // indexing (such a network would exhaust memory anyway).
    long long arcs = static_cast<long long>(n_rows)
                   + static_cast<long long>(n_rows) * n_cols
                   + n_cols;
    if (arcs > static_cast<long long>(std::numeric_limits<int>::max())) {
        LAP_THROW_DIMENSION("network_simplex: problem too large "
                            "(arc count exceeds 2^31); use method = 'jv' or 'auction'.");
    }
    state.num_arcs = static_cast<int>(arcs);

    // Allocate arc arrays
    state.arc_source.resize(state.num_arcs);
    state.arc_target.resize(state.num_arcs);
    state.arc_cost.resize(state.num_arcs);
    state.arc_flow.resize(state.num_arcs, 0);
    state.arc_state.resize(state.num_arcs, STATE_LOWER);

    // Allocate tree arrays
    state.parent.resize(state.num_nodes, NO_NODE);
    state.parent_arc.resize(state.num_nodes, NO_ARC);
    state.arc_to_parent_up.resize(state.num_nodes, false);
    state.depth.resize(state.num_nodes, 0);
    state.potential.resize(state.num_nodes, 0.0);

    int arc_idx = 0;
    int source = state.source_node();
    int sink = state.sink_node();

    // Source -> row arcs (indices 0..n_rows-1)
    for (int i = 0; i < n_rows; ++i) {
        state.arc_source[arc_idx] = source;
        state.arc_target[arc_idx] = state.row_node(i);
        state.arc_cost[arc_idx] = 0.0;
        ++arc_idx;
    }

    // Row -> col arcs (indices n_rows..n_rows+n_rows*n_cols-1)
    // cost_matrix is column-major (R convention): cost[i,j] = cost_matrix[i + j*n_rows]
    for (int i = 0; i < n_rows; ++i) {
        for (int j = 0; j < n_cols; ++j) {
            state.arc_source[arc_idx] = state.row_node(i);
            state.arc_target[arc_idx] = state.col_node(j);
            state.arc_cost[arc_idx] = cost_matrix[i + j * n_rows];
            ++arc_idx;
        }
    }

    // Col -> sink arcs (indices n_rows+n_rows*n_cols..num_arcs-1)
    for (int j = 0; j < n_cols; ++j) {
        state.arc_source[arc_idx] = state.col_node(j);
        state.arc_target[arc_idx] = sink;
        state.arc_cost[arc_idx] = 0.0;
        ++arc_idx;
    }

    // Initialize block search parameters
    state.block_size = std::max(1, static_cast<int>(std::sqrt(static_cast<double>(state.num_arcs))));
    state.next_arc = 0;
    state.pivot_count = 0;
}

// Compute reduced cost of an arc
// reduced_cost = cost[arc] - potential[source] + potential[target]
// For minimization: negative reduced cost means improvement possible
inline double reduced_cost(const NSState& state, int arc) {
    return state.arc_cost[arc]
           - state.potential[state.arc_source[arc]]
           + state.potential[state.arc_target[arc]];
}

// Get effective reduced cost considering arc direction and state
// For arcs at lower bound: enter if reduced_cost < 0 (increase flow)
// For arcs at upper bound: enter if reduced_cost > 0 (decrease flow)
inline double effective_reduced_cost(const NSState& state, int arc) {
    if (state.arc_state[arc] == STATE_TREE) {
        return 0.0;  // Tree arcs are always optimal
    }
    double rc = reduced_cost(state, arc);
    if (state.arc_state[arc] == STATE_UPPER) {
        return -rc;  // Flip sign for upper bound arcs
    }
    return rc;  // STATE_LOWER
}

} // namespace ns
} // namespace couplr

#endif // COUPLR_NS_GRAPH_H
