#ifndef COUPLR_NS_TYPES_H
#define COUPLR_NS_TYPES_H

#include <vector>
#include <limits>
#include <cmath>
#include <string>

namespace couplr {
namespace ns {

// Arc states
constexpr int STATE_LOWER = 0;  // Flow at lower bound (0)
constexpr int STATE_TREE = 1;   // In spanning tree
constexpr int STATE_UPPER = 2;  // Flow at upper bound (1)

// Special values
constexpr int NO_ARC = -1;
constexpr int NO_NODE = -1;
constexpr double BIG_M = 1e15;
constexpr double EPSILON = 1e-9;

// Node types for assignment network
constexpr int NODE_SOURCE = 0;
constexpr int NODE_ROW = 1;
constexpr int NODE_COL = 2;
constexpr int NODE_SINK = 3;

// Why the pivot loop stopped. Optimality means pricing scanned every non-tree
// arc and none had a negative reduced cost, which is the simplex optimality
// condition. PivotLimit means the cap below was reached while an improving arc
// was still available, so the basis is feasible but not proven optimal.
enum class Termination {
    Optimality,
    PivotLimit
};

// Pivot cap for one solve. Bounds the number of basis exchanges, not the work
// inside one exchange. This is the only cap on the pivot loop; every caller
// reaches that loop through lap::solve_network_simplex(). A solve that reaches
// the cap terminates with Termination::PivotLimit and status "iteration_limit".
// Evaluated in 64-bit: 4 * num_nodes^2 passes a 32-bit int at num_nodes 23171.
constexpr long long PIVOT_LIMIT_FACTOR = 4;

inline long long pivot_limit(int num_nodes) {
    return PIVOT_LIMIT_FACTOR * static_cast<long long>(num_nodes)
                              * static_cast<long long>(num_nodes);
}

// Result structure. Costs are the working costs held in NSState, which are
// negated for maximization and carry the forbidden sentinel.
struct NSResult {
    std::vector<int> assignment;   // row i -> col assignment[i] (0-indexed), -1 unmatched
    double total_cost = 0.0;
    int pivot_count = 0;
    int n_unmatched = 0;           // rows the final basis leaves without a column
    std::string status;            // "optimal" or "iteration_limit"
};

// Main state structure
struct NSState {
    // Problem dimensions
    int n_rows;
    int n_cols;
    int num_nodes;   // n_rows + n_cols + 2 (source + sink)
    int num_arcs;    // n_rows + n_rows*n_cols + n_cols

    // Arc representation (indexed 0..num_arcs-1)
    // Arc layout:
    //   [0, n_rows): source -> row_i arcs
    //   [n_rows, n_rows + n_rows*n_cols): row_i -> col_j arcs
    //   [n_rows + n_rows*n_cols, num_arcs): col_j -> sink arcs
    std::vector<int> arc_source;   // Source node of arc
    std::vector<int> arc_target;   // Target node of arc
    std::vector<double> arc_cost;  // Cost of arc
    std::vector<int> arc_flow;     // Current flow (0 or 1)
    std::vector<int> arc_state;    // STATE_LOWER, STATE_TREE, or STATE_UPPER

    // Spanning tree representation
    // All arrays indexed by node (0..num_nodes-1)
    std::vector<int> parent;       // Parent node in tree (-1 for root)
    std::vector<int> parent_arc;   // Arc connecting to parent (-1 for root)
    std::vector<bool> arc_to_parent_up;  // True if arc points toward parent
    std::vector<int> depth;        // Depth in tree (root = 0)

    // Dual variables
    std::vector<double> potential; // Node potentials pi[i]

    // For block search pricing
    int block_size;
    int next_arc;  // Next arc to check in pricing

    // Statistics
    int pivot_count;

    // Helper functions for node indexing
    inline int source_node() const { return 0; }
    inline int sink_node() const { return num_nodes - 1; }
    inline int row_node(int i) const { return 1 + i; }
    inline int col_node(int j) const { return 1 + n_rows + j; }

    // Get row/col index from node
    inline int node_to_row(int node) const { return node - 1; }
    inline int node_to_col(int node) const { return node - 1 - n_rows; }

    // Check node type
    inline int node_type(int node) const {
        if (node == 0) return NODE_SOURCE;
        if (node == num_nodes - 1) return NODE_SINK;
        if (node <= n_rows) return NODE_ROW;
        return NODE_COL;
    }

    // Arc index helpers
    inline int source_to_row_arc(int i) const { return i; }
    inline int row_to_col_arc(int i, int j) const { return n_rows + i * n_cols + j; }
    inline int col_to_sink_arc(int j) const { return n_rows + n_rows * n_cols + j; }

    // Get row/col from assignment arc index
    inline int arc_to_row(int arc) const { return (arc - n_rows) / n_cols; }
    inline int arc_to_col(int arc) const { return (arc - n_rows) % n_cols; }

    // Check if arc is an assignment arc (row->col)
    inline bool is_assignment_arc(int arc) const {
        return arc >= n_rows && arc < n_rows + n_rows * n_cols;
    }
};

// Pivot information
struct PivotInfo {
    int entering_arc;
    int leaving_arc;
    int join_node;       // LCA of entering arc endpoints
    int u_in, v_in;      // Endpoints of entering arc (u_in -> v_in)
    int u_out, v_out;    // Endpoints of leaving arc
    int delta;           // Flow change (0 or 1 for assignment)
    bool first_side;     // Which side of cycle leaving arc is on
};

} // namespace ns
} // namespace couplr

#endif // COUPLR_NS_TYPES_H
