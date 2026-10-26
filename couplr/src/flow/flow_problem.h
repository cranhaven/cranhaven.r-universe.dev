// src/flow/flow_problem.h
// The one internal representation every matching design compiles into.
// Pure C++ types - NO Rcpp dependencies, same rule as lap_types.h.
//
// A design is a min-cost flow problem:
//
//     min  sum_a cost(a) * f(a)
//     s.t. sum_{a out of v} f(a) - sum_{a into v} f(a) = supply(v)   for every node v
//          lower(a) <= f(a) <= upper(a)                              for every arc a
//
// Capacities are integral. Every matching design has integral capacities, and
// integrality is what makes the flow certificate exact rather than
// tolerance-bound: a fractional flow can satisfy complementary slackness to
// within a tolerance without corresponding to any matching. int64_t rather
// than int for the same reason flat_index() is 64-bit -- the package's own
// vignettes reach n = 50,000, where a capacity summed over rows overflows int.
//
// Costs are double and are sign-adjusted by the caller. A maximization instance
// is compiled by negating the costs, exactly as lap_certify.h requires, which is
// why nothing here takes a `maximize` flag.
//
// Node layout is one convention for every compiler in flow_compile.h:
//
//     0                        auxiliary source; potentials are normalized so
//                              pi[0] == 0
//     1                        auxiliary sink
//     2 .. 2+nr-1              row nodes    (treated units, group centres)
//     2+nr .. 2+nr+nc-1        column nodes (control units)
//     2+nr+nc ..               design-specific: category nodes for fine
//                              balance, stratum nodes for exact matching
//
// Four incompatible node numberings are in use across the solvers this replaces
// (csflow, cycle_cancel, full_matching and network_simplex each invented their
// own). Fixing one here is what lets a certificate, a warm start and a dual
// mapping be written once.
#pragma once

#include "flow_oracle.h"

#include <cstdint>
#include <limits>
#include <utility>
#include <vector>

namespace lap {

// Stands in for an unbounded arc. Not int64_t's maximum: capacities are summed
// when a node's throughput is bounded and when the auxiliary source's supply is
// derived, and a sentinel that saturates on the first addition turns an
// unbounded arc into a negative one.
constexpr int64_t FLOW_INF_CAP = std::numeric_limits<int64_t>::max() / 4;

// The two reserved nodes and the first row node of the layout above. Every
// compiler places its nodes from here, and every reader of a solved problem --
// the certificate, the dual mapping, a warm start -- finds them here.
constexpr int32_t FLOW_SOURCE    = 0;
constexpr int32_t FLOW_SINK      = 1;
constexpr int32_t FLOW_FIRST_ROW = 2;

struct FlowArc {
    int32_t tail  = -1;
    int32_t head  = -1;
    int64_t lower = 0;
    int64_t upper = FLOW_INF_CAP;
    double  cost  = 0.0;

    FlowArc() = default;
    FlowArc(int32_t tail_, int32_t head_, int64_t lower_, int64_t upper_, double cost_)
        : tail(tail_), head(head_), lower(lower_), upper(upper_), cost(cost_) {}
};

// A block of arcs backed by a cost source instead of stored one by one: the
// pair (i, j) is an arc from node row_base + i to node col_base + j, costing
// costs->at(i, j), present only where costs->allowed(i, j).
//
// This is the field that makes the representation forward-compatible with
// section C. Phase 2 expands every block into explicit arcs before solving;
// phase 3 replaces that expansion with a restricted arc list and a pricing loop
// over the same source. The solver sees explicit arcs either way.
struct BipartiteBlock {
    int32_t           row_base = 0;
    int32_t           col_base = 0;
    int64_t           lower    = 0;
    int64_t           upper    = 1;
    const CostOracle* costs    = nullptr;  // not owned
};

// Where a block's arcs landed in the explicit arc array, and which (i, j) each
// one carries. Written by expand_blocks(), read when a solved flow is mapped
// back to matched pairs.
struct BlockArcRange {
    int64_t first_arc = 0;
    int64_t n_arcs    = 0;
    std::vector<std::pair<int32_t, int32_t>> rc;  // aligned to the arc range
};

struct FlowProblem {
    int32_t              n_nodes = 0;
    std::vector<int64_t> supply;            // signed, size n_nodes, sums to zero
    std::vector<FlowArc> arcs;
    std::vector<BipartiteBlock> blocks;

    // Filled by expand_blocks(); empty until then.
    std::vector<BlockArcRange> block_arcs;
    bool expanded = false;

    // Warm start, both optional. Empty means cold. warm_flow is aligned to
    // `arcs` after expansion, so a warm start carried across an expansion has
    // to be supplied after it.
    std::vector<int64_t> warm_flow;
    std::vector<double>  warm_potential;

    bool empty() const { return n_nodes == 0 || (arcs.empty() && blocks.empty()); }
};

// Structural checks, run at the top of the solver. Throws
// lap::DimensionException on a malformed problem.
//
// An infeasible problem is not malformed. A supply no arc can carry, a lower
// bound no capacity can meet, a disconnected demand: all of those are solver
// statuses with a witness, not throws.
void validate(const FlowProblem& prob);

// Turn every block into explicit arcs, skipping pairs the source forbids, and
// record the arc range and the (i, j) behind each emitted arc.
//
// Idempotent: a second call on an expanded problem does nothing. Throws
// lap::DimensionException rather than allocating past FLOW_MAX_EXPANDED_ARCS,
// because the failure mode of the alternative is a machine in swap rather than
// an error a caller can act on.
//
// Blocks are expanded in index order and a block already carrying a
// BlockArcRange is left alone, so a problem whose first blocks were expanded
// over a candidate set finishes here with the rest expanded in full.
constexpr int64_t FLOW_MAX_EXPANDED_ARCS = static_cast<int64_t>(1) << 31;

void expand_blocks(FlowProblem& prob);

class CandidateSet;

// Expand one block over a candidate set instead of over its whole grid. The
// same two gates apply, so a candidate pair the source forbids gets no arc, and
// the (i, j) behind each emitted arc is recorded exactly as the full expansion
// records it: every reader of a solved flow works unchanged.
//
// Blocks expand in index order, so `block` must be the next unexpanded one, and
// the problem is expanded once the last block has been done. Throws
// lap::DimensionException on an out-of-order block, on a problem already
// expanded, and on a candidate set whose shape is not the block's.
void expand_block_subset(FlowProblem& prob, int32_t block, const CandidateSet& cand);

// Add arcs for `new_pairs` to a block of an already expanded problem, skipping
// any the source forbids, and return how many arcs that came to.
//
// The arcs land at the end of the block's own range rather than at the end of
// the arc array, so a block's arcs stay contiguous and stay aligned with its
// rc entries; later blocks' ranges move up by the number added. When warm_flow
// is present it moves in step, each new arc entering at its lower bound, which
// is the whole warm-start contract: an arc the incumbent potentials price below
// zero is pushed to its upper bound by the solver's slackness repair, and the
// augmentation loop repairs the conservation that breaks.
//
// Membership is the candidate set's question, not this function's. Offering a
// pair the block already carries produces a second arc for it, priced the same
// and free to carry flow of its own, so a block whose columns admit more than
// one unit can place the pair twice.
int64_t add_block_arcs(FlowProblem& prob, int32_t block,
                       const std::vector<std::pair<int32_t, int32_t>>& new_pairs);

}  // namespace lap
