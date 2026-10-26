// src/flow/flow_expand.cpp
// Structural validation and block expansion for the flow representation.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// These are the two things that happen to a FlowProblem before any arithmetic:
// validate() rejects a problem that is not a flow problem at all, and
// expand_blocks() turns the implicit bipartite blocks into the explicit arc
// array the solver walks. The split matters because the two failures are of
// different kinds. A malformed problem is a programming error in a compiler and
// throws; a problem with no feasible flow is an answer, and the solver returns
// it with a status.

#include "flow_problem.h"
#include "flow_candidates.h"
#include "../core/lap_error.h"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <string>
#include <utility>
#include <vector>

namespace lap {

namespace {

// Half-open range of node ids a block claims. Two blocks sharing a node id
// would silently alias two different (i, j) grids onto one node, which the
// solver cannot detect and the mapping back to row/column indices cannot undo.
struct NodeSpan {
    int64_t first = 0;
    int64_t last  = 0;  // exclusive
    int64_t block = 0;  // which block it came from, for the message

    bool empty() const { return last <= first; }
};

std::string arc_label(std::size_t a) {
    return "arc " + std::to_string(a);
}

// The two gates every block arc passes, and the only place they are written.
// admissible(i, j, c) is the source's own admissibility mask and the cost
// behind it, asked together; forbidden cells read as lap::BIG rather than Inf,
// so a cell can be admissible and still carry a cost the residual search cannot
// use. Returns false where the pair gets no arc, which is what makes a
// candidate set and an arc set different objects.
bool make_block_arc(const BipartiteBlock& blk, int64_t i, int64_t j, FlowArc& out) {
    const CostOracle& src = *blk.costs;
    double c = 0.0;
    if (!src.admissible(i, j, c)) return false;
    if (!std::isfinite(c)) return false;
    out = FlowArc(static_cast<int32_t>(blk.row_base + i),
                  static_cast<int32_t>(blk.col_base + j),
                  blk.lower, blk.upper, c);
    return true;
}

// The structural checks a block owes before anything is emitted for it.
// validate() runs them too, on problems that are never expanded.
void check_block(const FlowProblem& prob, std::size_t b) {
    if (b >= prob.blocks.size()) {
        LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                            " outside [0, " + std::to_string(prob.blocks.size()) + ")");
    }
    const BipartiteBlock& blk = prob.blocks[b];
    if (blk.costs == nullptr) {
        LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                            " has no cost source");
    }
    if (blk.costs->nrow() < 0 || blk.costs->ncol() < 0) {
        LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                            " has negative dimensions");
    }
}

void check_budget(const FlowProblem& prob, int64_t adding) {
    if (adding > FLOW_MAX_EXPANDED_ARCS - static_cast<int64_t>(prob.arcs.size())) {
        LAP_THROW_DIMENSION("FlowProblem: expanding the blocks needs more than " +
                            std::to_string(FLOW_MAX_EXPANDED_ARCS) + " arcs");
    }
}

// Emit one block's arcs and record where they landed. `cand` chooses which
// pairs are offered; nullptr offers the whole grid. Appends to block_arcs, so
// the caller has already established that this is the next block in order.
void expand_one_block(FlowProblem& prob, std::size_t b, const CandidateSet* cand) {
    const BipartiteBlock& blk = prob.blocks[b];
    const int64_t nr = blk.costs->nrow();
    const int64_t nc = blk.costs->ncol();

    BlockArcRange range;
    range.first_arc = static_cast<int64_t>(prob.arcs.size());

    FlowArc arc;
    for (int64_t i = 0; i < nr; ++i) {
        if (cand == nullptr) {
            for (int64_t j = 0; j < nc; ++j) {
                if (!make_block_arc(blk, i, j, arc)) continue;
                prob.arcs.push_back(arc);
                range.rc.emplace_back(static_cast<int32_t>(i), static_cast<int32_t>(j));
            }
        } else {
            const int32_t* const last = cand->row_end(i);
            for (const int32_t* j = cand->row_begin(i); j != last; ++j) {
                if (!make_block_arc(blk, i, *j, arc)) continue;
                prob.arcs.push_back(arc);
                range.rc.emplace_back(static_cast<int32_t>(i), *j);
            }
        }
    }

    range.n_arcs = static_cast<int64_t>(prob.arcs.size()) - range.first_arc;
    prob.block_arcs.push_back(std::move(range));
}

}  // namespace

void validate(const FlowProblem& prob) {
    const int64_t n_nodes = static_cast<int64_t>(prob.n_nodes);
    if (n_nodes < 0) {
        LAP_THROW_DIMENSION("FlowProblem: n_nodes is negative");
    }
    if (static_cast<int64_t>(prob.supply.size()) != n_nodes) {
        LAP_THROW_DIMENSION("FlowProblem: supply has " +
                            std::to_string(prob.supply.size()) +
                            " entries for " + std::to_string(n_nodes) + " nodes");
    }

    // Every unit that leaves a node enters another one, so the surpluses and
    // the deficits are the same total counted from both ends and the supplies
    // sum to zero. A problem violating this has no feasible flow for a reason
    // that is arithmetic rather than structural, so it is caught here instead
    // of being handed to the solver as an unsatisfiable demand.
    //
    // Accumulated in unsigned arithmetic, whose wraparound is defined, because
    // a signed overflow is undefined behaviour on exactly the oversized input
    // this check exists to reject. A nonzero total can only read as zero if it
    // is a multiple of 2^64, which no supply bounded by FLOW_INF_CAP reaches.
    unsigned long long balance = 0;
    for (const int64_t b : prob.supply) {
        balance += static_cast<unsigned long long>(b);
    }
    if (balance != 0ull) {
        LAP_THROW_DIMENSION("FlowProblem: supplies do not sum to zero");
    }

    for (std::size_t a = 0; a < prob.arcs.size(); ++a) {
        const FlowArc& arc = prob.arcs[a];
        if (arc.tail < 0 || static_cast<int64_t>(arc.tail) >= n_nodes) {
            LAP_THROW_DIMENSION("FlowProblem: " + arc_label(a) + " has tail " +
                                std::to_string(arc.tail) + " outside [0, " +
                                std::to_string(n_nodes) + ")");
        }
        if (arc.head < 0 || static_cast<int64_t>(arc.head) >= n_nodes) {
            LAP_THROW_DIMENSION("FlowProblem: " + arc_label(a) + " has head " +
                                std::to_string(arc.head) + " outside [0, " +
                                std::to_string(n_nodes) + ")");
        }
        if (arc.lower < 0) {
            LAP_THROW_DIMENSION("FlowProblem: " + arc_label(a) +
                                " has a negative lower bound");
        }
        if (arc.upper < arc.lower) {
            LAP_THROW_DIMENSION("FlowProblem: " + arc_label(a) + " has lower " +
                                std::to_string(arc.lower) + " above upper " +
                                std::to_string(arc.upper));
        }
        // A non-finite cost is not a forbidden arc. Forbidden is expressed by
        // omitting the arc, or by allowed(i, j) inside a block; an Inf or NaN
        // that reaches the residual search poisons a shortest-path distance
        // and then every potential derived from it.
        if (!std::isfinite(arc.cost)) {
            LAP_THROW_DIMENSION("FlowProblem: " + arc_label(a) +
                                " has a non-finite cost");
        }
    }

    std::vector<NodeSpan> spans;
    spans.reserve(prob.blocks.size() * 2u);
    for (std::size_t b = 0; b < prob.blocks.size(); ++b) {
        check_block(prob, b);
        const BipartiteBlock& blk = prob.blocks[b];
        const int64_t nr = blk.costs->nrow();
        const int64_t nc = blk.costs->ncol();
        if (blk.lower < 0 || blk.upper < blk.lower) {
            LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                                " has lower " + std::to_string(blk.lower) +
                                " above upper " + std::to_string(blk.upper));
        }
        if (blk.row_base < 0 || static_cast<int64_t>(blk.row_base) + nr > n_nodes) {
            LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                                " row nodes reach outside [0, " +
                                std::to_string(n_nodes) + ")");
        }
        if (blk.col_base < 0 || static_cast<int64_t>(blk.col_base) + nc > n_nodes) {
            LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) +
                                " column nodes reach outside [0, " +
                                std::to_string(n_nodes) + ")");
        }
        spans.push_back(NodeSpan{blk.row_base, blk.row_base + nr,
                                 static_cast<int64_t>(b)});
        spans.push_back(NodeSpan{blk.col_base, blk.col_base + nc,
                                 static_cast<int64_t>(b)});
    }

    // Sorted by start, an overlap can only be between neighbours, so the check
    // is O(B log B) rather than the quadratic sweep a blocked design with one
    // block per stratum would make expensive.
    spans.erase(std::remove_if(spans.begin(), spans.end(),
                               [](const NodeSpan& s) { return s.empty(); }),
                spans.end());
    std::sort(spans.begin(), spans.end(),
              [](const NodeSpan& x, const NodeSpan& y) {
                  return x.first < y.first;
              });
    for (std::size_t k = 1; k < spans.size(); ++k) {
        if (spans[k].first < spans[k - 1].last) {
            LAP_THROW_DIMENSION("FlowProblem: blocks " +
                                std::to_string(spans[k - 1].block) + " and " +
                                std::to_string(spans[k].block) +
                                " claim overlapping node ranges");
        }
    }

    if (!prob.warm_potential.empty() &&
        static_cast<int64_t>(prob.warm_potential.size()) != n_nodes) {
        LAP_THROW_DIMENSION("FlowProblem: warm_potential has " +
                            std::to_string(prob.warm_potential.size()) +
                            " entries for " + std::to_string(n_nodes) + " nodes");
    }

    // warm_flow is aligned to the arc array as it stands after expansion, so it
    // is only checkable once the arc array is final: either the blocks are
    // already expanded, or there were none to expand. Otherwise the solver
    // rechecks it after expand_blocks().
    if (!prob.warm_flow.empty() && (prob.expanded || prob.blocks.empty()) &&
        prob.warm_flow.size() != prob.arcs.size()) {
        LAP_THROW_DIMENSION("FlowProblem: warm_flow has " +
                            std::to_string(prob.warm_flow.size()) +
                            " entries for " + std::to_string(prob.arcs.size()) +
                            " arcs");
    }
}

void expand_blocks(FlowProblem& prob) {
    if (prob.expanded) return;

    // Blocks already carrying a range were expanded over a candidate set and
    // keep the arcs they have; only the rest are expanded here.
    const std::size_t first = prob.block_arcs.size();
    if (first > prob.blocks.size()) {
        LAP_THROW_DIMENSION("FlowProblem: " + std::to_string(first) +
                            " block ranges recorded for " +
                            std::to_string(prob.blocks.size()) + " blocks");
    }

    // The whole expansion is counted before a single arc is appended. Emitting
    // first and discovering the size later means the failure mode is a machine
    // in swap rather than an error a caller can act on, and the count that
    // matters is the dense one: a source may admit every pair.
    int64_t budget = static_cast<int64_t>(prob.arcs.size());
    for (std::size_t b = first; b < prob.blocks.size(); ++b) {
        check_block(prob, b);
        const int64_t nr = prob.blocks[b].costs->nrow();
        const int64_t nc = prob.blocks[b].costs->ncol();
        if (nr > 0 && nc > (FLOW_MAX_EXPANDED_ARCS - budget) / nr) {
            LAP_THROW_DIMENSION("FlowProblem: expanding the blocks needs more "
                                "than " + std::to_string(FLOW_MAX_EXPANDED_ARCS) +
                                " arcs");
        }
        budget += nr * nc;
    }

    prob.block_arcs.reserve(prob.blocks.size());
    for (std::size_t b = first; b < prob.blocks.size(); ++b) {
        expand_one_block(prob, b, nullptr);
    }

    prob.expanded = true;
}

void expand_block_subset(FlowProblem& prob, int32_t block, const CandidateSet& cand) {
    if (prob.expanded) {
        LAP_THROW_DIMENSION("FlowProblem: the problem is already expanded; "
                            "add_block_arcs() grows an expanded block");
    }
    const std::size_t b = static_cast<std::size_t>(block);
    if (block < 0) {
        LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(block) +
                            " is negative");
    }
    check_block(prob, b);
    if (b != prob.block_arcs.size()) {
        LAP_THROW_DIMENSION("FlowProblem: blocks expand in index order; block " +
                            std::to_string(b) + " comes after block " +
                            std::to_string(prob.block_arcs.size()));
    }

    const BipartiteBlock& blk = prob.blocks[b];
    if (cand.nrow() != blk.costs->nrow() || cand.ncol() != blk.costs->ncol()) {
        LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(b) + " is " +
                            std::to_string(blk.costs->nrow()) + " by " +
                            std::to_string(blk.costs->ncol()) +
                            " and its candidate set is " +
                            std::to_string(cand.nrow()) + " by " +
                            std::to_string(cand.ncol()));
    }
    check_budget(prob, cand.n_arcs());

    expand_one_block(prob, b, &cand);

    if (prob.block_arcs.size() == prob.blocks.size()) prob.expanded = true;
}

int64_t add_block_arcs(FlowProblem& prob, int32_t block,
                       const std::vector<std::pair<int32_t, int32_t>>& new_pairs) {
    if (!prob.expanded) {
        LAP_THROW_DIMENSION("FlowProblem: the problem is not expanded; "
                            "expand_blocks() or expand_block_subset() comes first");
    }
    const std::size_t b = static_cast<std::size_t>(block);
    if (block < 0) {
        LAP_THROW_DIMENSION("FlowProblem: block " + std::to_string(block) +
                            " is negative");
    }
    check_block(prob, b);

    // Alignment is checked before the arc array grows, so a warm start that was
    // already stale is named as stale rather than as one arc short.
    const bool warm = !prob.warm_flow.empty();
    if (warm && prob.warm_flow.size() != prob.arcs.size()) {
        LAP_THROW_DIMENSION("FlowProblem: warm_flow has " +
                            std::to_string(prob.warm_flow.size()) +
                            " entries for " + std::to_string(prob.arcs.size()) +
                            " arcs");
    }

    const BipartiteBlock& blk = prob.blocks[b];
    const int64_t nr = blk.costs->nrow();
    const int64_t nc = blk.costs->ncol();

    std::vector<FlowArc> fresh;
    std::vector<std::pair<int32_t, int32_t>> fresh_rc;
    fresh.reserve(new_pairs.size());
    fresh_rc.reserve(new_pairs.size());

    FlowArc arc;
    for (const std::pair<int32_t, int32_t>& p : new_pairs) {
        if (p.first < 0 || p.first >= nr || p.second < 0 || p.second >= nc) {
            LAP_THROW_DIMENSION("FlowProblem: pair (" + std::to_string(p.first) +
                                ", " + std::to_string(p.second) +
                                ") is outside block " + std::to_string(b) +
                                ", which is " + std::to_string(nr) + " by " +
                                std::to_string(nc));
        }
        if (!make_block_arc(blk, p.first, p.second, arc)) continue;
        fresh.push_back(arc);
        fresh_rc.push_back(p);
    }
    if (fresh.empty()) return 0;
    check_budget(prob, static_cast<int64_t>(fresh.size()));

    BlockArcRange& range = prob.block_arcs[b];
    const int64_t at    = range.first_arc + range.n_arcs;
    const int64_t added = static_cast<int64_t>(fresh.size());

    if (warm) {
        std::vector<int64_t> lower;
        lower.reserve(fresh.size());
        for (const FlowArc& a : fresh) lower.push_back(a.lower);
        prob.warm_flow.insert(prob.warm_flow.begin() +
                                  static_cast<std::ptrdiff_t>(at),
                              lower.begin(), lower.end());
    }
    prob.arcs.insert(prob.arcs.begin() + static_cast<std::ptrdiff_t>(at),
                     fresh.begin(), fresh.end());

    range.rc.insert(range.rc.end(), fresh_rc.begin(), fresh_rc.end());
    range.n_arcs += added;
    for (std::size_t k = b + 1u; k < prob.block_arcs.size(); ++k) {
        prob.block_arcs[k].first_arc += added;
    }

    return added;
}

}  // namespace lap
