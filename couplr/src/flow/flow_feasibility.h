// src/flow/flow_feasibility.h
// Reaching a feasible restricted master, and knowing when there is none.
// Pure C++ - NO Rcpp dependencies, same rule as lap_types.h.
//
// Pricing to optimality and reaching feasibility are different tasks. The pricer
// wants the pairs a dual solution would have used; a master that came back short
// of the required flow has no such solution to price with, and the answer it
// needs is which rows could not be matched at all. Doubling a candidate width
// until the shortfall goes away answers that by exhausting a ladder, and reports
// genuine infeasibility only by running out of rungs.
//
// Hall's condition is the answer instead. A row-perfect matching over the
// restricted arc set exists exactly when every row set S reaches |S| distinct
// columns, and lap_hall.h returns the S that fails together with N(S), by
// Hopcroft-Karp and the Koenig alternating-reachability cut. One round is:
//
//   1. Match over the *restricted* graph -- the candidate pairs the cost source
//      admits. Row-perfect, and this block is feasible.
//   2. Otherwise S and N(S) come back. Re-seed those rows, and only those: a row
//      that is already matched does not pay for a row that is not.
//   3. The re-seed reads the *full* source over the deficient rows. When it
//      finds no admissible column outside N(S), N(S) is the whole neighbourhood
//      of S in the complete implicit problem, |S| > |N(S)| holds there too, and
//      the answer is infeasible with a certificate rather than a wider ladder.
//
// Step 3 is O(|S| * ncol), which is affordable exactly when the deficient set is
// small, and a problem where it is not small is not one edge generation should
// be running.
//
// **The re-seed takes columns outside N(S) only.** Every restricted edge out of
// S lands inside N(S) -- that is what makes (S, N(S)) a witness -- so a matching
// of S is bounded by |N(S)| however many arcs into N(S) are added. Arcs into
// N(S) are the pricer's business, arcs out of it are the only ones that move the
// deficiency, and a re-seed that could not tell them apart would spend rounds
// adding pairs that cannot repair anything.
//
// Scope is the assignment block: rows carrying one unit each, which is the
// question Hall's condition and Hopcroft-Karp answer. A block whose rows demand
// several units -- a ratio match, a full match -- fails feasibility under a
// different condition and is not this header's.
#pragma once

#include "../core/lap_cost_source.h"
#include "../core/lap_error.h"
#include "../core/lap_hall.h"
#include "flow_candidates.h"
#include "flow_row_search.h"
#include "flow_topk.h"

#include <cstddef>
#include <cstdint>
#include <string>
#include <utility>
#include <vector>

namespace lap {

// The restricted problem as a cost source: the pairs the candidate set holds,
// less the ones the cost source forbids, priced by the source itself. It names
// its rows' columns, so a matching or a scan over it costs the candidates
// rather than the grid; a candidate the source forbids is in the range and
// fails allowed(), which is the superset for_each_allowed() is written for.
//
// It is the same problem a restricted master solves, which is what makes it
// both the graph Hall's witness is asked about and the source a certificate
// over the master's own pairs is taken against.
template <class Source>
struct CandidateGraph {
    const Source*       src  = nullptr;
    const CandidateSet* cand = nullptr;
    int64_t nrow = 0;
    int64_t ncol = 0;

    CandidateGraph(const Source& source, const CandidateSet& candidates)
        : src(&source), cand(&candidates), nrow(source.nrow), ncol(source.ncol) {}

    bool allowed(int64_t i, int64_t j) const {
        return cand->contains(i, j) && src->allowed(i, j);
    }
    double at(int64_t i, int64_t j) const { return src->at(i, j); }

    // Membership first, so a pair outside the set never reaches the source and
    // a computing source is not asked for a cost this graph does not carry.
    bool admissible(int64_t i, int64_t j, double& cost) const {
        return cand->contains(i, j) && cost_if_allowed(*src, i, j, cost);
    }

    const int32_t* allowed_begin(int64_t i) const { return cand->row_begin(i); }
    const int32_t* allowed_end(int64_t i) const { return cand->row_end(i); }
};

struct FeasibilityRound {
    enum class Status {
        feasible,    // the restricted arc set admits a row-perfect matching
        reseeded,    // deficient rows gained columns; the master is solved again
        infeasible   // no arc set over this source admits one, and here is why
    };

    Status status = Status::infeasible;

    // Over the restricted graph: the maximum matching, and when it leaves rows
    // unmatched, the deficient set S, N(S) and lap_hall.h's own re-check of
    // them. Empty rows and columns exactly when the status is `feasible`.
    DeficiencySet witness;

    // The witness re-checked against the full source, which is the claim an
    // `infeasible` status makes. False on every other status: there is no claim
    // about the complete problem to check.
    bool certified = false;

    // What the re-seed put into the candidate set, ascending by (i, j), as
    // add_block_arcs() takes it.
    std::vector<CandidateSet::Pair> added;

    // Pairs of a deficient row and a column outside N(S), which is the work
    // step 3 does, and how many of them the source admitted.
    int64_t n_scanned = 0;
    int64_t n_evaluated = 0;
};

// One feasibility round over `cand`, which grows by at most `width` columns per
// deficient row.
//
// `width` is the caller's ladder: the loop that doubles it doubles it for the
// deficient rows alone, because that is the only place it is spent. It has to be
// at least one -- a round that adds nothing hands the caller back the same
// deficiency and the same round to run again -- so a non-positive width throws
// rather than looping.
//
// `search` answers step 3 for the source it was built over, and a caller that
// runs several rounds passes the same one so whatever structure it holds is
// built once.
template <class Source>
FeasibilityRound feasibility_round(const Source& src, CandidateSet& cand, int width,
                                   RowSearch<Source>& search) {
    const int64_t nrow = src.nrow;
    const int64_t ncol = src.ncol;

    if (cand.nrow() != nrow || cand.ncol() != ncol) {
        LAP_THROW_DIMENSION("feasibility_round: candidate set is " +
                            std::to_string(cand.nrow()) + " x " +
                            std::to_string(cand.ncol()) + ", source is " +
                            std::to_string(nrow) + " x " + std::to_string(ncol));
    }
    if (width < 1) {
        LAP_THROW_DIMENSION("feasibility_round: width " + std::to_string(width) +
                            " adds no column to a deficient row, so the round "
                            "cannot change the deficiency it reports");
    }

    FeasibilityRound out;
    out.witness = hall_witness(CandidateGraph<Source>(src, cand));
    if (out.witness.row_perfect) {
        out.status = FeasibilityRound::Status::feasible;
        return out;
    }

    // N(S) as a mark, so the scan can tell a column that would move the
    // deficiency from one the deficient rows already reach.
    std::vector<char> in_cols(static_cast<std::size_t>(ncol > 0 ? ncol : 0),
                              static_cast<char>(0));
    for (int64_t j : out.witness.cols) in_cols[static_cast<std::size_t>(j)] = 1;

    const std::vector<int64_t>& rows = out.witness.rows;
    detail::RowTopK keep(static_cast<int64_t>(rows.size()), width);

    // Cheapest first, which is the k-nearest seed the ladder was standing in
    // for, taken over the columns that can repair the deficiency.
    RowScanWork work;
    for (std::size_t t = 0; t < rows.size(); ++t) {
        search.cheapest_outside(src, rows[t], in_cols, keep,
                                static_cast<int64_t>(t), work);
    }
    out.n_scanned = work.n_scanned;
    out.n_evaluated = work.n_evaluated;
    cand.note_evaluated(out.n_evaluated);

    std::vector<CandidateSet::Pair> want;
    keep.emit([&](int32_t t, int32_t j, double) {
        want.emplace_back(static_cast<int32_t>(rows[static_cast<std::size_t>(t)]), j);
    });

    if (want.empty()) {
        // Nothing outside N(S) is admissible to any row of S, so the witness the
        // restricted graph produced is a witness for the complete problem. It is
        // re-checked against the full source rather than inferred from the scan
        // that just ran, which is the same discipline hall_witness() applies to
        // its own output.
        out.status = FeasibilityRound::Status::infeasible;
        out.certified = hall_detail::verify_witness(src, out.witness.rows, out.witness.cols);
        return out;
    }

    out.added = cand.add_pairs(want);
    out.status = FeasibilityRound::Status::reseeded;
    return out;
}

// One round against a search built for this call alone, for a caller running a
// single round.
template <class Source>
FeasibilityRound feasibility_round(const Source& src, CandidateSet& cand, int width) {
    RowSearch<Source> search(src);
    return feasibility_round(src, cand, width, search);
}

}  // namespace lap
