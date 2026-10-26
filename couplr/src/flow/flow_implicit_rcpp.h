// src/flow/flow_implicit_rcpp.h
// The R shape of the edge-generation loop's record.
//
// One solve reports one loop; a design path reports one per point. Both cross
// with the same round record, the same match-vector convention and the same
// knobs, so those are declared here and defined once in flow_implicit_rcpp.cpp.
// What differs between the two -- what a solve reports beside the match, what a
// point reports beside it -- stays with its own caller.
#pragma once

#include <Rcpp.h>

#include "flow_implicit.h"

#include <cstdint>
#include <vector>

// A knob arrives as a double because R has no 64-bit integer type, and it is a
// count either way: a fractional one is a caller error rather than something to
// round into the nearest legal search.
int64_t implicit_knob_from_r(double v, const char* what);

lap::ImplicitOptions implicit_options_from_r(double keep_per_row, double width,
                                             double tol, double max_rounds,
                                             bool certify);

// Every row gets an entry whatever the loop decided, so a caller reads the
// match vector the same way on an answer and on a refusal. The loop leaves it
// empty when no master ever reached a matching, and 0 is unmatched.
Rcpp::IntegerVector implicit_match_to_r(const std::vector<int>& match, int64_t nrow);

// One row per round, as columns, which is the shape R reads as a data frame.
// Every count crosses as a double for the same reason the certificate's do.
Rcpp::List implicit_rounds_to_r(const std::vector<lap::ImplicitRound>& rounds,
                                bool maximize);
