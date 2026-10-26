// src/core/lap_rcpp_convert.h
// The R shape of lap's proof objects.
//
// A certificate and a Hall witness are produced in three places now -- the
// verify_assignment() wrappers, the witness wrappers, and the edge-generation
// loop, which assembles both for itself and hands them across in one crossing.
// The R-facing field set of each is declared here and defined once beside the
// type it reports, so a caller reading a certificate cannot tell which wrapper
// built it.
#pragma once

#include <Rcpp.h>

#include "lap_certify.h"
#include "lap_hall.h"

// Every field of a CertificateReport, in the order verify_assignment()'s
// documentation lists them. Counts and indices cross as doubles: R has no
// 64-bit integer type and n_admissible on a 50,000 square problem is 2.5e9.
// Defined in lap_certify_rcpp.cpp.
Rcpp::List certificate_report_to_list(const lap::CertificateReport& rep);

// Shared by the assignment and flow certificates, both of which cross exact
// potentials to R as an n x K matrix whose row sums are the exact values, and
// take the arithmetic mode as the string R validated. Defined in
// lap_certify_rcpp.cpp.
lap::Arithmetic arithmetic_from_string(const std::string& name);
Rcpp::NumericMatrix expansions_to_r(const std::vector<lap::exact::Expansion>& e);
std::vector<lap::exact::Expansion> expansions_from_r(const Rcpp::NumericMatrix& m,
                                                     bool negate);
const char* exact_source_name(lap::ExactDualsSource s);

// The three fields carrying the cost unit go back in the caller's sign. A
// maximize instance is certified against -c with duals to match, so its
// objectives and gap come out negated. Feasibility flags, slackness measures
// and reduced costs are properties of the internal minimization and are
// reported unflipped. Defined in lap_certify_rcpp.cpp.
void restore_certificate_sign(lap::CertificateReport& rep, bool maximize);

// The deficient row set, the columns it reaches, and the maximum matching that
// found them, all 1-based. Defined in lap_hall_rcpp.cpp.
Rcpp::List hall_witness_to_list(const lap::DeficiencySet& witness);
