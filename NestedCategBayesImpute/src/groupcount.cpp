#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerMatrix groupcount(IntegerVector g1, IntegerVector g2, int n1, int n2) {

  IntegerMatrix counts(n1, n2);
  for (int i = 0; i < g1.length(); i++) {
    counts[(g1[i] -1) + (g2[i]-1) * n1 ]++;
  }
  return counts;
}

// [[Rcpp::export]]
IntegerVector groupcount1D(IntegerVector g, int n) {
  IntegerVector counts(n);
  for (int i = 0; i < g.length(); i++) {
    counts[(g[i]-1)]++;
  }
  return counts;
}
