//  groupcount.h
#include <Rcpp.h>
using namespace Rcpp;
IntegerMatrix groupcount(IntegerVector g1, IntegerVector g2, int n1, int n2);
IntegerVector groupcount1D(IntegerVector g, int n);
