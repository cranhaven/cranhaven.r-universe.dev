#include <Rcpp.h>
#include "sampleW.h"
using namespace Rcpp;

IntegerVector SampleMatrixByColumn(NumericMatrix data, NumericVector r, IntegerVector dup) {
  int howmany = dup[0];
  int n = data.nrow();
  int nIndividuals = data.ncol();
  if (r.length() != nIndividuals * howmany) {
    Rprintf("The length of random number vector does not match the number of columns.");
    return 0;
  }
  IntegerVector samples(nIndividuals * howmany);
  IntegerVector duplicate(howmany);
  for (int i = 0; i < nIndividuals; i++) {
    samplew_multi2(data.begin() + i * n, n, r.begin() + i * howmany, duplicate.begin(),howmany);
    for (int j = 0; j < howmany; j++) {
      samples[j * nIndividuals + i] = duplicate[j];
    }

  }
  return samples;
}

NumericVector SampleMatrixByRow(NumericMatrix data, NumericVector r) {
  int nIndividuals = data.nrow();
  int n = data.ncol();
  if (r.length() != nIndividuals) {
    Rprintf("The length of random number vector does not match the number of columns.");
    return 0;
  }
  NumericVector samples(nIndividuals);
  NumericVector ps (n);
  for (int i = 0; i < nIndividuals; i++) {
    for (int j = 0; j <n; j++) {
      ps[j] = data(i,j);
    }
    samples[i] = samplew(ps.begin(), n,r[i]);
  }
  return samples;
}
