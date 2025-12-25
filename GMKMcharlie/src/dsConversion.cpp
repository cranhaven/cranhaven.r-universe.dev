# include <Rcpp.h>
# include "h/macros.hpp"
using namespace Rcpp;


// [[Rcpp::export]]
List d2s(NumericMatrix X, double zero = 0,
         double threshold = 1e-16, bool verbose = true)
{
  int d = X.nrow();
  int N = X.ncol();
  double *x = &X[0];
  List rst(N);
  int n = std::max(1, N / 100), pk = 0;
  for(int i = 0; i < N; ++i)
  {
    if(verbose and i % n == 0)
    {
      Rcout << "\r" << pk << "%";
      ++pk;
    }
    double *a = x + i * INT(d);
    int Nnonzero = 0;
    for(int j = 0; j < d; ++j)
    {
      if(std::abs(a[j] - zero) <= threshold) continue;
      ++Nnonzero;
    }
    IntegerVector ind(Nnonzero);
    NumericVector loss(Nnonzero);
    int k = 0;
    for(int j = 0; j < d; ++j)
    {
      if(std::abs(a[j] - zero) <= threshold) continue;
      ind[k] = j + 1;
      loss[k] = a[j];
      ++k;
    }
    rst[i] = DataFrame::create(Named("index") = ind, Named("value") = loss);
  }
  if(verbose) Rcout << "\r100%\n";
  return rst;
}


// [[Rcpp::export]]
NumericMatrix s2d(List X, int d, double zero = 0, bool verbose = true)
{
  int N = X.size();
  NumericMatrix rst(d, N);
  std::fill(&rst[0], &*rst.end(), zero);
  double *x = &rst[0];
  int n = std::max(1, N / 100), pk = 0;
  for(int i = 0; i < N; ++i)
  {
    if(verbose and i % n == 0)
    {
      Rcout << "\r" << pk << "%";
      ++pk;
    }
    List a = X[i];
    IntegerVector region = a[0];
    NumericVector loss = a[1];
    int *r = &region[0];
    double *l = &loss[0];
    double *b = x + i * INT(d);
    for(int j = 0, jend = region.size(); j < jend; ++j)
      b[r[j] - 1] = l[j];
  }
  if(verbose) Rcout << "\r100%\n";
  return rst;
}
































