#include "jack.h"


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
template <typename numT>
numT schEval(
  std::vector<numT> x, Partition lambda, IntIntMap<numT> S, int m, int k, Partition nu
) {
  const int nusize = nu.size();
  if(nusize == 0 || nu[0] == 0 || m == 0) {
    return numT(1);
  }
  if(nusize > m && nu[m] > 0) {
    return numT(0);
  }
  if(m == 1){
    return ipow<numT>(x[0], nu[0]);
  }
  int N = _N(lambda, nu);
  std::pair<int, int> Nm = std::make_pair(N, m);
  if(auto search = S.find(Nm); search != S.end()) {
    return S[Nm];
  }
  numT s = schEval<numT>(x, lambda, S, m-1, 1, nu);
  int i = k;
  while(nusize >= i && nu[i-1] > 0) {
    if(nusize == i || nu[i-1] > nu[i]) {
      Partition _nu(nu);
      _nu[i-1] = nu[i-1] - 1;
      if(nu[i-1] > 1) {
        s = s + x[m-1] * schEval<numT>(x, lambda, S, m, i, _nu);
      } else {
        s = s + x[m-1] * schEval<numT>(x, lambda, S, m-1, 1, _nu);
      }
    }
    i++;
  }
  if(k == 1) {
    S[Nm] = s;
  }
  return s;
}

template <typename numT>
numT SchurEval(std::vector<numT> x, Partition lambda) {
  IntIntMap<numT> S;
  return schEval<numT>(x, lambda, S, x.size(), 1, lambda);
}

// [[Rcpp::export]]
std::string SchurEvalRcpp_gmpq(Rcpp::StringVector x, Rcpp::IntegerVector lambda) {
  int n = x.size();
  std::vector<gmpq> xQ;
  xQ.reserve(n);
  for(int i = 0; i < n; i++) {
    xQ.emplace_back(gmpq(Rcpp::as<std::string>(x[i])));
  }
  Partition lambdaP(lambda.begin(), lambda.end());
  gmpq result = SchurEval<gmpq>(xQ, lambdaP);
  return QSPRAY::utils::q2str(result);
}

// [[Rcpp::export]]
double SchurEvalRcpp_double(Rcpp::NumericVector x, Rcpp::IntegerVector lambda) {
  std::vector<double> xD(x.begin(), x.end());
  Partition lambdaP(lambda.begin(), lambda.end());
  return SchurEval<double>(xD, lambdaP);
}
