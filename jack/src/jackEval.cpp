#include "jack.h"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
template <typename numT>
numT jacEval(
    std::vector<numT> x, Partition lambda, IntIntMap<numT> S, numT alpha,
    int m, int k, Partition mu, Partition nu, numT beta
) {
  const int nusize = nu.size();
  if(nusize == 0 || nu[0] == 0 || m == 0) {
    return numT(1);
  }
  if(nusize > m && nu[m] > 0) {
    return numT(0);
  }
  numT oneT(1);
  if(m == 1) {
    numT al(0);
    numT prod(1);
    for(int i = 1; i < nu[0]; i++) {
      al += alpha;
      prod *= (al + oneT);
    }
    return ipow<numT>(x[0], nu[0]) * prod;
  }
  int N = _N(lambda, nu);
  std::pair<int, int> Nm = std::make_pair(N, m);
  if(k == 0) {
    if(auto search = S.find(Nm); search != S.end()) {
      return S[Nm];
    }
  }
  numT s = jacEval<numT>(x, lambda, S, alpha, m-1, 0, nu, nu, oneT) * beta *
    ipow<numT>(x[m-1], weight(mu) - weight(nu));
  int i = k > 1 ? k : 1;
  while(nusize >= i && nu[i-1] > 0) {
    if(nusize == i || nu[i-1] > nu[i]) {
      Partition _nu(nu);
      _nu[i-1] = nu[i-1] - 1;
      numT gamma = beta * _betaratio<numT>(mu, nu, i, alpha);
      if(nu[i-1] > 1) {
        s = s + jacEval<numT>(x, lambda, S, alpha, m, i, mu, _nu, gamma);
      } else {
        s = s + jacEval<numT>(x, lambda, S, alpha, m-1, 0, _nu, _nu, oneT) *
          gamma * ipow<numT>(x[m-1], weight(mu) - weight(_nu));
      }
    }
    i++;
  }
  if(k == 0) {
    S[Nm] = s;
  }
  return s;
}

template <typename numT>
numT JackEval(std::vector<numT> x, Partition lambda, numT alpha) {
  IntIntMap<numT> S;
  numT oneT(1);
  return jacEval<numT>(x, lambda, S, alpha, x.size(), 0, lambda, lambda, oneT);
}

// [[Rcpp::export]]
std::string JackEvalRcpp_gmpq(
  Rcpp::StringVector x, Rcpp::IntegerVector lambda, std::string alpha
) {
  int n = x.size();
  std::vector<gmpq> xQ;
  xQ.reserve(n);
  for(int i = 0; i < n; i++) {
    xQ.emplace_back(gmpq(Rcpp::as<std::string>(x[i])));
  }
  Partition lambdaP(lambda.begin(), lambda.end());
  gmpq alphaQ(alpha);
  gmpq result = JackEval<gmpq>(xQ, lambdaP, alphaQ);
  return QSPRAY::utils::q2str(result);
}

// [[Rcpp::export]]
double JackEvalRcpp_double(
    Rcpp::NumericVector x, Rcpp::IntegerVector lambda, double alpha
) {
  std::vector<double> xD(x.begin(), x.end());
  Partition lambdaP(lambda.begin(), lambda.end());
  return JackEval<double>(xD, lambdaP, alpha);
}


// [[Rcpp::export]]
void test() {
  Rcpp::StringVector x = {"2", "3", "4", "5"};
  Rcpp::IntegerVector lambda = {3, 1};
  Rcpp::Rcout << JackEvalRcpp_gmpq(x, lambda, "5/2") << "\n";
  Rcpp::NumericVector y = {2, 3, 4, 5};
  Rcpp::Rcout << JackEvalRcpp_double(y, lambda, 2.5) << "\n";
}


