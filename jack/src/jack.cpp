#include "jack.h"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
template <typename T>
Qspray<T> jac(
  Partition lambda, IntIntMap<Qspray<T>> S, T alpha,
  int m, int k, Partition mu, Partition nu, T beta
) {
  const int nusize = nu.size();
  if(nusize == 0 || nu[0] == 0 || m == 0) {
    return Qspray<T>(T(1));
  }
  if(nusize > m && nu[m] > 0) {
    return Qspray<T>(T(0));
  }
  T oneT(1);
  if(m == 1) {
    T al(0);
    T prod(1);
    for(int i = 1; i < nu[0]; i++) {
      al += alpha;
      prod *= (al + oneT);
    }
    return Qspray<T>(T(prod)) * (Qlone<T>(1).power(nu[0]));
  }
  int N = _N(lambda, nu);
  std::pair<int, int> Nm = std::make_pair(N, m);
  if(k == 0) {
    if(auto search = S.find(Nm); search != S.end()) {
      return S[Nm];
    }
  }
  Qspray<T> s =
    jac(lambda, S, alpha, m-1, 0, nu, nu, oneT) * Qspray<T>(beta) *
      (Qlone<T>(m).power(weight(mu) - weight(nu)));
  int i = k > 1 ? k : 1;
  while(nusize >= i && nu[i-1] > 0) {
    if(nusize == i || nu[i-1] > nu[i]) {
      Partition _nu(nu);
      _nu[i-1] = nu[i-1] - 1;
      T gamma = beta * _betaratio<T>(mu, nu, i, alpha);
      if(nu[i-1] > 1) {
        s += jac(lambda, S, alpha, m, i, mu, _nu, gamma);
      } else {
        s += jac(lambda, S, alpha, m-1, 0, _nu, _nu, oneT) *
              Qspray<T>(gamma) * (Qlone<T>(m).power(weight(mu) - weight(_nu)));
      }
    }
    i++;
  }
  if(k == 0) {
    S[Nm] = s;
  }
  return s;
}

template <typename T>
Qspray<T> JackPol(int n, Partition lambda, T alpha) {
  IntIntMap<Qspray<T>> S;
  return jac(lambda, S, alpha, n, 0, lambda, lambda, T(1));
}

SymbolicQspray JackSymPol(int n, Partition lambda) {
  IntIntMap<SymbolicQspray> S;
  RatioOfQsprays<gmpq> alpha(Qlone<gmpq>(1), Qspray<gmpq>(gmpq(1)));
  return jac(lambda, S, alpha, n, 0, lambda, lambda, RatioOfQsprays<gmpq>(1));
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// [[Rcpp::export]]
Rcpp::List JackPolRcpp(int n, Rcpp::IntegerVector lambda, std::string alpha) {
  Partition lambdaP(lambda.begin(), lambda.end());
  gmpq alphaQ(alpha);
  Qspray<gmpq> P = JackPol<gmpq>(n, lambdaP, alphaQ);
  return returnQspray(P);
}

// [[Rcpp::export]]
Rcpp::List JackSymPolRcpp(int n, Rcpp::IntegerVector lambda) {
  Partition lambdaP(lambda.begin(), lambda.end());
  SymbolicQspray P = JackSymPol(n, lambdaP);
  return returnSymbolicQspray(P);
}

