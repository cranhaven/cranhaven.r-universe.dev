#include "jack.h"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
typedef Qspray<int> Zspray;
typedef IntIntMap<Zspray> Zij;

Zspray sch(Partition lambda, Zij S, int m, int k, Partition nu) {
  const int nusize = nu.size();
  if(nusize == 0 || nu[0] == 0 || m == 0) {
    return Zspray(1);
  }
  if(nusize > m && nu[m] > 0) {
    return Zspray(0);
  }
  if(m == 1){
    return Qlone<int>(1).power(nu[0]);
  }
  int N = _N(lambda, nu);
  std::pair<int, int> Nm = std::make_pair(N, m);
  if(auto search = S.find(Nm); search != S.end()) {
    return S[Nm];
  }
  Zspray s = sch(lambda, S, m-1, 1, nu);
  int i = k;
  while(nusize >= i && nu[i-1] > 0) {
    if(nusize == i || nu[i-1] > nu[i]) {
      Partition _nu(nu);
      _nu[i-1] = nu[i-1] - 1;
      if(nu[i-1] > 1) {
        s += Qlone<int>(m) * sch(lambda, S, m, i, _nu);
      } else {
        s += Qlone<int>(m) * sch(lambda, S, m-1, 1, _nu);
      }
    }
    i++;
  }
  if(k == 1) {
    S[Nm] = s;
  }
  return s;
}

Zspray SchurPol(int n, Partition lambda) {
  Zij S;
  return sch(lambda, S, n, 1, lambda);
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// [[Rcpp::export]]
Rcpp::List SchurPolRcpp(int n, Rcpp::IntegerVector lambda) {
  Partition lambdaP(lambda.begin(), lambda.end());
  Zspray S = SchurPol(n, lambdaP);
  Polynomial<int> P = S.get();
  Polynomial<gmpq> Q;
  for(auto it = P.begin(); it != P.end(); it++) {
    Q[it->first] = gmpq(it->second);
  }
  return returnQspray(Qspray<gmpq>(Q));
}
