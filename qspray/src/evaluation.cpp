#include "qspray.h"

using namespace QSPRAY;

// -------------------------------------------------------------------------- //
qcplx qxmult(qcplx z1, qcplx z2) {
  gmpq r1 = z1.real();
  gmpq i1 = z1.imag();
  gmpq r2 = z2.real();
  gmpq i2 = z2.imag();
  qcplx result(r1*r2 - i1*i2, r1*i2 + r2*i1);
  return result;
}


// -------------------------------------------------------------------------- //
qcplx qxpow(qcplx z, unsigned k) {
  qcplx result(gmpq("1"), gmpq("0"));
  while(k) {
    if(k & 1) {
      result = qxmult(result, z);
    }
    k >>= 1;
    z = qxmult(z, z);
  }
  return result;
}


// -------------------------------------------------------------------------- //
// [[Rcpp::export]]
Rcpp::StringVector evalQxspray(const Rcpp::List Powers,
                               const Rcpp::StringVector coeffs,
                               const Rcpp::StringVector v_re,
                               const Rcpp::StringVector v_im) {
  Qspray<gmpq> Q     = makeQspray(Powers, coeffs);
  Polynomial<gmpq> S = Q.get();

  std::vector<qcplx> v;
  int n = v_re.size();
  v.reserve(n);
  for(int i = 0; i < n; i++) {
    qcplx vi(gmpq(Rcpp::as<std::string>(v_re(i))),
             gmpq(Rcpp::as<std::string>(v_im(i))));
    v.emplace_back(vi);
  }

  powers pows;
  gmpq coef;

  qcplx result(gmpq(0), gmpq(0));

  for(auto it = S.begin(); it != S.end(); ++it) {
    pows = it->first;
    coef = it->second;
    qcplx term(coef, gmpq(0));
    int i = 0;
    for(auto ci = pows.begin(); ci != pows.end(); ++ci) {
      term = qxmult(term, qxpow(v[i++], *ci));
    }
    result += term;
  }

  return Rcpp::StringVector::create(
    QSPRAY::utils::q2str(result.real()), 
    QSPRAY::utils::q2str(result.imag())
  );
}
