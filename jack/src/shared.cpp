#include "jack.h"

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
int _N(Partition lambda, Partition mu) {
  size_t n = lambda.size();
  int out = mu[n-1];
  int product = 1;
  for(size_t i = n-1; i > 0; i--) {
    product *= lambda[i] + 1;
    out += mu[i-1] * product;
  }
  return out;
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
int weight(Partition mu) {
  int w = 0;
  int musize = mu.size();
  for(int i = 0; i < musize; i++) {
    w += mu[i];
  }
  return w;
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ //
template <typename T> // alpha = number or Qlone(1)/1
std::pair<T,T> _betaPQ(Partition kappa, Partition mu, int k, T alpha) {
  T t = T(k) - T(mu[k-1]) * alpha;
  std::vector<T> u;
  u.reserve(k);
  for(int i = 0; i < k; i++) {
    u.emplace_back(t - T(i) + T(kappa[i])*alpha);
  }
  std::vector<T> v;
  v.reserve(k-1);
  for(int i = 0; i < k-1; i++) {
    v.emplace_back(t - T(i+1) + T(mu[i])*alpha);
  }
  int musize = mu.size();
  int muk = mu[k-1];
  std::vector<T> w;
  w.reserve(muk-1);
  T al(0);
  for(int i = 1; i < muk; i++) {
    int j = 0;  // dual partition
    while(j < musize && mu[j] >= i) {
      j++;
    }
    al += alpha;
    w.emplace_back(T(j) - t - al);
  }
  T num1(1);
  T den1(1);
  T num2(1);
  T den2(1);
  T num3(1);
  T den3(1);
  for(int i = 0; i < k; i++) {
    num1 *= u[i];
    den1 *= u[i] + alpha - T(1);
  }
  for(int i = 0; i < k-1; i++) {
    num2 *= v[i] + alpha;
    den2 *= v[i];
  }
  for(int i = 0; i < muk-1; i++) {
    num3 *= w[i] + alpha;
    den3 *= w[i];
  }
  return std::pair<T,T>(alpha*num1*num2*num3, den1*den2*den3);
}

template <typename T>
T _betaratio(Partition kappa, Partition mu, int k, T alpha) {
  std::pair<T,T> PQ = _betaPQ<T>(kappa, mu, k, alpha);
  return PQ.first / PQ.second;
}

template gmpq   _betaratio<gmpq>(Partition, Partition, int, gmpq);
template double _betaratio<double>(Partition, Partition, int, double);

template <typename T>
RatioOfQsprays<T> _betaratio(Partition kappa, Partition mu, int k, RatioOfQsprays<T> alpha) {
  Qspray<T> alphaNumerator = alpha.getNumerator(); // assuming denominator=1
  std::pair<Qspray<T>,Qspray<T>> PQ = _betaPQ<Qspray<T>>(kappa, mu, k, alphaNumerator);
  Qspray<T> P = PQ.first;
  Qspray<T> Q = PQ.second;
  RATIOOFQSPRAYS::utils::simplifyFraction(P, Q);
  return RatioOfQsprays<T>(P, Q);
}

template RatioOfQsprays<gmpq> _betaratio<RatioOfQsprays<gmpq>>(Partition, Partition, int, RatioOfQsprays<gmpq>);
