// -*- mode: C++; c-indent-level: 2; c-basic-offset: 4; indent-tabs-mode: nil; // -*-

#include "RcppEigen.h"
// [[Rcpp::depends(RcppEigen)]]
#include "Dico.h"

Dico DictParts(int m, int n) {
  std::unordered_map<int, int> D;
  Eigen::Matrix<int, Eigen::Dynamic, 3, Eigen::RowMajor> Last(1, 3);
  Last << 0, m, m;
  int fin = 0;
  for(int i = 0; i < n; i++) {
    Eigen::Matrix<int, Eigen::Dynamic, 3, Eigen::RowMajor> NewLast(0, 3);
    for(unsigned j = 0; j < Last.rows(); j++) {
      int manque = Last.coeff(j, 1);
      int l = std::min(manque, Last.coeff(j, 2));
      if(l > 0) {
        D[Last.coeff(j, 0)] = fin + 1;
        Eigen::Matrix<int, Eigen::Dynamic, 3, Eigen::RowMajor> x(l, 3);
        for(int r = 0; r < l; r++) {
          x(r, 0) = fin + r + 1;
          x(r, 1) = manque - r - 1;
          x(r, 2) = r + 1;
        }
        NewLast.conservativeResize(NewLast.rows() + l, Eigen::NoChange);
        NewLast.bottomRows(l) = x;
        fin += l;
      }
    }
    Last = NewLast;
  }
  Dico out;
  out.dict = D;
  out.last = fin;
  return out;
}

Eigen::ArrayXi cleanPart(Eigen::ArrayXi& kappa) {
  int n = kappa.size();
  if(n == 0){
    return {};
  }
  std::vector<int> vout(0);
  int i = 0;
  while(i < n && kappa.coeff(i) > 0) {
    vout.push_back(kappa.coeff(i));
    i++;
  }
  if(i == 0){
    return {};
  }
  int* ptr_data = &vout[0];
  return Eigen::Map<Eigen::ArrayXi, Eigen::Unaligned>(ptr_data, vout.size());
}

Eigen::ArrayXd dualPartition(Eigen::ArrayXi& kappa, int to = -1) {
  kappa = cleanPart(kappa);
  int l = kappa.size();
  if(l == 0) {
    return {};
  }
  int l0 = to == -1 ? kappa(0) : to;
  Eigen::ArrayXd out(l0);
  out(0) = (double)l;
  for(int i = 1; i < l0; i++) {
    int s = 0.0;
    for(int j = 0; j < l; j++) {
      if(kappa(j) > i) {
        s += 1.0;
      }
    }
    out(i) = s;
  }
  return out;
}

Eigen::ArrayXd sequence(int start, int end) {
  int lout = end - start + 1;
  Eigen::ArrayXd out(lout);
  for(int i = 0; i < lout; i++) {
    out(i) = (double)(i + start);
  }
  return out;
}

double betaratio(Eigen::ArrayXi& kappa,
                 Eigen::ArrayXi& mu,
                 int k,
                 double alpha) {
  double t = (double)k - alpha * (double)mu.coeff(k - 1);
  Eigen::ArrayXd v;
  if(k > 1) {
    Eigen::ArrayXd mu_dbl = mu.cast<double>();
    Eigen::ArrayXd ss = sequence(1, k - 1);
    v = alpha * mu_dbl.topRows(k - 1) - ss + t;
  } else {
    v = {0.0};
  }
  Eigen::ArrayXd u;
  if(k > 0) {
    Eigen::ArrayXd kappa_dbl = kappa.cast<double>();
    Eigen::ArrayXd sss = sequence(1, k);
    u = alpha * kappa_dbl.topRows(k) - sss + t + 1.0;
  } else {
    u = {0.0};
  }
  int l = mu(k - 1) - 1;
  Eigen::ArrayXd w;
  if(l > 0) {
    Eigen::ArrayXd muPrime = dualPartition(mu, l);
    Eigen::ArrayXd lrange = sequence(1, l);
    w = muPrime - alpha * lrange - t;
  } else {
    w = {0.0};
  }
  double prod1 = (u / (u + alpha - 1.0)).prod();
  double prod2 = ((v + alpha) / v).prod();
  double prod3 = ((w + alpha) / w).prod();
  return alpha * prod1 * prod2 * prod3;
}

template <typename T, typename R>
R T_(double alpha, T& a, T& b, Eigen::ArrayXi& kappa) {
  int lkappa = kappa.size();
  if(lkappa == 0 || kappa.coeff(0) == 0) {
    return 1.0;
  }
  lkappa -= 1;
  int kappai = kappa.coeff(lkappa);
  double kappai_dbl = (double)kappai;
  double i = (double)lkappa;
  double c = kappai_dbl - 1.0 - i / alpha;
  R prod1_den = (b + c).prod();
  if(prod1_den == 0.0) {
    return 0.0;
  }
  double d = kappai_dbl * alpha - i - 1.0;
  Eigen::ArrayXd e;
  if(kappai > 1) {
    Eigen::ArrayXd s = sequence(1, kappai - 1);
    Eigen::ArrayXd kappaPrime = dualPartition(kappa, kappai - 1);
    e = kappaPrime - alpha * s + d;
  } else {
    e = {0.0};
  }
  Eigen::ArrayXd g = e + 1.0;
  Eigen::ArrayXd f;
  if(lkappa > 0) {
    Eigen::ArrayXd kappa_dbl = kappa.topRows(lkappa).cast<double>();
    Eigen::ArrayXd ss = sequence(1, lkappa);
    f = alpha * kappa_dbl - ss - d;
  } else {
    f = {0.0};
  }
  Eigen::ArrayXd h = f + alpha;
  Eigen::ArrayXd l = h * f;
  R prod1_num = (a + c).prod();
  double prod2 = ((g - alpha) * e / g / (e + alpha)).prod();
  double prod3 = ((l - f) / (l + h)).prod();
  return prod1_num / prod1_den * prod2 * prod3;
}

template <typename U, typename S, typename R>
void jack(double alpha,
          S& x,
          std::unordered_map<int, int> dico,
          int k,
          R beta,
          int c,
          int t,
          Eigen::ArrayXi& mu,
          U& jarray,
          Eigen::ArrayXi& kappa,
          int nkappa) {
  int i0 = k > 1 ? k : 1;
  int i1 = mu.size();
  for(int i = i0; i <= i1; i++) {
    int u = mu.coeff(i - 1);
    if(mu.size() == i || u > mu.coeff(i)) {
      R gamma = beta * betaratio(kappa, mu, i, alpha);
      Eigen::ArrayXi muP = mu;  // clone(mu);
      muP(i - 1) = u - 1;
      muP = cleanPart(muP);
      int nmuP = 0;
      for(int j = 0; j < muP.size(); j++) {
        nmuP = dico.at(nmuP) + muP.coeff(j) - 1;
      }
      if(muP.size() >= i && u > 1) {
        jack<U, S, R>(alpha, x, dico, i, gamma, c + 1, t, muP, jarray, kappa,
                      nkappa);
      } else {
        if(nkappa > 1) {
          if(muP.size() > 0) {
            jarray(nkappa - 1, t - 1) += gamma * jarray.coeff(nmuP - 1, t - 2) *
                                         pow(x.coeff(t - 1), c + 1);
          } else {
            jarray(nkappa - 1, t - 1) += gamma * pow(x.coeff(t - 1), c + 1);
          }
        }
      }
    }
  }
  if(k == 0) {
    if(nkappa > 1) {
      jarray(nkappa - 1, t - 1) += jarray.coeff(nkappa - 1, t - 2);
    }
  } else {
    int nmu = 0;
    for(int i = 0; i < mu.size(); i++) {
      nmu = dico.at(nmu) + mu.coeff(i) - 1;
    }
    jarray(nkappa - 1, t - 1) +=
        beta * pow(x.coeff(t - 1), c) * jarray.coeff(nmu - 1, t - 2);
  }
}

template <typename U,
          typename T,
          typename S,
          typename Rs,
          typename Rj,
          typename Rt>  // U: Complex/NumericMatrix
Rs summation(T& a,
             T& b,
             S& x,
             std::unordered_map<int, int> dico,
             int n,
             double alpha,
             int i,
             Rs z,
             int j,
             Eigen::ArrayXi& kappa,
             U& jarray) {
  if(i == n) {
    return Rs(0);
  }
  int lkappa = kappa.size();
  int lkappaP = lkappa + 1;
  int kappai = 1;
  Rs s(0);
  while((i > 0 || kappai <= j) &&
        (i == 0 ||
         ((lkappa == 0 || kappai <= kappa.coeff(lkappa - 1)) && kappai <= j))) {
    Eigen::ArrayXi kappaP(lkappa + 1);
    for(int k = 0; k < lkappa; k++) {
      kappaP(k) = kappa.coeff(k);
    }
    kappaP(lkappa) = kappai;
    int nkappaP = 0;
    for(int k = 0; k < lkappaP; k++) {
      nkappaP = dico.at(nkappaP) + kappaP.coeff(k) - 1;
    }
    z = z * T_<T, Rt>(alpha, a, b, kappaP);
    if(nkappaP > 1 && (lkappaP == 1 || kappaP.coeff(1) == 0)) {
      jarray(nkappaP - 1, 0) = x.coeff(0) *
                               (1.0 + alpha * (kappaP.coeff(0) - 1)) *
                               jarray.coeff(nkappaP - 2, 0);
    }
    for(int t = 2; t <= n; t++) {
      jack<U, S, Rj>(alpha, x, dico, 0, 1.0, 0, t, kappaP, jarray, kappaP,
                     nkappaP);
    }
    s += z * jarray.coeff(nkappaP - 1, n - 1);
    if(j > kappai && i <= n) {
      s += summation<U, T, S, Rs, Rj, Rt>(a, b, x, dico, n, alpha, i + 1, z,
                                          j - kappai, kappaP, jarray);
    }
    kappai += 1;
  }
  return s;
}

template <typename T, typename S, typename R, typename Rt>
R summationI(T& a,
             T& b,
             S x,
             int n,
             double alpha,
             int i,
             R z,
             int j,
             Eigen::ArrayXi& kappa) {
  int lkappa = kappa.size();
  int kappai = 1;
  R s(0);
  while((i > 0 || kappai <= j) &&
        (i == 0 || (kappai <= kappa.coeff(i - 1) && kappai <= j))) {
    Eigen::ArrayXi kappaP(lkappa + 1);
    for(int k = 0; k < lkappa; k++) {
      kappaP(k) = kappa.coeff(k);
    }
    kappaP(lkappa) = kappai;
    Rt t = T_<T, Rt>(alpha, a, b, kappaP);
    z = z * x * t * ((double)(n - i) + alpha * (double)(kappai - 1));
    if(j > kappai && i <= n) {
      s += summationI<T, S, R, Rt>(a, b, x, n, alpha, (i + 1), z, (j - kappai),
                                   kappaP);
    }
    s += z;
    kappai += 1;
  }
  return s;
}

template <typename T, typename S, typename R, typename Rt>
R hypergeoI(int m, double alpha, T& a, T& b, int n, S x) {
  Eigen::ArrayXi emptyPart = {};
  return 1.0 +
         summationI<T, S, R, Rt>(a, b, x, n, alpha, 0, R(1), m, emptyPart);
}

template <typename U,
          typename T,
          typename S,
          typename Sx,
          typename Rs,
          typename Rj,
          typename Rt>
Rs hypergeom(int m, T& a, T& b, S& x, double alpha) {
  int n = x.size();
  bool xconst = true;
  int i = 1;
  while(xconst && i < n) {
    xconst = x.coeff(i) == x.coeff(0);
    i++;
  }
  if(xconst) {
    return hypergeoI<T, Sx, Rs, Rt>(m, alpha, a, b, x.size(), x.coeff(0));
  }
  Dico dict = DictParts(m, n);
  U jarray(dict.last, n);
  jarray = 0;
  //  jarray.fill(0);
  S xx(n);
  xx(0) = x.coeff(0);
  for(int i = 1; i < n; i++) {
    xx(i) = xx.coeff(i - 1) + x.coeff(i);
  }
  //  S xx = arma::cumsum(x);
  for(int j = 0; j < n; j++) {
    jarray(0, j) = xx.coeff(j);
  }
  Eigen::ArrayXi emptyPart = {};
  Rs s = summation<U, T, S, Rs, Rj, Rt>(a, b, x, dict.dict, n, alpha, 0,
                                        (Rs)1.0, m, emptyPart, jarray);
  return (Rs)1.0 + s;
}

// [[Rcpp::export]]
std::complex<double> hypergeom_Cplx_Cplx(int m,
                                         Eigen::ArrayXcd& a,
                                         Eigen::ArrayXcd& b,
                                         Eigen::ArrayXcd& x,
                                         double alpha) {
  return hypergeom<Eigen::ArrayXXcd, Eigen::ArrayXcd, Eigen::ArrayXcd,
                   std::complex<double>, std::complex<double>,
                   std::complex<double>, std::complex<double>>(m, a, b, x,
                                                               alpha);
}

// [[Rcpp::export]]
double hypergeom_R_R(int m,
                     Eigen::ArrayXd& a,
                     Eigen::ArrayXd& b,
                     Eigen::ArrayXd& x,
                     double alpha) {
  return hypergeom<Eigen::ArrayXXd, Eigen::ArrayXd, Eigen::ArrayXd, double,
                   double, double, double>(m, a, b, x, alpha);
}

// [[Rcpp::export]]
std::complex<double> hypergeom_Cplx_R(int m,
                                      Eigen::ArrayXcd& a,
                                      Eigen::ArrayXcd& b,
                                      Eigen::ArrayXd& x,
                                      double alpha) {
  return hypergeom<Eigen::ArrayXXd, Eigen::ArrayXcd, Eigen::ArrayXd, double,
                   std::complex<double>, double, std::complex<double>>(
      m, a, b, x, alpha);
}

// [[Rcpp::export]]
std::complex<double> hypergeom_R_Cplx(int m,
                                      Eigen::ArrayXd& a,
                                      Eigen::ArrayXd& b,
                                      Eigen::ArrayXcd& x,
                                      double alpha) {
  return hypergeom<Eigen::ArrayXXcd, Eigen::ArrayXd, Eigen::ArrayXcd,
                   std::complex<double>, std::complex<double>,
                   std::complex<double>, double>(m, a, b, x, alpha);
}
