#include <boost/multiprecision/gmp.hpp>
#include "RcppArmadillo.h"
#include "roptim.h"
using namespace roptim;
namespace mp = boost::multiprecision;

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(roptim)]]
// [[Rcpp::depends(BH)]]

const double Epsilon = pow(std::numeric_limits<double>::epsilon(), 0.5);

arma::vec from01(const arma::vec& u) {
  return arma::log(u / (1.0 - u));
}

double from01scalar(double u) {
  return log(u / (1.0 - u));
}

double to01(double x) {
  return 1.0 / (1.0 + exp(-x));
}

double dfrom01(double u) {
  return 1.0 / (u * (1.0 - u));
}

arma::vec dlogis(const arma::vec& x) {
  const arma::vec expminusx = arma::exp(-x);
  const arma::vec one_plus_expminusx = 1.0 + expminusx;
  return expminusx / (one_plus_expminusx % one_plus_expminusx);
}

arma::vec ldlogis(const arma::vec& x) {
  return -x - 2.0 * arma::log1p(arma::exp(-x));
}
arma::vec dldlogis(const arma::vec& x) {
  return 1.0 - 2.0 / (1.0 + arma::exp(-x));
}

double forig(const arma::vec& x, const arma::mat& P, const arma::vec& b) {
  return arma::prod(dlogis(P * x + b));
}

double f(const arma::vec& u, const arma::mat& P, const arma::vec& b) {
  double result = arma::prod(dlogis(P * from01(u) + b));
  return std::isnormal(result) ? result : 0.0;
  //  return arma::prod(dlogis(P * from01(u) + b));
}

double logf(const arma::vec& u, const arma::mat& P, const arma::vec& b) {
  return arma::sum(ldlogis(P * from01(u) + b));
}

double df(const double ui,
          const arma::vec& Pi,
          const double y1,
          const arma::vec& y2) {
  //  return y1 * dfrom01(ui) * arma::sum(Pi % y2);
  double result = y1 * dfrom01(ui) * arma::sum(Pi % y2);
  return std::isnormal(result) ? result : 0.0;
}

double dlogf(const double ui, const arma::vec& Pi, const arma::vec& y2) {
  return dfrom01(ui) * arma::sum(Pi % y2);
}

class F : public Functor {
 public:
  arma::mat P;
  arma::vec b;
  double operator()(const arma::vec& u) override { return logf(u, P, b); }
  void Gradient(const arma::vec& u, arma::vec& gr) override {
    const size_t d = P.n_cols;
    gr = arma::zeros<arma::vec>(d);
    const arma::vec y2 = dldlogis(P * from01(u) + b);
    for(size_t i = 0; i < d; i++) {
      gr(i) = dlogf(u.at(i), P.col(i), y2);
    }
  }
};

class xF : public Functor {
 public:
  arma::mat P;
  arma::vec b;
  arma::vec mu;
  size_t j;
  double operator()(const arma::vec& u) override {
    const size_t d = P.n_cols;
    const double result =
        pow(f(u, P, b), 1.0 / (d + 2)) * (from01scalar(u.at(j)) - mu.at(j));
    return std::isnormal(result) ? result : 0.0;
  }
  void Gradient(const arma::vec& u, arma::vec& gr) override {
    const size_t d = P.n_cols;
    const double alpha = 1.0 / (d + 2);
    gr = arma::zeros<arma::vec>(d);
    const double y1alpha = pow(f(u, P, b), alpha);
    const arma::vec y2 = dldlogis(P * from01(u) + b);
    const double diff = from01scalar(u.at(j)) - mu.at(j);
    for(size_t i = 0; i < d; i++) {
      const double z = y1alpha * dfrom01(u.at(i));
      double result;
      if(i == j) {
        result = z * (alpha * arma::sum(P.col(i) % y2) * diff + 1.0);
      } else {
        result = alpha * z * arma::sum(P.col(i) % y2) * diff;
      }
      gr(i) = std::isnormal(result) ? result : 0.0;
    }
  }
};

// [[Rcpp::export]]
Rcpp::List get_umax(const arma::mat& P,
                    const arma::vec& b,
                    arma::vec& init,
                    const double ufactr) {
  F optimand;
  optimand.P = P;
  optimand.b = b;
  const size_t d = P.n_cols;
  Roptim<F> opt("L-BFGS-B");
  opt.control.trace = 0;
  opt.control.maxit = 10000;
  opt.control.fnscale = -1.0;  // maximize
  opt.control.factr = ufactr;
  //  opt.control.pgtol = 1.0e-10;
  opt.control.lmm = 10;
  opt.set_hessian(false);
  arma::vec lwr = arma::zeros(d) + Epsilon;
  arma::vec upr = arma::ones(d) - Epsilon;
  opt.set_lower(lwr);
  opt.set_upper(upr);
  opt.minimize(optimand, init);
  if(opt.convergence() != 0) {
    Rcpp::Rcout << "-- umax -----------------------" << std::endl;
    opt.print();
  }
  return Rcpp::List::create(
      Rcpp::Named("mu") = from01(opt.par()),
      Rcpp::Named("umax") = pow(exp(opt.value()), 2.0 / (d + 2)));
}

// [[Rcpp::export]]
double get_vmin_i(const arma::mat& P,
                  const arma::vec& b,
                  const size_t i,
                  const arma::vec& mu,
                  const double vfactr) {
  xF optimand;
  optimand.P = P;
  optimand.b = b;
  optimand.j = i;
  optimand.mu = mu;
  Roptim<xF> opt("L-BFGS-B");
  opt.control.trace = 0;
  opt.control.maxit = 10000;
  // opt.control.fnscale = 1.0;  // minimize
  opt.control.factr = vfactr;
  opt.control.lmm = 20;
  opt.set_hessian(false);
  const size_t d = P.n_cols;
  arma::vec init = 0.5 * arma::ones(d);
  init.at(i) = to01(mu.at(i)) / 2.0;
  arma::vec lwr = arma::zeros(d) + Epsilon;
  arma::vec upr = arma::ones(d) - Epsilon;
  upr.at(i) = to01(mu.at(i));
  opt.set_lower(lwr);
  opt.set_upper(upr);
  opt.minimize(optimand, init);
  if(opt.convergence() != 0) {
    Rcpp::Rcout << "-- vmin -----------------------" << std::endl;
    opt.print();
  }
  return opt.value();
}

// [[Rcpp::export]]
arma::vec get_vmin(const arma::mat& P,
                   const arma::vec& b,
                   const arma::vec& mu,
                   const double vfactr) {
  const size_t d = P.n_cols;
  arma::vec vmin(d);
  for(size_t i = 0; i < d; i++) {
    vmin.at(i) = get_vmin_i(P, b, i, mu, vfactr);
  }
  return vmin;
}

double get_vmax_i(const arma::mat& P,
                  const arma::vec& b,
                  const size_t i,
                  const arma::vec& mu,
                  const double vfactr) {
  xF optimand;
  optimand.P = P;
  optimand.b = b;
  optimand.j = i;
  optimand.mu = mu;
  Roptim<xF> opt("L-BFGS-B");
  opt.control.trace = 0;
  opt.control.maxit = 10000;
  opt.control.fnscale = -1.0;  // maximize
  opt.control.factr = vfactr;
  opt.control.lmm = 20;
  opt.set_hessian(false);
  const size_t d = P.n_cols;
  arma::vec init = 0.5 * arma::ones(d);
  init.at(i) = (to01(mu.at(i)) + 1.0) / 2.0;
  arma::vec lwr = arma::zeros(d) + Epsilon;
  lwr.at(i) = to01(mu.at(i));
  arma::vec upr = arma::ones(d) - Epsilon;
  opt.set_lower(lwr);
  opt.set_upper(upr);
  opt.minimize(optimand, init);
  if(opt.convergence() != 0) {
    Rcpp::Rcout << "-- vmax -----------------------" << std::endl;
    opt.print();
  }
  return opt.value();
}

// [[Rcpp::export]]
arma::vec get_vmax(const arma::mat& P,
                   const arma::vec& b,
                   const arma::vec& mu,
                   const double vfactr) {
  const size_t d = P.n_cols;
  arma::vec vmax(d);
  for(size_t i = 0; i < d; i++) {
    vmax.at(i) = get_vmax_i(P, b, i, mu, vfactr);
  }
  return vmax;
}

// [[Rcpp::export]]
Rcpp::List get_bounds(const arma::mat& P,
                      const arma::vec& b,
                      arma::vec& init,
                      const double ufactr,
                      const double vfactr) {
  const Rcpp::List L = get_umax(P, b, init, ufactr);
  const arma::vec mu = L["mu"];
  const double umax = L["umax"];
  const arma::vec vmin = get_vmin(P, b, mu, vfactr);
  const arma::vec vmax = get_vmax(P, b, mu, vfactr);
  return Rcpp::List::create(Rcpp::Named("umax") = umax, Rcpp::Named("mu") = mu,
                            Rcpp::Named("vmin") = vmin,
                            Rcpp::Named("vmax") = vmax);
}

// [[Rcpp::export]]
arma::mat rcd(const size_t n,
              const arma::mat& P,
              const arma::vec& b,
              arma::vec& init,
              const double ufactr,
              const double vfactr) {
  const size_t d = P.n_cols;
  arma::mat tOut(d, n);
  const Rcpp::List bounds = get_bounds(P, b, init, ufactr, vfactr);
  const double umax = bounds["umax"];
  const arma::vec mu = bounds["mu"];
  const arma::vec vmin = bounds["vmin"];
  const arma::vec vmax = bounds["vmax"];
  size_t k = 0;
  while(k < n) {
    const double u = R::runif(0.0, umax);
    arma::vec v(d);
    for(size_t i = 0; i < d; i++) {
      v.at(i) = R::runif(vmin.at(i), vmax.at(i));
    }
    const arma::vec x = v / sqrt(u) + mu;
    if(u < pow(forig(x, P, b), 2.0 / (d + 2))) {
      tOut.col(k) = x;
      k++;
    }
  }
  return tOut.t();
}

////////////////////////////////////////////////////////////////////////////////
double plogis(double x) {
  return 1.0 / (1.0 + exp(-x));
}

double qlogis(double u) {
  return log(u / (1.0 - u));
}

const double MachineEps = std::numeric_limits<double>::epsilon();

double rtlogis1(double x) {
  double b = plogis(x);
  if(b == 0.0) {
    Rcpp::Rcout << "b = 0\n";
    return x;
  }
  double u = R::runif(0.0, b);
  return qlogis(u);
}

double rtlogis2(const double x) {
  double a = plogis(x);
  if(a == 1.0) {
    Rcpp::Rcout << "a = 1\n";
    return x;
  }
  double u = R::runif(a, 1.0);
  return qlogis(u);
}

std::string scalar2q(double x) {
  mp::mpq_rational q(x);
  return q.convert_to<std::string>();
}

Rcpp::CharacterVector vector2q(arma::colvec& x) {
  Rcpp::CharacterVector out(x.size());
  for(auto i = 0; i < x.size(); i++) {
    mp::mpq_rational q(x(i));
    out(i) = q.convert_to<std::string>();
  }
  return out;
}

Rcpp::CharacterVector newColumn(const arma::colvec& Xt,
                                double atilde,
                                const bool yzero) {
  arma::colvec head;
  arma::colvec newcol;
  if(yzero) {
    head = {0.0, atilde};
    newcol = arma::join_vert(head, -Xt);
  } else {
    head = {0.0, -atilde};
    newcol = arma::join_vert(head, Xt);
  }
  return vector2q(newcol);
}  // add column then transpose:

Rcpp::CharacterMatrix addHin(Rcpp::CharacterMatrix H,
                             const arma::colvec& Xt,
                             double atilde,
                             const bool yzero) {
  Rcpp::CharacterMatrix Ht = Rcpp::transpose(H);
  Rcpp::CharacterVector newcol = newColumn(Xt, atilde, yzero);
  Rcpp::CharacterMatrix Hnew = Rcpp::transpose(Rcpp::cbind(Ht, newcol));
  Hnew.attr("representation") = "H";
  return Hnew;
}

// [[Rcpp::export]]
Rcpp::List loop1(const Rcpp::List H,
                 const Rcpp::List Points,
                 const int y,
                 const arma::colvec& Xt) {
  const size_t N = H.size();
  Rcpp::NumericVector weight(N);
  Rcpp::NumericVector At(N);
  Rcpp::List Hnew(N);
  if(y == 0) {
    for(auto i = 0; i < N; i++) {
      arma::mat points = Points[i];
      double MIN = arma::min(points * Xt);
      double atilde = rtlogis2(MIN);
      At(i) = atilde;
      weight(i) = 1.0 - plogis(MIN);
      Hnew[i] = addHin(H[i], Xt, atilde, true);
    }
  } else {
    for(auto i = 0; i < N; i++) {
      arma::mat points = Points[i];
      double MAX = arma::max(points * Xt);
      double atilde = rtlogis1(MAX);
      At(i) = atilde;
      weight(i) = plogis(MAX);
      Hnew[i] = addHin(H[i], Xt, atilde, false);
    }
  }
  return Rcpp::List::create(Rcpp::Named("H") = Hnew, Rcpp::Named("At") = At,
                            Rcpp::Named("weight") = weight);
}
