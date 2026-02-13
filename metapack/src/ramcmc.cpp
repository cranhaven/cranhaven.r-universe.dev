#include <RcppArmadillo.h>
#include <cmath>

arma::mat chol_update(arma::mat& L, arma::vec& u) {
  int n = u.n_elem - 1;
  for (int i = 0; i < n; i++) {
    double r = sqrt(L(i,i) * L(i,i) + u(i) * u(i));
    double c = r / L(i, i);
    double s = u(i) / L(i, i);
    L(i, i) = r;
    L(arma::span(i + 1, n), i) =
      (L(arma::span(i + 1, n), i) + s * u.rows(i + 1, n)) / c;
    u.rows(i + 1, n) = c * u.rows(i + 1, n) -
      s * L(arma::span(i + 1, n), i);
  }
  L(n, n) = std::sqrt(L(n, n) * L(n, n) + u(n) * u(n));
  return L;
}

arma::mat chol_downdate(arma::mat& L, arma::vec& u) {
  int n = u.n_elem - 1;
  for (int i = 0; i < n; i++) {
    double r = sqrt(L(i,i) * L(i,i) - u(i) * u(i));
    double c = r / L(i, i);
    double s = u(i) / L(i, i);
    L(i, i) = r;
    L(arma::span(i + 1, n), i) =
      (L(arma::span(i + 1, n), i) - s * u.rows(i + 1, n)) / c;
    u.rows(i + 1, n) = c * u.rows(i + 1, n) -
      s * L(arma::span(i + 1, n), i);
  }
  L(n, n) = sqrt(L(n, n) * L(n, n) - u(n) * u(n));
  return L;
}

void adapt_S(arma::mat& S, arma::vec& u, const double& current, const double& target, const int& n, const double& gamma) {

  double change = current - target;
  u = S * u / arma::norm(u) * std::sqrt(std::min(1.0, static_cast<double>(u.n_elem) * std::pow(static_cast<double>(n), -gamma)) *
    std::abs(change));

  if(change > 0.0) {
    chol_update(S, u);
  } else {
    chol_downdate(S, u);
  }
}
