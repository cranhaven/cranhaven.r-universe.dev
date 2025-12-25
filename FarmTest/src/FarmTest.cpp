# include <RcppArmadillo.h>
# include <algorithm>
# include <string>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
int sgn(const double x) {
  return (x > 0) - (x < 0);
}

// [[Rcpp::export]]
double f1(const double x, const arma::vec& resSq, const int n, const double rhs) {
  return arma::mean(arma::min(resSq / x, arma::ones(n))) - rhs;
}

// [[Rcpp::export]]
double rootf1(const arma::vec& resSq, const int n, const double rhs, double low, double up, const double tol = 0.001, const int maxIte = 500) {
  int ite = 1;
  while (ite <= maxIte && up - low > tol) {
    double mid = 0.5 * (up + low);
    double val = f1(mid, resSq, n, rhs);
    if (val < 0) {
      up = mid;
    } else {
      low = mid;
    }
    ite++;
  }
  return 0.5 * (low + up);
}

// [[Rcpp::export]]
double f2(const double x, const arma::vec& resSq, const int N, const double rhs) {
  return arma::mean(arma::min(resSq / x, arma::ones(N))) - rhs;
}

// [[Rcpp::export]]
double rootf2(const arma::vec& resSq, const int n, const int d, const int N, const double rhs, double low, double up, const double tol = 0.001, 
              const int maxIte = 500) {
  int ite = 0;
  while (ite <= maxIte && up - low > tol) {
    double mid = 0.5 * (up + low);
    double val = f2(mid, resSq, N, rhs);
    if (val < 0) {
      up = mid;
    } else {
      low = mid;
    }
    ite++;
  }
  return 0.5 * (low + up);
}

// [[Rcpp::export]]
double g1(const double x, const arma::vec& resSq, const int n, const double rhs) {
  return arma::mean(arma::min(resSq / x, arma::ones(n))) - rhs;
}

// [[Rcpp::export]]
double rootg1(const arma::vec& resSq, const int n, const double rhs, double low, double up, const double tol = 0.001, const int maxIte = 500) {
  int ite = 0;
  while (ite <= maxIte && up - low > tol) {
    double mid = 0.5 * (up + low);
    double val = g1(mid, resSq, n, rhs);
    if (val < 0) {
      up = mid;
    } else {
      low = mid;
    }
    ite++;
  }
  return 0.5 * (low + up);
}

// [[Rcpp::export]]
double huberDer(const arma::vec& res, const double tau, const int n) {
  double rst = 0.0;
  for (int i = 0; i < n; i++) {
    double cur = res(i);
    rst -= std::abs(cur) <= tau ? cur : tau * sgn(cur);
  }
  return rst / n;
}

// [[Rcpp::export]]
double huberMean(arma::vec X, const int n, const double tol = 0.001, const int iteMax = 500) {
  double rhs = std::log(n) / n;
  double mx = arma::mean(X);
  X -= mx;
  double tau = arma::stddev(X) * std::sqrt((long double)n / std::log(n));
  double derOld = huberDer(X, tau, n);
  double mu = -derOld, muDiff = -derOld;
  arma::vec res = X - mu;
  arma::vec resSq = arma::square(res);
  tau = std::sqrt((long double)rootf1(resSq, n, rhs, arma::min(resSq), arma::accu(resSq)));
  double derNew = huberDer(res, tau, n);
  double derDiff = derNew - derOld;
  int ite = 1;
  while (std::abs(derNew) > tol && ite <= iteMax) {
    double alpha = 1.0;
    double cross = muDiff * derDiff;
    if (cross > 0) {
      double a1 = cross / derDiff * derDiff;
      double a2 = muDiff * muDiff / cross;
      alpha = std::min(std::min(a1, a2), 100.0);
    }
    derOld = derNew;
    muDiff = -alpha * derNew;
    mu += muDiff;
    res = X - mu;
    resSq = arma::square(res);
    tau = std::sqrt((long double)rootf1(resSq, n, rhs, arma::min(resSq), arma::accu(resSq)));
    derNew = huberDer(res, tau, n);
    derDiff = derNew - derOld;
    ite++;
  }
  return mu + mx;
}

// [[Rcpp::export]]
arma::vec huberMeanVec(const arma::mat& X, const int n, const int p, const double epsilon = 0.001, const int iteMax = 500) {
  arma::vec rst(p);
  for (int i = 0; i < p; i++) {
    rst(i) = huberMean(X.col(i), n, epsilon, iteMax);
  }
  return rst;
}

// [[Rcpp::export]]
double hMeanCov(const arma::vec& Z, const int n, const int d, const int N, double rhs, const double epsilon = 0.0001, const int iteMax = 500) {
  double muOld = 0;
  double muNew = arma::mean(Z);
  double tau = arma::stddev(Z) * std::sqrt((long double)n / (2 * std::log(d) + std::log(n)));
  int iteNum = 0;
  arma::vec res(n), resSq(n), w(n);
  while ((std::abs(muNew - muOld) > epsilon) && iteNum < iteMax) {
    muOld = muNew;
    res = Z - muOld;
    resSq = arma::square(res);
    tau = std::sqrt((long double)rootf2(resSq, n, d, N, rhs, arma::min(resSq), arma::accu(resSq)));
    w = arma::min(tau / arma::abs(res), arma::ones(N));
    muNew = arma::as_scalar(Z.t() * w) / arma::accu(w);
    iteNum++;
  }
  return muNew;
}

// [[Rcpp::export]]
Rcpp::List huberCov(const arma::mat& X, const int n, const int p) {
  double rhs2 = (2 * std::log(p) + std::log(n)) / n;
  arma::vec mu(p);
  arma::mat sigmaHat(p, p);
  for (int j = 0; j < p; j++) {
    mu(j) = huberMean(X.col(j), n);
    double theta = huberMean(arma::square(X.col(j)), n);
    double temp = mu(j) * mu(j);
    if (theta > temp) {
      theta -= temp;
    }
    sigmaHat(j, j) = theta;
  }
  int N = n * (n - 1) >> 1;
  arma::mat Y(N, p);
  for (int i = 0, k = 0; i < n - 1; i++) {
    for (int j = i + 1; j < n; j++) {
      Y.row(k++) = X.row(i) - X.row(j);
    }
  }
  for (int i = 0; i < p - 1; i++) {
    for (int j = i + 1; j < p; j++) {
      sigmaHat(i, j) = sigmaHat(j, i) = hMeanCov(0.5 * Y.col(i) % Y.col(j), n, p, N, rhs2);
    }
  }
  return Rcpp::List::create(Rcpp::Named("means") = mu, Rcpp::Named("cov") = sigmaHat);
}

// [[Rcpp::export]]
double mad(const arma::vec& x) {
  return 1.482602 * arma::median(arma::abs(x - arma::median(x)));
}

// [[Rcpp::export]]
arma::mat standardize(arma::mat X, const arma::rowvec& mx, const arma::vec& sx, const int p) {
  for (int i = 0; i < p; i++) {
    X.col(i) = (X.col(i) - mx(i)) / sx(i);
  }
  return X;
}

// [[Rcpp::export]]
void updateHuber(const arma::mat& Z, const arma::vec& res, arma::vec& der, arma::vec& grad, const int n, const double tau, const double n1) {
  for (int i = 0; i < n; i++) {
    double cur = res(i);
    if (std::abs(cur) <= tau) {
      der(i) = -cur;
    } else {
      der(i) = -tau * sgn(cur);
    }
  }
  grad = n1 * Z.t() * der;
}

// [[Rcpp::export]]
arma::vec adaHuberReg(const arma::mat& X, arma::vec Y, const int n, const int p, const double tol = 0.0001, const int iteMax = 5000) {
  const double n1 = 1.0 / n;
  double rhs = n1 * (p + std::log(n * p));
  arma::rowvec mx = arma::mean(X, 0);
  arma::vec sx = arma::stddev(X, 0, 0).t();
  double my = arma::mean(Y);
  arma::mat Z = arma::join_rows(arma::ones(n), standardize(X, mx, sx, p));
  Y -= my;
  double tau = 1.345 * mad(Y);
  arma::vec der(n);
  arma::vec gradOld(p + 1), gradNew(p + 1);
  updateHuber(Z, Y, der, gradOld, n, tau, n1);
  arma::vec beta = -gradOld, betaDiff = -gradOld;
  arma::vec res = Y - Z * beta;
  arma::vec resSq = arma::square(res);
  tau = std::sqrt((long double)rootg1(resSq, n, rhs, arma::min(resSq), arma::accu(resSq)));
  updateHuber(Z, res, der, gradNew, n, tau, n1);
  arma::vec gradDiff = gradNew - gradOld;
  int ite = 1;
  while (arma::norm(gradNew, "inf") > tol && ite <= iteMax) {
    double alpha = 1.0;
    double cross = arma::as_scalar(betaDiff.t() * gradDiff);
    if (cross > 0) {
      double a1 = cross / arma::as_scalar(gradDiff.t() * gradDiff);
      double a2 = arma::as_scalar(betaDiff.t() * betaDiff) / cross;
      alpha = std::min(std::min(a1, a2), 100.0);
    }
    gradOld = gradNew;
    betaDiff = -alpha * gradNew;
    beta += betaDiff;
    res -= Z * betaDiff;
    resSq = arma::square(res);
    tau = std::sqrt((long double)rootg1(resSq, n, rhs, arma::min(resSq), arma::accu(resSq)));
    updateHuber(Z, res, der, gradNew, n, tau, n1);
    gradDiff = gradNew - gradOld;
    ite++;
  }
  beta.rows(1, p) /= sx;
  beta(0) = huberMean(Y + my - X * beta.rows(1, p), n);
  return beta;
}

// [[Rcpp::export]]
arma::vec huberReg(const arma::mat& X, arma::vec Y, const int n, const int p, const double tol = 0.0001, const double constTau = 1.345, 
                   const int iteMax = 5000) {
  const double n1 = 1.0 / n;
  arma::rowvec mx = arma::mean(X, 0);
  arma::vec sx = arma::stddev(X, 0, 0).t();
  double my = arma::mean(Y);
  arma::mat Z = arma::join_rows(arma::ones(n), standardize(X, mx, sx, p));
  Y -= my;
  double tau = constTau * mad(Y);
  arma::vec der(n);
  arma::vec gradOld(p + 1), gradNew(p + 1);
  updateHuber(Z, Y, der, gradOld, n, tau, n1);
  arma::vec beta = -gradOld, betaDiff = -gradOld;
  arma::vec res = Y - Z * beta;
  tau = constTau * mad(res);
  updateHuber(Z, res, der, gradNew, n, tau, n1);
  arma::vec gradDiff = gradNew - gradOld;
  int ite = 1;
  while (arma::norm(gradNew, "inf") > tol && ite <= iteMax) {
    double alpha = 1.0;
    double cross = arma::as_scalar(betaDiff.t() * gradDiff);
    if (cross > 0) {
      double a1 = cross / arma::as_scalar(gradDiff.t() * gradDiff);
      double a2 = arma::as_scalar(betaDiff.t() * betaDiff) / cross;
      alpha = std::min(std::min(a1, a2), 100.0);
    }
    gradOld = gradNew;
    betaDiff = -alpha * gradNew;
    beta += betaDiff;
    res -= Z * betaDiff;
    tau = constTau * mad(res);
    updateHuber(Z, res, der, gradNew, n, tau, n1);
    gradDiff = gradNew - gradOld;
    ite++;
  }
  beta.rows(1, p) /= sx;
  beta(0) = huberMean(Y + my - X * beta.rows(1, p), n);
  return beta;
}

// [[Rcpp::export]]
arma::vec huberRegCoef(const arma::mat& X, arma::vec Y, const int n, const int p, const double tol = 0.0001, const double constTau = 1.345, 
                       const int iteMax = 5000) {
  const double n1 = 1.0 / n;
  arma::rowvec mx = arma::mean(X, 0);
  arma::vec sx = arma::stddev(X, 0, 0).t();
  double my = arma::mean(Y);
  arma::mat Z = arma::join_rows(arma::ones(n), standardize(X, mx, sx, p));
  Y -= my;
  double tau = constTau * mad(Y);
  arma::vec der(n);
  arma::vec gradOld(p + 1), gradNew(p + 1);
  updateHuber(Z, Y, der, gradOld, n, tau, n1);
  arma::vec beta = -gradOld, betaDiff = -gradOld;
  arma::vec res = Y - Z * beta;
  tau = constTau * mad(res);
  updateHuber(Z, res, der, gradNew, n, tau, n1);
  arma::vec gradDiff = gradNew - gradOld;
  int ite = 1;
  while (arma::norm(gradNew, "inf") > tol && ite <= iteMax) {
    double alpha = 1.0;
    double cross = arma::as_scalar(betaDiff.t() * gradDiff);
    if (cross > 0) {
      double a1 = cross / arma::as_scalar(gradDiff.t() * gradDiff);
      double a2 = arma::as_scalar(betaDiff.t() * betaDiff) / cross;
      alpha = std::min(std::min(a1, a2), 100.0);
    }
    gradOld = gradNew;
    betaDiff = -alpha * gradNew;
    beta += betaDiff;
    res -= Z * betaDiff;
    tau = constTau * mad(res);
    updateHuber(Z, res, der, gradNew, n, tau, n1);
    gradDiff = gradNew - gradOld;
    ite++;
  }
  return beta.rows(1, p) / sx;
}

// [[Rcpp::export]]
double huberRegItcp(const arma::mat& X, arma::vec Y, const int n, const int p, const double tol = 0.0001, const double constTau = 1.345, 
                       const int iteMax = 5000) {
  const double n1 = 1.0 / n;
  arma::rowvec mx = arma::mean(X, 0);
  arma::vec sx = arma::stddev(X, 0, 0).t();
  double my = arma::mean(Y);
  arma::mat Z = arma::join_rows(arma::ones(n), standardize(X, mx, sx, p));
  Y -= my;
  double tau = constTau * mad(Y);
  arma::vec der(n);
  arma::vec gradOld(p + 1), gradNew(p + 1);
  updateHuber(Z, Y, der, gradOld, n, tau, n1);
  arma::vec beta = -gradOld, betaDiff = -gradOld;
  arma::vec res = Y - Z * beta;
  tau = constTau * mad(res);
  updateHuber(Z, res, der, gradNew, n, tau, n1);
  arma::vec gradDiff = gradNew - gradOld;
  int ite = 1;
  while (arma::norm(gradNew, "inf") > tol && ite <= iteMax) {
    double alpha = 1.0;
    double cross = arma::as_scalar(betaDiff.t() * gradDiff);
    if (cross > 0) {
      double a1 = cross / arma::as_scalar(gradDiff.t() * gradDiff);
      double a2 = arma::as_scalar(betaDiff.t() * betaDiff) / cross;
      alpha = std::min(std::min(a1, a2), 100.0);
    }
    gradOld = gradNew;
    betaDiff = -alpha * gradNew;
    beta += betaDiff;
    res -= Z * betaDiff;
    tau = constTau * mad(res);
    updateHuber(Z, res, der, gradNew, n, tau, n1);
    gradDiff = gradNew - gradOld;
    ite++;
  }
  beta.rows(1, p) /= sx;
  return huberMean(Y + my - X * beta.rows(1, p), n);
}

// [[Rcpp::export]]
arma::vec getP(const arma::vec& T, const std::string alternative) {
  arma::vec rst;
  if (alternative == "two.sided") {
    rst = 2 * arma::normcdf(-arma::abs(T));
  } else if (alternative == "less") {
    rst = arma::normcdf(T);
  } else {
    rst = arma::normcdf(-T);
  }
  return rst;
}

// [[Rcpp::export]]
arma::vec getPboot(const arma::vec& mu, const arma::mat& boot, const arma::vec& h0, const std::string alternative, const int p, const int B) {
  arma::vec rst(p);
  if (alternative == "two.sided") {
    for (int i = 0; i < p; i++) {
      rst(i) = arma::accu(arma::abs(boot.row(i) - mu(i)) >= std::abs(mu(i) - h0(i)));
    }
  } else if (alternative == "less") {
    for (int i = 0; i < p; i++) {
      rst(i) = arma::accu(boot.row(i) <= 2 * mu(i) - h0(i));
    }
  } else {
    for (int i = 0; i < p; i++) {
      rst(i) = arma::accu(boot.row(i) >= 2 * mu(i) - h0(i));
    }
  }
  return rst / B;
}

// [[Rcpp::export]]
arma::vec adjust(const arma::vec& Prob, const double alpha, const int p) {
  double piHat = std::min((double)arma::accu(Prob > alpha) / (p * (1 - alpha)), 1.0);
  arma::uvec rk = arma::sort_index(arma::sort_index(Prob)) + 1;
  return arma::min(Prob * piHat * p / rk, arma::ones(p));
}

// [[Rcpp::export]]
arma::vec getRatio(const arma::vec& eigenVal, const int n, const int p) {
  int temp = std::min(n, p);
  int len = temp < 4 ? temp - 1 : temp >> 1;
  if (len == 0) {
    arma::vec rst(1);
    rst(0) = eigenVal(p - 1);
    return rst;
  }
  arma::vec ratio(len);
  double comp = eigenVal(p - 1) / eigenVal(p - 2);
  ratio(0) = comp;
  for (int i = 1; i < len; i++) {
    ratio(i) = eigenVal(p - 1 - i) / eigenVal(p - 2 - i);
  }
  return ratio;
}

// [[Rcpp::export]]
Rcpp::List rmTest(const arma::mat& X, const arma::vec& h0, const double alpha = 0.05, const std::string alternative = "two.sided") {
  int n = X.n_rows, p = X.n_cols;
  arma::vec mu(p), sigma(p);
  for (int j = 0; j < p; j++) {
    mu(j) = huberMean(X.col(j), n);
    double theta = huberMean(arma::square(X.col(j)), n);
    double temp = mu(j) * mu(j);
    if (theta > temp) {
      theta -= temp;
    }
    sigma(j) = theta;
  }
  sigma = arma::sqrt(sigma / n);
  arma::vec T = (mu - h0) / sigma;
  arma::vec Prob = getP(T, alternative);
  arma::vec pAdjust = adjust(Prob, alpha, p);
  arma::uvec significant = pAdjust <= alpha;
  return Rcpp::List::create(Rcpp::Named("means") = mu, Rcpp::Named("stdDev") = sigma, Rcpp::Named("tStat") = T, Rcpp::Named("pValues") = Prob, 
                            Rcpp::Named("pAdjust") = pAdjust, Rcpp::Named("significant") = significant);
}

// [[Rcpp::export]]
Rcpp::List rmTestBoot(const arma::mat& X, const arma::vec& h0, const double alpha = 0.05, const std::string alternative = "two.sided", 
                      const int B = 500) {
  int n = X.n_rows, p = X.n_cols;
  arma::vec mu = huberMeanVec(X, n, p);
  arma::mat boot(p, B);
  for (int i = 0; i < B; i++) {
    arma::uvec idx = arma::find(arma::randi(n, arma::distr_param(0, 1)) == 1);
    int subn = idx.size();
    arma::mat subX = X.rows(idx);
    boot.col(i) = huberMeanVec(subX, subn, p);
  }
  arma::vec Prob = getPboot(mu, boot, h0, alternative, p, B);
  arma::vec pAdjust = adjust(Prob, alpha, p);
  arma::uvec significant = pAdjust <= alpha;
  return Rcpp::List::create(Rcpp::Named("means") = mu, Rcpp::Named("pValues") = Prob, Rcpp::Named("pAdjust") = pAdjust, 
                            Rcpp::Named("significant") = significant);
}

// [[Rcpp::export]]
Rcpp::List rmTestTwo(const arma::mat& X, const arma::mat& Y, const arma::vec& h0, const double alpha = 0.05, 
                     const std::string alternative = "two.sided") {
  int nX = X.n_rows, nY = Y.n_rows, p = X.n_cols;
  arma::vec muX(p), sigmaX(p), muY(p), sigmaY(p);
  for (int j = 0; j < p; j++) {
    muX(j) = huberMean(X.col(j), nX);
    muY(j) = huberMean(Y.col(j), nY);
    double theta = huberMean(arma::square(X.col(j)), nX);
    double temp = muX(j) * muX(j);
    if (theta > temp) {
      theta -= temp;
    }
    sigmaX(j) = theta;
    theta = huberMean(arma::square(Y.col(j)), nY);
    temp = muY(j) * muY(j);
    if (theta > temp) {
      theta -= temp;
    }
    sigmaY(j) = theta;
  }
  arma::vec T = (muX - muY - h0) / arma::sqrt(sigmaX / nX + sigmaY / nY);
  sigmaX = arma::sqrt(sigmaX / nX);
  sigmaY = arma::sqrt(sigmaY / nY);
  arma::vec Prob = getP(T, alternative);
  arma::vec pAdjust = adjust(Prob, alpha, p);
  arma::uvec significant = pAdjust <= alpha;
  return Rcpp::List::create(Rcpp::Named("meansX") = muX, Rcpp::Named("meansY") = muY, Rcpp::Named("stdDevX") = sigmaX, 
                            Rcpp::Named("stdDevY") = sigmaY, Rcpp::Named("tStat") = T, Rcpp::Named("pValues") = Prob, 
                            Rcpp::Named("pAdjust") = pAdjust, Rcpp::Named("significant") = significant);
}

// [[Rcpp::export]]
Rcpp::List rmTestTwoBoot(const arma::mat& X, const arma::mat& Y, const arma::vec& h0, const double alpha = 0.05, 
                         const std::string alternative = "two.sided", const int B = 500) {
  int nX = X.n_rows, nY = Y.n_rows, p = X.n_cols;
  arma::vec muX = huberMeanVec(X, nX, p);
  arma::vec muY = huberMeanVec(Y, nY, p);
  arma::mat bootX(p, B), bootY(p, B);
  for (int i = 0; i < B; i++) {
    arma::uvec idx = arma::find(arma::randi(nX, arma::distr_param(0, 1)) == 1);
    int subn = idx.size();
    arma::mat subX = X.rows(idx);
    bootX.col(i) = huberMeanVec(subX, subn, p);
    idx = arma::find(arma::randi(nY, arma::distr_param(0, 1)) == 1);
    subn = idx.size();
    arma::mat subY = Y.rows(idx);
    bootY.col(i) = huberMeanVec(subY, subn, p);
  }
  arma::vec Prob = getPboot(muX - muY, bootX - bootY, h0, alternative, p, B);
  arma::vec pAdjust = adjust(Prob, alpha, p);
  arma::uvec significant = pAdjust <= alpha;
  return Rcpp::List::create(Rcpp::Named("meansX") = muX, Rcpp::Named("meansY") = muY, Rcpp::Named("pValues") = Prob, 
                            Rcpp::Named("pAdjust") = pAdjust, Rcpp::Named("significant") = significant);
}

// [[Rcpp::export]]
Rcpp::List farmTest(const arma::mat& X, const arma::vec& h0, int K = -1, const double alpha = 0.05, const std::string alternative = "two.sided") {
  int n = X.n_rows, p = X.n_cols;
  Rcpp::List listCov = huberCov(X, n, p);
  arma::vec mu = listCov["means"];
  arma::mat sigmaHat = listCov["cov"];
  arma::vec sigma = sigmaHat.diag();
  arma::vec eigenVal;
  arma::mat eigenVec;
  arma::eig_sym(eigenVal, eigenVec, sigmaHat);
  arma::vec ratio;
  if (K <= 0) {
    ratio = getRatio(eigenVal, n, p);
    K = arma::index_max(ratio) + 1;
  }
  arma::mat B(p, K);
  for (int i = 1; i <= K; i++) {
    double lambda = std::sqrt((long double)std::max(eigenVal(p - i), 0.0));
    B.col(i - 1) = lambda * eigenVec.col(p - i);
  }
  arma::vec f = huberRegCoef(B, arma::mean(X, 0).t(), p, K);
  for (int j = 0; j < p; j++) {
    double temp = arma::norm(B.row(j), 2);
    if (sigma(j) > temp * temp) {
      sigma(j) -= temp * temp;
    }
  }
  mu -= B * f;
  sigma = arma::sqrt(sigma / n);
  arma::vec T = (mu - h0) / sigma;
  arma::vec Prob = getP(T, alternative);
  arma::vec pAdjust = adjust(Prob, alpha, p);
  arma::uvec significant = pAdjust <= alpha;
  return Rcpp::List::create(Rcpp::Named("means") = mu, Rcpp::Named("stdDev") = sigma, Rcpp::Named("loadings") = B, Rcpp::Named("nfactors") = K, 
                            Rcpp::Named("tStat") = T, Rcpp::Named("pValues") = Prob, Rcpp::Named("pAdjust") = pAdjust, 
                            Rcpp::Named("significant") = significant, Rcpp::Named("eigens") = eigenVal, Rcpp::Named("ratio") = ratio);
}

// [[Rcpp::export]]
Rcpp::List farmTestTwo(const arma::mat& X, const arma::mat& Y, const arma::vec& h0, int KX = -1, int KY = -1, const double alpha = 0.05, 
                       const std::string alternative = "two.sided") {
  int nX = X.n_rows, nY = Y.n_rows, p = X.n_cols;
  Rcpp::List listCov = huberCov(X, nX, p);
  arma::vec muX = listCov["means"];
  arma::mat sigmaHat = listCov["cov"];
  arma::vec sigmaX = sigmaHat.diag();
  arma::vec eigenValX, eigenValY;
  arma::mat eigenVec;
  arma::eig_sym(eigenValX, eigenVec, sigmaHat);
  arma::vec ratioX, ratioY;
  if (KX <= 0) {
    ratioX = getRatio(eigenValX, nX, p);
    KX = arma::index_max(ratioX) + 1;
  }
  arma::mat BX(p, KX);
  for (int i = 1; i <= KX; i++) {
    double lambda = std::sqrt((long double)std::max(eigenValX(p - i), 0.0));
    BX.col(i - 1) = lambda * eigenVec.col(p - i);
  }
  arma::vec fX = huberRegCoef(BX, arma::mean(X, 0).t(), p, KX);
  listCov = huberCov(Y, nY, p);
  arma::vec muY = listCov["means"];
  sigmaHat = Rcpp::as<arma::mat>(listCov["cov"]);
  arma::vec sigmaY = sigmaHat.diag();
  arma::eig_sym(eigenValY, eigenVec, sigmaHat);
  if (KY <= 0) {
    ratioY = getRatio(eigenValY, nY, p);
    KY = arma::index_max(ratioY) + 1;
  }
  arma::mat BY(p, KY);
  for (int i = 1; i <= KY; i++) {
    double lambda = std::sqrt((long double)std::max(eigenValY(p - i), 0.0));
    BY.col(i - 1) = lambda * eigenVec.col(p - i);
  }
  arma::vec fY = huberRegCoef(BY, arma::mean(Y, 0).t(), p, KY);
  for (int j = 0; j < p; j++) {
    double temp = arma::norm(BX.row(j), 2);
    if (sigmaX(j) > temp * temp) {
      sigmaX(j) -= temp * temp;
    }
    temp = arma::norm(BY.row(j), 2);
    if (sigmaY(j) > temp * temp) {
      sigmaY(j) -= temp * temp;
    }
  }
  muX -= BX * fX;
  muY -= BY * fY;
  arma::vec T = (muX - muY - h0) / arma::sqrt(sigmaX / nX + sigmaY / nY);
  sigmaX = arma::sqrt(sigmaX / nX);
  sigmaY = arma::sqrt(sigmaY / nY);
  arma::vec Prob = getP(T, alternative);
  arma::vec pAdjust = adjust(Prob, alpha, p);
  arma::uvec significant = pAdjust <= alpha;
  return Rcpp::List::create(Rcpp::Named("meansX") = muX, Rcpp::Named("meansY") = muY, Rcpp::Named("stdDevX") = sigmaX, 
                            Rcpp::Named("stdDevY") = sigmaY, Rcpp::Named("loadingsX") = BX, Rcpp::Named("loadingsY") = BY,
                            Rcpp::Named("nfactorsX") = KX, Rcpp::Named("nfactorsY") = KY, Rcpp::Named("tStat") = T, 
                            Rcpp::Named("pValues") = Prob, Rcpp::Named("pAdjust") = pAdjust, Rcpp::Named("significant") = significant, 
                            Rcpp::Named("eigensX") = eigenValX, Rcpp::Named("eigensY") = eigenValY, Rcpp::Named("ratioX") = ratioX, 
                            Rcpp::Named("ratioY") = ratioY);
}

// [[Rcpp::export]]
Rcpp::List farmTestFac(const arma::mat& X, const arma::mat& fac, const arma::vec& h0, const double alpha = 0.05, 
                       const std::string alternative = "two.sided") {
  int n = X.n_rows, p = X.n_cols, K = fac.n_cols;
  arma::mat Sigma = arma::cov(fac);
  arma::vec mu(p), sigma(p);
  arma::vec theta, beta;
  arma::mat B(p, K);
  for (int j = 0; j < p; j++) {
    theta = huberReg(fac, X.col(j), n, K);
    mu(j) = theta(0);
    beta = theta.rows(1, K);
    B.row(j) = beta.t();
    double sig = huberMean(arma::square(X.col(j)), n);
    double temp = mu(j) * mu(j);
    if (sig > temp) {
      sig -= temp;
    }
    temp = arma::as_scalar(beta.t() * Sigma * beta);
    if (sig > temp) {
      sig -= temp;
    }
    sigma(j) = sig;
  }
  sigma = arma::sqrt(sigma / n);
  arma::vec T = (mu - h0) / sigma;
  arma::vec Prob = getP(T, alternative);
  arma::vec pAdjust = adjust(Prob, alpha, p);
  arma::uvec significant = pAdjust <= alpha;
  return Rcpp::List::create(Rcpp::Named("means") = mu, Rcpp::Named("stdDev") = sigma, Rcpp::Named("loadings") = B, Rcpp::Named("nfactors") = K,
                            Rcpp::Named("tStat") = T, Rcpp::Named("pValues") = Prob, Rcpp::Named("pAdjust") = pAdjust, 
                            Rcpp::Named("significant") = significant);
}

// [[Rcpp::export]]
Rcpp::List farmTestFacBoot(const arma::mat& X, const arma::mat& fac, const arma::vec& h0, const double alpha = 0.05, 
                           const std::string alternative = "two.sided", const int B = 500) {
  int n = X.n_rows, p = X.n_cols, K = fac.n_cols;
  arma::vec mu(p);
  for (int j = 0; j < p; j++) {
    mu(j) = huberRegItcp(fac, X.col(j), n, K);
  }
  arma::mat boot(p, B);
  for (int i = 0; i < B; i++) {
    arma::uvec idx = arma::find(arma::randi(n, arma::distr_param(0, 1)) == 1);
    int subn = idx.size();
    arma::mat subX = X.rows(idx);
    for (int j = 0; j < p; j++) {
      boot(j, i) = huberRegItcp(fac.rows(idx), subX.col(j), subn, K);
    }
  }
  arma::vec Prob = getPboot(mu, boot, h0, alternative, p, B);
  arma::vec pAdjust = adjust(Prob, alpha, p);
  arma::uvec significant = pAdjust <= alpha;
  return Rcpp::List::create(Rcpp::Named("means") = mu, Rcpp::Named("nfactors") = K, Rcpp::Named("pValues") = Prob, 
                            Rcpp::Named("pAdjust") = pAdjust, Rcpp::Named("significant") = significant);
}

// [[Rcpp::export]]
Rcpp::List farmTestTwoFac(const arma::mat& X, const arma::mat& facX, const arma::mat& Y, const arma::mat& facY, const arma::vec& h0, 
                          const double alpha = 0.05, const std::string alternative = "two.sided") {
  int nX = X.n_rows, nY = Y.n_rows, p = X.n_cols, KX = facX.n_cols, KY = facY.n_cols;
  arma::mat SigmaX = arma::cov(facX);
  arma::mat SigmaY = arma::cov(facY);
  arma::vec muX(p), sigmaX(p), muY(p), sigmaY(p);
  arma::vec theta, beta;
  arma::mat BX(p, KX), BY(p, KY);
  for (int j = 0; j < p; j++) {
    theta = huberReg(facX, X.col(j), nX, KX);
    muX(j) = theta(0);
    beta = theta.rows(1, KX);
    BX.row(j) = beta.t();
    double sig = huberMean(arma::square(X.col(j)), nX);
    double temp = muX(j) * muX(j);
    if (sig > temp) {
      sig -= temp;
    }
    temp = arma::as_scalar(beta.t() * SigmaX * beta);
    if (sig > temp) {
      sig -= temp;
    }
    sigmaX(j) = sig;
    theta = huberReg(facY, Y.col(j), nY, KY);
    muY(j) = theta(0);
    beta = theta.rows(1, KY);
    BY.row(j) = beta.t();
    sig = huberMean(arma::square(Y.col(j)), nY);
    temp = muY(j) * muY(j);
    if (sig > temp) {
      sig -= temp;
    }
    temp = arma::as_scalar(beta.t() * SigmaY * beta);
    if (sig > temp) {
      sig -= temp;
    }
    sigmaY(j) = sig;
  }
  arma::vec T = (muX - muY - h0) / arma::sqrt(sigmaX / nX + sigmaY / nY);
  sigmaX = arma::sqrt(sigmaX / nX);
  sigmaY = arma::sqrt(sigmaY / nY);
  arma::vec Prob = getP(T, alternative);
  arma::vec pAdjust = adjust(Prob, alpha, p);
  arma::uvec significant = pAdjust <= alpha;
  return Rcpp::List::create(Rcpp::Named("meansX") = muX, Rcpp::Named("meansY") = muY, Rcpp::Named("stdDevX") = sigmaX, 
                            Rcpp::Named("stdDevY") = sigmaY, Rcpp::Named("loadingsX") = BX, Rcpp::Named("loadingsY") = BY,
                            Rcpp::Named("nfactorsX") = KX, Rcpp::Named("nfactorsY") = KY, Rcpp::Named("tStat") = T, 
                            Rcpp::Named("pValues") = Prob, Rcpp::Named("pAdjust") = pAdjust, Rcpp::Named("significant") = significant);
}

// [[Rcpp::export]]
Rcpp::List farmTestTwoFacBoot(const arma::mat& X, const arma::mat& facX, const arma::mat& Y, const arma::mat& facY, 
                              const arma::vec& h0, const double alpha = 0.05, const std::string alternative = "two.sided", const int B = 500) {
  int nX = X.n_rows, nY = Y.n_rows, p = X.n_cols, KX = facX.n_cols, KY = facY.n_cols;
  arma::vec muX(p), muY(p);
  for (int j = 0; j < p; j++) {
    muX(j) = huberRegItcp(facX, X.col(j), nX, KX);
    muY(j) = huberRegItcp(facY, Y.col(j), nY, KY);
  }
  arma::mat bootX(p, B), bootY(p, B);
  for (int i = 0; i < B; i++) {
    arma::uvec idx = arma::find(arma::randi(nX, arma::distr_param(0, 1)) == 1);
    int subn = idx.size();
    arma::mat subX = X.rows(idx);
    for (int j = 0; j < p; j++) {
      bootX(j, i) = huberRegItcp(facX.rows(idx), subX.col(j), subn, KX);
    }
    idx = arma::find(arma::randi(nY, arma::distr_param(0, 1)) == 1);
    subn = idx.size();
    arma::mat subY = Y.rows(idx);
    for (int j = 0; j < p; j++) {
      bootY(j, i) = huberRegItcp(facY.rows(idx), subY.col(j), subn, KY);
    }
  }
  arma::vec Prob = getPboot(muX - muY, bootX - bootY, h0, alternative, p, B);
  arma::vec pAdjust = adjust(Prob, alpha, p);
  arma::uvec significant = pAdjust <= alpha;
  return Rcpp::List::create(Rcpp::Named("meansX") = muX, Rcpp::Named("meansY") = muY, Rcpp::Named("nfactorsX") = KX, 
                            Rcpp::Named("nfactorsY") = KY, Rcpp::Named("pValues") = Prob, Rcpp::Named("pAdjust") = pAdjust, 
                            Rcpp::Named("significant") = significant);
}
