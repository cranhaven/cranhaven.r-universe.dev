#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

#include <Rdefines.h>
#include <Rmath.h>

using namespace Rcpp;
using namespace arma;

static const double pii = M_PI;
static const double sqpii = sqrt(M_PI);

// RANDOM NUMBER GENERATOR

arma::umat indicatorFun(int p){
  arma::umat minusM(p-1, p, fill::zeros);
  for (int j=0; j<p; j++){
    int k = 0;
    for (int l=0; l<p; l++){
      if (l!=j){
        minusM(k,j) = l;
        k++;
      }
    }
  }
  return minusM;
}

arma::vec generateXi(int p, arma::umat minus, arma::vec xa, arma::mat Rinv,
                     arma::vec lower, arma::vec upper, double kap){
  double mj, tj, lv, rv, xij;
  arma::uvec pj; arma::rowvec a1; arma::vec xj;

  for (int j=0; j<p; j++){
    pj = minus.col(j);
    xj = xa(pj);
    a1 = xj.t()*Rinv.rows(pj);
    mj = -a1(j)/Rinv(j,j);
    tj = sqrt(mj*mj + (kap - as_scalar(a1.cols(pj)*xj))/Rinv(j,j));
    lv = std::max(lower(j), (mj-tj));
    rv = std::min(upper(j), (mj+tj));
    xij  = lv + (rv - lv)*arma::randu<double>();
    xa(j) = xij;
  }
  return xa;
}

// Generate random numbers from Truncated Multivariate Student's-t distribution t(mu,Sigma,nu; (a,b))
// ------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
arma::mat rttrunc(int n, double nu, const arma::vec mu, const arma::mat Sigma,
                  const arma::vec a, const arma::vec b, int burn, int lag){
  int m = lag*n + burn;
  int p = Sigma.n_cols;
  double nup  = nu + p;
  arma::vec s = arma::sqrt(arma::diagvec(Sigma));
  arma::mat R = Sigma%(1.0/(s * s.t()));
  arma::mat Rinv = arma::inv(R);
  arma::mat X(n, p, fill::zeros);

  Rcpp::NumericVector l1 = Rcpp::wrap((a - mu)/s);
  Rcpp::NumericVector u1 = Rcpp::wrap((b - mu)/s);
  arma::vec pa = Rcpp::pt(l1, nu, true, false);
  arma::vec pb = Rcpp::pt(u1, nu, true, false);
  arma::vec x0 = arma::randu<arma::vec>(p);
  Rcpp::NumericVector x1 = Rcpp::wrap(pa + (pb - pa)%x0);
  arma::colvec x  = Rcpp::qt(x1, nu, true, false);
  arma::vec lower = Rcpp::as<arma::vec>(l1);
  arma::vec upper = Rcpp::as<arma::vec>(u1);

  arma::uvec q1 = find_nonfinite(x);
  x.elem(q1) = lower.elem(q1);
  q1 = find_nonfinite(x);
  x.elem(q1) = upper.elem(q1);

  double delta, y, kap;
  arma::umat minusj = indicatorFun(p);
  int count = 1;
  for (int i=0; i<m; i++){
    delta = as_scalar(x.t()*Rinv*x);
    y = arma::randu<double>()*exp(-0.5*nup*log(1.0 + delta/nu));
    kap = nu*(exp((-2.0/nup)*log(y)) - 1.0);
    x = generateXi(p, minusj, x, Rinv, lower, upper, kap); // Simulate x
    if (i==(burn + count*lag - 1)){
      X.row(count-1) = x.t();
      count++;
    }
  }
  X = X.t();
  X = X.each_col()%s;
  X = (X.each_col() + mu).t();
  X.replace(arma::datum::inf, arma::datum::nan);
  X.replace(-arma::datum::inf, arma::datum::nan);
  return X;
}


// Generate random numbers from Truncated Multivariate Normal distribution N(mu,Sigma; (a,b))
// ------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
arma::mat rtnormal(int n, const arma::vec mu, const arma::mat Sigma, const arma::vec a,
                   const arma::vec b, int burn, int lag){
  int m = lag*n + burn;
  int p = Sigma.n_cols;
  arma::vec s = arma::sqrt(arma::diagvec(Sigma));
  arma::mat R = Sigma%(1.0/(s * s.t()));
  arma::mat Rinv = arma::inv(R);
  arma::mat X(n, p, fill::zeros);

  Rcpp::NumericVector l1 = Rcpp::wrap((a - mu)/s);
  Rcpp::NumericVector u1 = Rcpp::wrap((b - mu)/s);
  arma::vec pa = Rcpp::pnorm(l1, 0.0, 1.0, true, false);
  arma::vec pb = Rcpp::pnorm(u1, 0.0, 1.0, true, false);
  arma::vec x0 = arma::randu<arma::vec>(p);
  Rcpp::NumericVector x1 = Rcpp::wrap(pa + (pb - pa)%x0);
  arma::colvec x  = Rcpp::qnorm(x1, 0.0, 1.0, true, false);
  arma::vec lower = Rcpp::as<arma::vec>(l1);
  arma::vec upper = Rcpp::as<arma::vec>(u1);

  arma::uvec q1 = find_nonfinite(x);
  x.elem(q1) = lower.elem(q1);
  q1 = find_nonfinite(x);
  x.elem(q1) = upper.elem(q1);

  double delta, kap;
  arma::umat minusj = indicatorFun(p);
  int count = 1;
  for (int i=0; i<m; i++){
    delta = as_scalar(x.t()*Rinv*x);
    kap   = -2.0*log(arma::randu<double>()) + delta;
    x = generateXi(p, minusj, x, Rinv, lower, upper, kap); // Simulate x
    if (i==(burn + count*lag - 1)){
      X.row(count-1) = x.t();
      count++;
    }
  }
  X = X.t();
  X = X.each_col()%s;
  X = (X.each_col() + mu).t();
  X.replace(arma::datum::inf, arma::datum::nan);
  X.replace(-arma::datum::inf, arma::datum::nan);
  return X;
}


// Generate random numbers from Truncated Multivariate Power Exponential distribution PE(mu,Sigma,beta; (a,b))
// ------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
arma::mat rtPE(int n, const double beta, const arma::vec mu, const arma::mat Sigma,
               const arma::vec a, const arma::vec b, int burn, int lag){
  int m = lag*n + burn;
  int p = Sigma.n_cols;
  arma::vec s = arma::sqrt(arma::diagvec(Sigma));
  arma::mat R = Sigma%(1.0/(s * s.t()));
  arma::mat Rinv = arma::inv(R);
  arma::mat X(n, p, fill::zeros);

  arma::vec lower = (a - mu)/s;
  arma::vec upper = (b - mu)/s;
  arma::vec x = lower;
  arma::uvec q1 = find_nonfinite(x);
  x.elem(q1) = upper.elem(q1);
  x.replace(arma::datum::inf, 0.0);

  double delta, y, kap;
  arma::umat minusj = indicatorFun(p);
  int count = 1;
  for (int i=0; i<m; i++){
    delta = as_scalar(x.t()*Rinv*x);
    y   = -2.0*log(arma::randu<double>()) + exp(beta*log(delta));
    kap = exp((1.0/beta)*log(y));
    x = generateXi(p, minusj, x, Rinv, lower, upper, kap); // Simulate x
    if (i==(burn + count*lag - 1)){
      X.row(count-1) = x.t();
      count++;
    }
  }
  X = X.t();
  X = X.each_col()%s;
  X = (X.each_col() + mu).t();
  X.replace(arma::datum::inf, arma::datum::nan);
  X.replace(-arma::datum::inf, arma::datum::nan);
  return X;
}


// Generate random numbers from Truncated Multivariate Pearson VII distribution PVII(mu,Sigma,N,nu; (a,b))
// ------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
arma::mat rtPVII(int n, const double N, const double nu, const arma::vec mu, const arma::mat Sigma,
                 const arma::vec a, const arma::vec b, int burn, int lag){
  int m = lag*n + burn;
  int p = Sigma.n_cols;
  arma::vec s = arma::sqrt(arma::diagvec(Sigma));
  arma::mat R = Sigma%(1.0/(s * s.t()));
  arma::mat Rinv = arma::inv(R);
  arma::mat X(n, p, fill::zeros);

  Rcpp::NumericVector l1 = Rcpp::wrap((a - mu)/s);
  Rcpp::NumericVector u1 = Rcpp::wrap((b - mu)/s);
  double Ni  = N - (p - 1.0)/2.0;
  double nui = sqrt((2.0*Ni - 1.0)/nu);
  arma::vec pa = Rcpp::pt(nui*l1, 2.0*Ni-1.0, true, false);
  arma::vec pb = Rcpp::pt(nui*u1, 2.0*Ni-1.0, true, false);
  arma::vec x0 = arma::randu<arma::vec>(p);
  Rcpp::NumericVector x1 = Rcpp::wrap(pa + (pb - pa)%x0);
  arma::colvec x  = (Rcpp::qt(x1, 2.0*Ni-1.0, true, false))/nui;
  arma::vec lower = Rcpp::as<arma::vec>(l1);
  arma::vec upper = Rcpp::as<arma::vec>(u1);

  arma::uvec q1 = find_nonfinite(x);
  x.elem(q1)    = lower.elem(q1);
  q1 = find_nonfinite(x);
  x.elem(q1)    = upper.elem(q1);

  double delta, y, kap;
  arma::umat minusj = indicatorFun(p);
  int count = 1;
  for (int i=0; i<m; i++){
    delta = as_scalar(x.t()*Rinv*x);
    y = arma::randu<double>()*exp(-N*log(1.0 + delta/nu));
    kap = nu*(exp((-1.0/N)*log(y)) - 1.0);
    x = generateXi(p, minusj, x, Rinv, lower, upper, kap); // Simulate x
    if (i==(burn + count*lag - 1)){
      X.row(count-1) = x.t();
      count++;
    }
  }
  X = X.t();
  X = X.each_col()%s;
  X = (X.each_col() + mu).t();
  X.replace(arma::datum::inf, arma::datum::nan);
  X.replace(-arma::datum::inf, arma::datum::nan);
  return X;
}


// Generate random numbers from Truncated Multivariate Slash distribution Slash(mu,Sigma,nu; (a,b))
// ------------------------------------------------------------------------------------------------------------
// Compute the slash DGF for a given values (u,nu,p)
double slash_g(double u, double nu, int p){
  double a = nu + 0.50*p;
  double gt = tgamma(a)*pow(0.5*u, -a)*R::pgamma(1.0, a, 2.0/u, 1, 0);
  return gt;
}
// Compute the inverse of the slash DGF using Brent's algorithm
double r8_epsilon ( ){
  const double value = 2.220446049250313E-016;
  return value;
}
double BrentMethod(double y1, double nu, int p1){
  double a = 0.0001;  double b = 1e5;  double t = 1e-10;  double macheps = r8_epsilon( );
  double c, d, e, fa, fb, fc, m, tol, p, q, r, s, sa, sb;

  sa = a;
  sb = b;
  fa = slash_g(sa, nu, p1) - y1;
  fb = slash_g(sb, nu, p1) - y1;
  c = sa;
  fc = fa;
  e = sb - sa;
  d = e;

  for ( ; ; )
  {
    if (std::fabs(fc) < std::fabs(fb)){
      sa = sb;
      sb = c;
      c = sa;
      fa = fb;
      fb = fc;
      fc = fa;
    }
    tol = 2.0*macheps*std::fabs(sb) + t;
    m = 0.5*(c - sb);

    if (std::fabs(m) <= tol || fb == 0.0 ){
      break;
    }

    if (std::fabs(e) < tol || std::fabs(fa) <= std::fabs(fb)){
      e = m; d = e;
    } else {
      s = fb/fa;
      if (sa == c){
        p = 2.0*m*s;
        q = 1.0 - s;
      } else {
        q = fa/fc;
        r = fb/fc;
        p = s*(2.0*m*q*(q - r) - (sb - sa)*(r - 1.0));
        q = (q - 1.0)*(r - 1.0)*(s - 1.0);
      }
      if ( 0.0 < p ){
        q = -q;
      } else {
        p = -p;
      }
      s = e;
      e = d;
      if (2.0*p < 3.0*m*q - std::fabs(tol*q) && p < std::fabs(0.5*s*q)){
        d = p/q;
      } else {
        e = m;
        d = e;
      }
    }
    sa = sb;
    fa = fb;
    if (tol < std::fabs(d)){
      sb = sb + d;
    } else if ( 0.0 < m ){
      sb = sb + tol;
    } else {
      sb = sb - tol;
    }
    fb = slash_g(sb, nu, p1) - y1;
    if ((0.0 < fb && 0.0 < fc) || (fb <= 0.0 && fc <= 0.0)){
      c = sa;
      fc = fa;
      e = sb - sa;
      d = e;
    }
  }
  return sb;
}

// [[Rcpp::export]]
arma::mat rtslash(int n, const double nu, const arma::vec mu, const arma::mat Sigma,
                  const arma::vec a, const arma::vec b, int burn, int lag){
  int m = lag*n + burn;
  int p = Sigma.n_cols;
  arma::vec s = arma::sqrt(arma::diagvec(Sigma));
  arma::mat R = Sigma%(1.0/(s * s.t()));
  arma::mat Rinv = arma::inv(R);
  arma::mat X(n, p, fill::zeros);

  arma::vec lower = (a - mu)/s;
  arma::vec upper = (b - mu)/s;
  arma::vec x = lower;
  arma::uvec q1 = find_nonfinite(x);
  x.elem(q1) = upper.elem(q1);
  x.replace(arma::datum::inf, 0.0);

  double delta, y, kap;
  arma::umat minusj = indicatorFun(p);
  int count = 1;
  for (int i=0; i<m; i++){
    delta = as_scalar(x.t()*Rinv*x);
    y   = arma::randu<double>()*slash_g(delta, nu, p);
    kap = BrentMethod(y, nu, p);
    x = generateXi(p, minusj, x, Rinv, lower, upper, kap); // Simulate x
    if (i==(burn + count*lag - 1)){
      X.row(count-1) = x.t();
      count++;
    }
  }
  X = X.t();
  X = X.each_col()%s;
  X = (X.each_col() + mu).t();
  X.replace(arma::datum::inf, arma::datum::nan);
  X.replace(-arma::datum::inf, arma::datum::nan);
  return X;
}


// Generate random numbers from Truncated Multivariate Contaminated Normal distribution CN(mu,Sigma,nu,rho; (a,b))
// ----------------------------------------------------------------------------------------------------------------
// Compute the inverse for the Contaminated normal DGF using Newton Raphson
double ginvCN(double nu, double rho, int p, double y){
  double t1 = -2.0*log(y);
  double c1 = nu*pow(rho, (0.5*p));
  double t2 = t1 + 2.0*(c1*exp(-0.5*rho*t1) + (1.0-nu)*exp(-0.5*t1) - y)/(rho*c1*exp(-0.5*rho*t1) + (1.0-nu)*exp(-0.5*t1));
  while (std::fabs(t2 - t1) > 1e-10){
    t1 = t2;
    t2 = t1 + 2.0*(c1*exp(-0.5*rho*t1) + (1.0-nu)*exp(-0.5*t1) - y)/(rho*c1*exp(-0.5*rho*t1) + (1.0-nu)*exp(-0.5*t1));
  }
  if (t2 < 0.0){ t2 = 0.0;}
  return t2;
}

// [[Rcpp::export]]
arma::mat rtCN(int n, const double nu, const double rho, const arma::vec mu, const arma::mat Sigma,
               const arma::vec a, const arma::vec b, int burn, int lag){
  int m = lag*n + burn;
  int p = Sigma.n_cols;
  arma::vec s = arma::sqrt(arma::diagvec(Sigma));
  arma::mat R = Sigma%(1.0/(s * s.t()));
  arma::mat Rinv = arma::inv(R);
  arma::mat X(n, p, fill::zeros);

  arma::vec lower = (a - mu)/s;
  arma::vec upper = (b - mu)/s;
  arma::vec x   = lower;
  arma::uvec q1 = find_nonfinite(x);
  x.elem(q1)    = upper.elem(q1);
  x.replace(arma::datum::inf, 0.0);

  double delta, dgf, y, kap;
  arma::umat minusj = indicatorFun(p);
  int count = 1;
  for (int i=0; i<m; i++){
    delta = as_scalar(x.t()*Rinv*x);
    dgf   = nu*pow(rho, (0.5*p))*exp(-0.5*rho*delta) + (1.0-nu)*exp(-0.5*delta);
    y     = arma::randu<double>()*(dgf);
    kap   = ginvCN(nu, rho, p, y);
    x = generateXi(p, minusj, x, Rinv, lower, upper, kap); // Simulate x
    if (i==(burn + count*lag - 1)){
      X.row(count-1) = x.t();
      count++;
    }
  }
  X = X.t();
  X = X.each_col()%s;
  X = (X.each_col() + mu).t();
  X.replace(arma::datum::inf, arma::datum::nan);
  X.replace(-arma::datum::inf, arma::datum::nan);
  return X;
}


// Generate random numbers from any Truncated Multivariate distribution given DGF f(mu,Sigma; gFun, (a,b))
// ------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
arma::mat randomG(int n, arma::vec mu, arma::mat Sigma, arma::vec a, arma::vec b, Function gFUN, Function ginvFUN,
                  int burn, int lag){
  int m = lag*n + burn;
  int p = Sigma.n_cols;
  arma::vec s = arma::sqrt(arma::diagvec(Sigma));
  arma::mat R = Sigma%(1.0/(s * s.t()));
  arma::mat Rinv = arma::inv(R);
  arma::mat X(n, p, fill::zeros);

  arma::vec lower = (a - mu)/s;
  arma::vec upper = (b - mu)/s;
  arma::vec x   = lower;
  arma::uvec q1 = find_nonfinite(x);
  x.elem(q1)    = upper.elem(q1);
  x.replace(arma::datum::inf, 0.0);

  double delta, y, kap;
  arma::umat minusj = indicatorFun(p);
  int count = 1;
  for (int i=0; i<m; i++){
    delta = as_scalar(x.t()*Rinv*x);
    y   = arma::randu<double>()*as<double>(gFUN(delta));
    kap = as<double>(ginvFUN(y));
    x = generateXi(p, minusj, x, Rinv, lower, upper, kap); // Simulate x
    if (i==(burn + count*lag - 1)){
      X.row(count-1) = x.t();
      count++;
    }
  }
  X = X.t();
  X = X.each_col()%s;
  X = (X.each_col() + mu).t();
  X.replace(arma::datum::inf, arma::datum::nan);
  X.replace(-arma::datum::inf, arma::datum::nan);
  return X;
}


// COMPUTE MOMENTS - E(Y), E(YYt), Var(Y)

// Compute moments from truncated multivariate Student-t distribution
// ------------------------------------------------------------------------------------------------------------
Rcpp::List tuniv(arma::vec mu, arma::mat sigma, double nu, arma::vec lower, arma::vec upper, int n2,
           int n, int burn, int thinning){
  double md0 = arma::datum::nan;
  double vr0 = arma::datum::nan;

  if (n2 == 1){ // Exists mean and variance for all nu > 0
    double s11 = sqrt(as_scalar(sigma));
    double a = as_scalar(lower - mu)/s11;
    double b = as_scalar(upper - mu)/s11;
    double alpha0 = R::pt(b, nu, 1, 0) - R::pt(a, nu, 1, 0);
    double meanx  = 0.0, varx = 0.0;
    if (nu == 1.0){
      meanx = log((1.0 + b*b)/(1.0 + a*a))/(2.0*pii*alpha0);
    } else {
      meanx = tgamma(0.5*(nu + 1.0))/(alpha0*sqrt(nu*pii)*tgamma(0.5*nu))*(nu/(1.0-nu))*(pow(1.0 + b*b/nu, -0.5*(nu-1.0)) - pow(1.0 + a*a/nu, -0.5*(nu-1.0)));
    }
    md0 = as_scalar(mu) + s11*meanx; // Mean of Y
    if (nu > 2.0){
      varx = (nu*(nu-1.0)/((nu-2.0)*alpha0))*(R::pt(b*sqrt((nu-2.0)/nu), nu-2.0, 1, 0) - R::pt(a*sqrt((nu-2.0)/nu), nu-2.0, 1, 0)) - nu - meanx*meanx;
      vr0 = varx*as_scalar(sigma);  // Variance  of Y
    } else {
      if (nu == 2.0){
        varx = (asinh(b/sqrt(2.0)) - asinh(a/sqrt(2.0)))/alpha0 - 2.0 - meanx*meanx;
        vr0 = varx*as_scalar(sigma);  // Variance  of Y
      } else {
        arma::mat gen = rttrunc(n, nu, mu, sigma, lower, upper, burn, thinning);
        vr0 = as_scalar(var(gen));
      }
    }
  } else {

    if (nu > 1.0){ // Exists the mean
      double s11 = sqrt(as_scalar(sigma));
      double a, b;
      if(lower.is_finite()){ a = as_scalar(lower - mu)/s11; } else { a = -1e40; }
      if(upper.is_finite()){ b = as_scalar(upper - mu)/s11; } else { b = 1e40; }
      double alpha0 = R::pt(b, nu, 1, 0) - R::pt(a, nu, 1, 0);
      double meanx  = tgamma(0.5*(nu + 1.0))/(alpha0*sqrt(nu*pii)*tgamma(0.5*nu))*(nu/(1.0-nu))*(pow(1.0 + b*b/nu, -0.5*(nu-1.0)) - pow(1.0 + a*a/nu, -0.5*(nu-1.0)));
      md0 = as_scalar(mu) + s11*meanx;   // Mean of Y
      if (nu > 2.0){ // Exists the variance
        double varx = (nu*(nu-1.0)/((nu-2.0)*alpha0))*(R::pt(b*sqrt((nu-2.0)/nu), nu-2.0, 1, 0) - R::pt(a*sqrt((nu-2.0)/nu), nu-2.0, 1, 0)) - nu - meanx*meanx;
        vr0 = varx*as_scalar(sigma);     // Variance  of Y
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("mean") = md0,
                            Rcpp::Named("var")  = vr0);
}

// [[Rcpp::export]]
Rcpp::List Tmoment(const arma::vec mu, const arma::mat Sigma, const double nu, const arma::vec lower,
                   const arma::vec upper, int n, int burn, int thinning){
  int p = mu.size();
  // Non-truncated variables
  arma::uvec ind2  = intersect(find_nonfinite(lower), find_nonfinite(upper));
  int lind = ind2.size();
  // Results
  arma::vec mean0(p, fill::zeros);       mean0.replace(0, arma::datum::nan);
  arma::mat var0(p, p, fill::zeros);     var0.replace(0, arma::datum::nan);
  arma::mat mom20(p, p, fill::zeros);    mom20.replace(0, arma::datum::nan);

  if (lind == p){ // Non-truncated variables
    if (nu > 1.0){
      mean0 = mu;
      if (nu > 2.0){ var0 = Sigma*(nu)/(nu - 2.0); mom20 = var0 + mean0*mean0.t(); }
    }
  } else {
    // Doubly truncated variables
    arma::uvec ind0 = intersect(find_finite(lower), find_finite(upper));
    int n2 = ind0.size();

    if ((lind==0) & (p==1)){ // All variables are truncated: univariate case
      Rcpp::List momentos = tuniv(mu, Sigma, nu, lower, upper, n2, n, burn, thinning);
      mean0(0) = as<double>(momentos["mean"]);
      var0(0,0) = as<double>(momentos["var"]);
      mom20(0,0) = var0(0,0) + mean0(0)*mean0(0);
    } else {

      if ((lind==0) & (p>1)){ // All variables are truncated: p-variate case
        double d = n2 + nu;
        if (d > 1.0){ // Exists mean
          arma::mat gen = rttrunc(n, nu, mu, Sigma, lower, upper, burn, thinning);
          mean0 = (mean(gen,0)).t();
          if (d > 2.0){ // Exists variance (all)
            var0  = cov(gen); mom20 = var0 + mean0*mean0.t();
          } else {
            if (n2 > 0){ // Exists variance (bounded), covariance (all)
              // Non-bounded region
              arma::uvec ind3 = unique(join_vert(find_nonfinite(lower), find_nonfinite(upper)));
              arma::mat Anan(ind3.size(), ind3.size(), fill::zeros);    Anan.replace(0, arma::datum::nan);
              var0  = cov(gen);                var0(ind3,ind3)  = Anan;
              mom20 = var0 + mean0*mean0.t();  mom20(ind3,ind3) = Anan;
            }
          }
        }
      } else {

        if ((lind==(p-1)) & (p>1)){ // One variable is truncated: p-variate case
          double d = n2 + nu;
          if (d > 1.0){ // Exists mean
            // Truncated variables
            arma::uvec ind1 = unique(join_vert(find_finite(lower), find_finite(upper)));
            Rcpp::List momentos = tuniv(mu(ind1), Sigma(ind1,ind1), nu, lower(ind1), upper(ind1), n2, n, burn, thinning);
            arma::vec mx(1);    mx(0) = as<double>(momentos["mean"]);
            arma::mat vx(1,1);  vx(0,0) = as<double>(momentos["var"]);
            mean0(ind1) = mx;
            mean0(ind2) = mu(ind2) + as_scalar((mean0(ind1) - mu(ind1))/Sigma(ind1,ind1))*Sigma(ind2,ind1);
            if (d > 2.0){ // Exists variance (all)
              var0(ind1,ind1) = vx;
              double omega21  = (nu + as_scalar((var0(ind1,ind1) + (mean0(ind1)-mu(ind1))*(mean0(ind1)-mu(ind1)))/Sigma(ind1,ind1)))/(nu - 1.0);
              var0(ind2,ind2) = omega21*Sigma(ind2,ind2) - (omega21 - as_scalar(var0(ind1,ind1)/Sigma(ind1,ind1)))/as_scalar(Sigma(ind1,ind1))*Sigma(ind2,ind1)*Sigma(ind1,ind2);
              var0(ind2,ind1) = as_scalar(var0(ind1,ind1)/Sigma(ind1,ind1))*Sigma(ind2,ind1);
              var0(ind1,ind2) = (var0(ind2,ind1)).t();
              mom20 = var0 + mean0*mean0.t();
            } else {
              if (n2 > 0){// Exists variance (bounded), covariance (all)
                var0(ind0,ind0) = vx;
                var0(ind2,ind0) = as_scalar(var0(ind0,ind0)/Sigma(ind0,ind0))*Sigma(ind2,ind0);
                var0(ind0,ind2) = (var0(ind2,ind0)).t();
                mom20 = var0 + mean0*mean0.t();
              }
            }
          }
        } else {

          // The number of truncated variables varies between 2 and p-1
          double d = n2 + nu;
          if (d > 1.0){ // Exists mean
            // Truncated variables
            arma::uvec ind1 = unique(join_vert(find_finite(lower), find_finite(upper)));
            arma::mat gen = rttrunc(n, nu, mu(ind1), Sigma(ind1,ind1), lower(ind1), upper(ind1), burn, thinning);
            arma::mat sigInv = arma::inv(Sigma(ind1,ind1));
            mean0(ind1) = (mean(gen,0)).t();
            mean0(ind2) = mu(ind2) + Sigma(ind2,ind1)*sigInv*(mean0(ind1) - mu(ind1));
            if (d > 2.0){ // Exists variance (all)
              var0(ind1,ind1) = cov(gen);
              arma::mat Iden  = eye(ind1.size(), ind1.size());
              double omega21  = (nu + trace(var0(ind1,ind1)*sigInv) + as_scalar((mean0(ind1) - mu(ind1)).t()*sigInv*(mean0(ind1) - mu(ind1))))/(nu + ind1.size() - 2.0);
              var0(ind2,ind2) = omega21*Sigma(ind2,ind2) - Sigma(ind2,ind1)*sigInv*(omega21*Iden - var0(ind1,ind1)*sigInv)*Sigma(ind1,ind2);
              var0(ind2,ind1) = Sigma(ind2,ind1)*sigInv*var0(ind1,ind1);
              var0(ind1,ind2) = (var0(ind2,ind1)).t();
              mom20 = var0 + mean0*mean0.t();
            } else {
              if (n2 > 0){ // Exists variance (bounded), covariance (all)
                var0(ind1,ind1) = cov(gen);
                var0(ind2,ind1) = Sigma(ind2,ind1)*sigInv*var0(ind1,ind1);
                var0(ind1,ind2) = (var0(ind2,ind1)).t();
                mom20 = var0 + mean0*mean0.t();
                // Non-bounded region
                arma::uvec ind3 = unique(join_vert(find_nonfinite(lower), find_nonfinite(upper)));
                arma::mat Anan(ind3.size(), ind3.size(), fill::zeros);    Anan.replace(0, arma::datum::nan);
                var0(ind3,ind3) = Anan;     mom20(ind3,ind3) = Anan;
              }
            }
          }
        } // End if
      } // End if
    } // End if
  } // End if
  return Rcpp::List::create(Rcpp::Named("EY")   = mean0,
                            Rcpp::Named("EYY")  = 0.5*(mom20 + mom20.t()),
                            Rcpp::Named("VarY") = 0.5*(var0 + var0.t()));
}


// Compute moments from truncated multivariate Normal distribution
// ------------------------------------------------------------------------------------------------------------
Rcpp::List uniNorm(arma::vec mu1, arma::mat sigma1, arma::vec a1, arma::vec b1, int n, int burn, int lag){
  // Moments for univariate case
  double meany = 0.0; double vary = 0.0;
  double s11 = sqrt(as_scalar(sigma1));
  double a, b;
  if(a1.is_finite()){ a = as_scalar((a1 - mu1))/s11; } else { a = -1e40; }
  if(b1.is_finite()){ b = as_scalar((b1 - mu1))/s11; } else { b = 1e40; }
  double den  = R::pnorm(b, 0.0, 1.0, 1, 0) - R::pnorm(a, 0.0, 1.0, 1, 0);
  if (den < 1e-250){
    arma::mat gen = rtnormal(n, mu1, sigma1, a1, b1, burn, lag);
    meany = as_scalar(mean(gen));
    vary = as_scalar(var(gen));
  } else {
    double pdfa = R::dnorm(a, 0.0, 1.0, 0);
    double pdfb = R::dnorm(b, 0.0, 1.0, 0);
    meany = as_scalar(mu1) + s11*((pdfa - pdfb)/den);
    vary = as_scalar(sigma1)*(1.0 + (a*pdfa - b*pdfb)/den - ((pdfa - pdfb)*(pdfa - pdfb)/(den*den)));
  }
  return Rcpp::List::create(Rcpp::Named("mean")   = meany,
                            Rcpp::Named("var") = vary);
}

// [[Rcpp::export]]
Rcpp::List Nmoment(const arma::vec mu, const arma::mat Sigma, const arma::vec lower, const arma::vec upper,
                   int n, int burn, int thinning){
  int p = mu.size();
  // Non-truncated variables
  arma::uvec ind2 = intersect(find_nonfinite(lower), find_nonfinite(upper));
  int lind = ind2.size();
  // Results
  arma::vec mean0(p);  arma::mat var0(p,p);  arma::mat mom20(p,p);

  if (lind==p){// Non-truncated variables
    mean0 = mu; var0 = Sigma; mom20 = var0 + mean0*mean0.t();
  } else {

    if ((lind==0) & (p==1)){ // All variables are truncated: univariate case
      Rcpp::List momentos = uniNorm(mu, Sigma, lower, upper, n, burn, thinning);
      mean0(0)   = as<double>(momentos["mean"]);
      var0(0,0)  = as<double>(momentos["var"]);
      mom20(0,0) = var0(0,0) + mean0(0)*mean0(0);
    } else {

      if ((lind==0) & (p>1)){ //All variables are truncated: p-variate case
        arma::mat gen = rtnormal(n, mu, Sigma, lower, upper, burn, thinning);
        mean0 = (mean(gen,0)).t();
        var0  = cov(gen);
        mom20 = var0 + mean0*mean0.t();
      } else {
        // Truncated variables
        arma::uvec ind1 = unique(join_vert(find_finite(lower), find_finite(upper)));

        if ((lind==(p-1)) & (p>1)){ // One variable is truncated: p-variate case
          Rcpp::List momentos = uniNorm(mu(ind1), Sigma(ind1,ind1), lower(ind1), upper(ind1), n, burn, thinning);
          arma::vec mx(1);   mx(0) = as<double>(momentos["mean"]);
          arma::mat vx(1,1); vx(0,0) = as<double>(momentos["var"]);
          mean0(ind1) = mx;
          mean0(ind2) = mu(ind2) + as_scalar((mean0(ind1) - mu(ind1))/Sigma(ind1,ind1))*Sigma(ind2,ind1);
          var0(ind1,ind1) = vx;
          var0(ind2,ind1) = as_scalar(var0(ind1,ind1)/Sigma(ind1,ind1))*Sigma(ind2,ind1);
          var0(ind1,ind2) = (var0(ind2,ind1)).t();
          var0(ind2,ind2) = Sigma(ind2,ind2) - ((1.0 - as_scalar(var0(ind1,ind1)/Sigma(ind1,ind1)))/as_scalar(Sigma(ind1,ind1)))*Sigma(ind2,ind1)*Sigma(ind1,ind2);
          mom20 = var0 + mean0*mean0.t();
        } else {

          if ((lind>0) & (lind<(p-1))){ // The number of truncated variables varies between 2 and p-1
            arma::mat Iden = eye(ind1.size(), ind1.size());
            arma::mat sigInv = arma::inv(Sigma(ind1,ind1));
            arma::mat gen = rtnormal(n, mu(ind1), Sigma(ind1,ind1), lower(ind1), upper(ind1), burn, thinning);
            mean0(ind1) = (mean(gen,0)).t();
            mean0(ind2) = mu(ind2) + Sigma(ind2,ind1)*sigInv*(mean0(ind1) - mu(ind1));
            var0(ind1,ind1) = cov(gen);
            var0(ind2,ind2) = Sigma(ind2,ind2) - Sigma(ind2,ind1)*sigInv*(Iden - var0(ind1,ind1)*sigInv)*Sigma(ind1,ind2);
            var0(ind2,ind1) = Sigma(ind2,ind1)*sigInv*var0(ind1,ind1);
            var0(ind1,ind2) = (var0(ind2,ind1)).t();
            mom20 = var0 + mean0*mean0.t();
          }
        }
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("EY")   = mean0,
                            Rcpp::Named("EYY")  = 0.5*(mom20 + mom20.t()),
                            Rcpp::Named("VarY") = 0.5*(var0 + var0.t()));
}


// Compute moments from truncated multivariate Power Exponential distribution
// ------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
Rcpp::List PEmoment(const arma::vec mu, const arma::mat Sigma, const double beta, const arma::vec lower,
                    const arma::vec upper, int n, int burn, int thinning){
  int p = mu.size();
  // Non-truncated variables
  arma::uvec ind2 = intersect(find_nonfinite(lower), find_nonfinite(upper));
  int lind = ind2.size();
  // Results
  arma::vec mean0(p); arma::mat mom20(p,p); arma::mat var0(p,p);

  if (lind==p){ // Non-truncated variables
    mean0 = mu;
    var0  = (pow(2.0, (1.0/beta))*tgamma((p+2.0)/(2.0*beta))/(p*tgamma(p/(2.0*beta))))*Sigma;
    mom20 = var0 + mean0*mean0.t();
  } else {

    // There is at least one truncated variable
    arma::uvec ind1 = unique(join_vert(find_finite(lower), find_finite(upper)));
    arma::mat gen = rtPE(n, beta, mu, Sigma, lower, upper, burn, thinning);
    mean0 = (mean(gen,0)).t();
    var0  = cov(gen);
    mom20 = var0 + mean0*mean0.t();
  }
  return Rcpp::List::create(Rcpp::Named("EY")   = mean0,
                            Rcpp::Named("EYY")  = 0.5*(mom20 + mom20.t()),
                            Rcpp::Named("VarY") = 0.5*(var0 + var0.t()));
}


// Compute moments from truncated multivariate Pearson VII distribution
// ------------------------------------------------------------------------------------------------------------
Rcpp::List PVIIuni(arma::vec mu, arma::mat Sigma, double N, double nu, arma::vec lower, arma::vec upper,
                   int n2, int n, int burn, int thinning){
  double md0 = arma::datum::nan;
  double vr0 = arma::datum::nan;
  if (n2 == 1){ // Exists mean and variance
    double s11 = sqrt(as_scalar(Sigma));
    double a = as_scalar(lower - mu)/s11;
    double b = as_scalar(upper - mu)/s11;
    double nu1 = sqrt((2.0*N - 1.0)/nu);
    double alpha0 = R::pt(nu1*b, 2.0*N-1.0, 1, 0) - R::pt(nu1*a, 2.0*N-1.0, 1, 0);
    double meanx  = 0.0, varx = 0.0;
    if (N == 1.0){
      meanx = sqrt(nu)/(2.0*alpha0*pii)*log((nu + b*b)/(nu + a*a));
    } else {
      meanx = sqrt(nu)*tgamma(N)/(2.0*(1-N)*alpha0*sqpii*tgamma(N-0.5))*(pow(1.0 + b*b/nu, 1.0-N) - pow(1.0 + a*a/nu, 1.0-N));
    }
    md0 = as_scalar(mu) + s11*meanx; // Mean of Y
    if (N > 1.50){
      double nu2 = sqrt((2.0*N - 3.0)/nu);
      varx = nu*(N-1.0)/(alpha0*(N-1.5))*(R::pt(nu2*b, 2.0*N-3.0, 1, 0) - R::pt(nu2*a, 2.0*N-3.0, 1, 0)) - nu - meanx*meanx;
      vr0 = varx*as_scalar(Sigma);
    } else {
      if (N == 1.50){
        varx = nu/(2.0*alpha0)*(asinh(b/sqrt(nu)) - asinh(a/sqrt(nu))) - nu - meanx*meanx;
        vr0 = varx*as_scalar(Sigma);
      } else {
        arma::mat gen = rtPVII(n, N, nu, mu, Sigma, lower, upper, burn, thinning);
        vr0  = as_scalar(var(gen));
      }
    }
  } else {
    if (N > 1.0){ // Exists mean
      double s11 = sqrt(as_scalar(Sigma));
      double nu1 = sqrt((2.0*N - 1.0)/nu);
      double a, b;
      if(lower.is_finite()){ a = as_scalar(lower - mu)/s11; } else { a = -1e40; }
      if(upper.is_finite()){ b = as_scalar(upper - mu)/s11; } else { b = 1e40; }
      double alpha0 = R::pt(nu1*b, 2.0*N-1.0, 1, 0) - R::pt(nu1*a, 2.0*N-1.0, 1, 0);
      double meanx = sqrt(nu)*tgamma(N)/(2.0*(1-N)*alpha0*sqpii*tgamma(N-0.5))*(pow(1.0 + b*b/nu, 1.0-N) - pow(1.0 + a*a/nu, 1.0-N));
      md0 = as_scalar(mu) + s11*meanx;
      if (N > 1.50){ // Exists variance
        double nu2  = sqrt((2.0*N-3.0)/nu);
        double varx = nu*(N-1.0)/(alpha0*(N-1.5))*(R::pt(nu2*b, 2.0*N-3.0, 1, 0) - R::pt(nu2*a, 2.0*N-3.0, 1, 0)) - nu - meanx*meanx;
        vr0 = varx*as_scalar(Sigma);
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("mean") = md0,
                            Rcpp::Named("var")  = vr0);
}

// [[Rcpp::export]]
Rcpp::List PVIImoment(const arma::vec mu, const arma::mat Sigma, const double N, const double nu, const arma::vec lower,
                      const arma::vec upper, int n, int burn, int thinning){
  int p = mu.size();
  // Non-truncated variables
  arma::uvec ind2  = intersect(find_nonfinite(lower), find_nonfinite(upper));
  int lind = ind2.size();
  // Results
  arma::vec mean0(p, fill::zeros);       mean0.replace(0, arma::datum::nan);
  arma::mat var0(p, p, fill::zeros);     var0.replace(0, arma::datum::nan);
  arma::mat mom20(p, p, fill::zeros);    mom20.replace(0, arma::datum::nan);

  if (lind == p){ // Non-truncated variables
    if (N > 0.5*(p+1.0)){
      mean0 = mu;
      if (N > 0.5*(p+2.0)){ var0 = nu*Sigma/(2.0*N - p - 2.0); mom20 = var0 + mean0*mean0.t(); }
    }
  } else {
    // Doubly truncated variables
    arma::uvec ind0 = intersect(find_finite(lower), find_finite(upper));
    int n2 = ind0.size();

    if ((lind==0) & (p==1)){ // All variables are truncated: univariate case
      Rcpp::List momentos = PVIIuni(mu, Sigma, N, nu, lower, upper, n2, n, burn, thinning);
      mean0(0) = as<double>(momentos["mean"]);
      var0(0,0) = as<double>(momentos["var"]);
      mom20(0,0) = var0(0,0) + mean0(0)*mean0(0);
    } else {

      if ((lind==0) & (p>1)){ // All variables are truncated: p-variate case
        double d = p - n2;
        if ( N > 0.5*(d+1.0)){ // Exists mean
          arma::mat gen = rtPVII(n, N, nu, mu, Sigma, lower, upper, burn, thinning);
          mean0 = (mean(gen,0)).t();
          if (N > 0.5*(d+2.0)){ // Exists variance
            var0  = cov(gen);  mom20 = var0 + mean0*mean0.t();
          } else {
            if (n2 > 0){ // Exists variance(bounded), covariance(all)
              var0  = cov(gen);  mom20 = var0 + mean0*mean0.t();
              // Non-bounded region
              arma::uvec ind3 = unique(join_vert(find_nonfinite(lower), find_nonfinite(upper)));
              arma::mat Anan(ind3.size(), ind3.size(), fill::zeros);    Anan.replace(0, arma::datum::nan);
              var0(ind3,ind3) = Anan;  mom20(ind3,ind3) = Anan;
            }
          }
        }
      } else {

        if ((lind==(p-1)) & (p>1)){ // One variable is truncated: p-variate case
          double d = p - n2;
          if (N > 0.5*(d+1.0)){ // Exists mean
            // Truncated variables
            arma::uvec ind1 = unique(join_vert(find_finite(lower), find_finite(upper)));
            Rcpp::List momentos = PVIIuni(mu(ind1), Sigma(ind1,ind1), N-0.5*lind, nu, lower(ind1), upper(ind1), n2, n, burn, thinning);
            arma::vec mx(1);   mx(0) = as<double>(momentos["mean"]);
            arma::mat vx(1,1); vx(0,0) = as<double>(momentos["var"]);
            mean0(ind1) = mx;
            mean0(ind2) = mu(ind2) + as_scalar((mean0(ind1) - mu(ind1))/Sigma(ind1,ind1))*Sigma(ind2,ind1);
            if (N > 0.5*(d+2.0)){ // Exists variance
              var0(ind1,ind1) = vx;
              double omega21  = (nu + as_scalar((var0(ind1,ind1) + (mean0(ind1)-mu(ind1))*(mean0(ind1)-mu(ind1)))/Sigma(ind1,ind1)))/(2.0*N - lind - 2.0);
              var0(ind2,ind2) = omega21*Sigma(ind2,ind2) - (omega21 - as_scalar(var0(ind1,ind1)/Sigma(ind1,ind1)))/as_scalar(Sigma(ind1,ind1))*Sigma(ind2,ind1)*Sigma(ind1,ind2);
              var0(ind2,ind1) = as_scalar(var0(ind1,ind1)/Sigma(ind1,ind1))*Sigma(ind2,ind1);
              var0(ind1,ind2) = (var0(ind2,ind1)).t();
              mom20 = var0 + mean0*mean0.t();
            } else {
              if (n2 > 0){ // Exists variance(bounded), covariance(all)
                var0(ind0,ind0) = vx;
                var0(ind2,ind0) = as_scalar(var0(ind0,ind0)/Sigma(ind0,ind0))*Sigma(ind2,ind0);
                var0(ind0,ind2) = (var0(ind2,ind0)).t();
                mom20 = var0 + mean0*mean0.t();
              }
            }
          }
        } else {

          // The number of truncated variables varies between 2 and p-1
          double d = p - n2;
          if (N > 0.5*(d+1.0)){ // Exists mean
            // Truncated variables
            arma::uvec ind1 = unique(join_vert(find_finite(lower), find_finite(upper)));
            arma::mat gen = rtPVII(n, (N-0.5*lind), nu, mu(ind1), Sigma(ind1,ind1), lower(ind1), upper(ind1), burn, thinning);
            arma::mat sigInv = arma::inv(Sigma(ind1,ind1));
            mean0(ind1) = (mean(gen,0)).t();
            mean0(ind2) = mu(ind2) + Sigma(ind2,ind1)*sigInv*(mean0(ind1) - mu(ind1));
            if (N > 0.5*(d+2.0)){ // Exists variance
              var0(ind1,ind1) = cov(gen);
              arma::mat Iden = eye(ind1.size(), ind1.size());
              double omega21 = (nu + trace(var0(ind1,ind1)*sigInv) + as_scalar((mean0(ind1) - mu(ind1)).t()*sigInv*(mean0(ind1) - mu(ind1))))/(2.0*N - lind - 2.0);
              var0(ind2,ind2) = omega21*Sigma(ind2,ind2) - Sigma(ind2,ind1)*sigInv*(omega21*Iden - var0(ind1,ind1)*sigInv)*Sigma(ind1,ind2);
              var0(ind2,ind1) = Sigma(ind2,ind1)*sigInv*var0(ind1,ind1);
              var0(ind1,ind2) = (var0(ind2,ind1)).t();
              mom20 = var0 + mean0*mean0.t();
            } else {
              if (n2 > 0){ // Exists variance(bounded), covariance(all)
                var0(ind1,ind1) = cov(gen);
                var0(ind2,ind1) = Sigma(ind2,ind1)*sigInv*var0(ind1,ind1);
                var0(ind1,ind2) = (var0(ind2,ind1)).t();
                mom20 = var0 + mean0*mean0.t();
                // Non-bounded region
                arma::uvec ind3 = unique(join_vert(find_nonfinite(lower), find_nonfinite(upper)));
                arma::mat Anan(ind3.size(), ind3.size(), fill::zeros);    Anan.replace(0, arma::datum::nan);
                var0(ind3,ind3) = Anan;      mom20(ind3,ind3) = Anan;
              }
            }
          }
        } // End if
      } // End if
    } // End if
  } // End if
  return Rcpp::List::create(Rcpp::Named("EY")   = mean0,
                            Rcpp::Named("EYY")  = 0.5*(mom20 + mom20.t()),
                            Rcpp::Named("VarY") = 0.5*(var0 + var0.t()));
}


// Compute moments from truncated multivariate Slash distribution
// ------------------------------------------------------------------------------------------------------------

// w2.1 for the Slash distribution
double wSlash(arma::mat X, arma::vec mu, arma::mat Sinv, double q, int p){
  int n1 = X.n_rows;
  double cont = 0.0; double delta = 0.0;
  for (int i=0; i<n1; i++){
    delta = as_scalar(((X.row(i)).t() - mu).t()*Sinv*((X.row(i)).t() - mu));
    cont += slash_g(delta, q-1.0, p)/slash_g(delta, q, p);
  }
  double omega = cont/n1;
  return omega;
}

// [[Rcpp::export]]
Rcpp::List Slashmoment(const arma::vec mu, const arma::mat Sigma, const double nu, const arma::vec lower,
                       const arma::vec upper, int n, int burn, int thinning){

  int p = mu.size();
  // Non-truncated variables
  arma::uvec ind2  = intersect(find_nonfinite(lower), find_nonfinite(upper));
  int lind = ind2.size();
  // Results
  arma::vec mean0(p);
  arma::mat var0(p, p, fill::zeros);   var0.replace(0, arma::datum::nan);
  arma::mat mom20(p, p, fill::zeros);  mom20.replace(0, arma::datum::nan);

  if (lind==p){ // Non-truncated variables
    mean0 = mu;
    if (nu > 1.0){ var0 = (nu/(nu - 1.0))*Sigma; mom20 = var0 + mean0*mean0.t(); }
  } else {

    if (lind == 0){ // All variables are truncated: p-variate case
      arma::mat gen = rtslash(n, nu, mu, Sigma, lower, upper, burn, thinning);
      mean0 = (mean(gen,0)).t();
      if (nu > 1.0){ var0 = cov(gen); mom20 = var0 + mean0*mean0.t(); }
    } else {

      // The number of truncated variables varies between 1 and p-1
      arma::uvec ind1  = unique(join_vert(find_finite(lower), find_finite(upper)));   // Truncated variables
      arma::mat gen = rtslash(n, nu, mu(ind1), Sigma(ind1,ind1), lower(ind1), upper(ind1), burn, thinning);
      arma::mat sigInv = arma::inv(Sigma(ind1,ind1));
      mean0(ind1) = (mean(gen,0)).t();
      mean0(ind2) = mu(ind2) + Sigma(ind2,ind1)*sigInv*(mean0(ind1) - mu(ind1));
      if (nu > 1.0){
        arma::mat Iden  = eye(ind1.size(), ind1.size());
        double omega21  = wSlash(gen, mu(ind1), sigInv, nu, ind1.size());
        var0(ind1,ind1) = cov(gen);
        var0(ind2,ind2) = omega21*Sigma(ind2,ind2) - Sigma(ind2,ind1)*sigInv*(omega21*Iden - var0(ind1,ind1)*sigInv)*Sigma(ind1,ind2);
        var0(ind2,ind1) = Sigma(ind2,ind1)*sigInv*var0(ind1,ind1);
        var0(ind1,ind2) = (var0(ind2,ind1)).t();
        mom20 = var0 + mean0*mean0.t();
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("EY")   = mean0,
                            Rcpp::Named("EYY")  = 0.5*(mom20 + mom20.t()),
                            Rcpp::Named("VarY") = 0.5*(var0 + var0.t()));
}


// Compute moments from truncated multivariate Contaminated normal distribution
// ------------------------------------------------------------------------------------------------------------

// Compute the density of Multivariate normal distribution
static double const log2pi = log(2.0 * M_PI);
arma::vec dmvnorm1(arma::mat const &x,arma::rowvec const &mean,arma::mat const &sigma,bool const logd = false) {
  using arma::uword;
  uword const n = x.n_rows, xdim = x.n_cols;
  arma::vec out(n);
  arma::mat const rooti = arma::inv(trimatu(arma::chol(sigma)));
  double const rootisum = arma::sum(log(rooti.diag())),
    constants   = -(double)xdim/2.0 * log2pi,
    other_terms = rootisum + constants;
  arma::rowvec z;
  for (uword i = 0; i < n; i++) {
    z      = (x.row(i) - mean) * rooti;
    out(i) = other_terms - 0.5 * arma::dot(z,z);
  }
  if (logd){ return out; }
  return exp(out);
}

Rcpp::List CNunivar(arma::vec muk, arma::vec sigma1, double nu, double rho, arma::vec a1, arma::vec b1, int n, int burn, int lag){

  double meany = 0.0;  double vary = 0.0;
  // Variable 1: X~TN(mu,Sigma/rho)
  double s11 = sqrt(as_scalar(sigma1/rho));
  double a, b;
  if(a1.is_finite()){ a = as_scalar((a1 - muk))/s11; } else { a = -1e40; }
  if(b1.is_finite()){ b = as_scalar((b1 - muk))/s11; } else { b = 1e40; }
  double den1 = R::pnorm(b, 0.0, 1.0, 1, 0) - R::pnorm(a, 0.0, 1.0, 1, 0);
  // Variable 2: X~TN(mu,Sigma)
  double s22 = sqrt(as_scalar(sigma1));
  if(a1.is_finite()){ a = as_scalar((a1 - muk))/s22; } else { a = -1e40; }
  if(b1.is_finite()){ b = as_scalar((b1 - muk))/s22; } else { b = 1e40; }
  double den2 = R::pnorm(b, 0.0, 1.0, 1, 0) - R::pnorm(a, 0.0, 1.0, 1, 0);
  double pi0  = nu*den1 + (1.0 - nu)*den2;

  if (pi0 < 1e-250){
    arma::mat gen = rtCN(n, nu, rho, muk, sigma1, a1, b1, burn, lag);
    meany = as_scalar(mean(gen));
    vary = as_scalar(var(gen));
  } else {

    // Variable 1: X~TN(mu,Sigma/rho)
    Rcpp::List moment1 = uniNorm(muk, (sigma1/rho), a1, b1, n, burn, lag);
    double mu1  = as<double>(moment1["mean"]);
    double var1 = as<double>(moment1["var"]);
    // Variable 2: X~TN(mu,Sigma)
    Rcpp::List moment2 = uniNorm(muk, sigma1, a1, b1, n, burn, lag);
    double mu2  = as<double>(moment2["mean"]);
    double var2 = as<double>(moment2["var"]);
    // Moments
    meany = (nu*den1*mu1 + (1.0 - nu)*den2*mu2)/pi0;
    double e2y  = (nu*den1*(var1 + mu1*mu1) + (1.0 - nu)*den2*(var2 + mu2*mu2))/pi0;
    vary = e2y - meany*meany;
  }
  return Rcpp::List::create(Rcpp::Named("mean") = meany,
                            Rcpp::Named("var")  = vary);
}

// [[Rcpp::export]]
Rcpp::List CNmoment(const arma::vec mu, const arma::mat Sigma, const double nu, const double rho,
                    const arma::vec lower, const arma::vec upper, int n, int burn, int thinning){

  int p = mu.size();
  // Non-truncated variables
  arma::uvec ind2  = intersect(find_nonfinite(lower), find_nonfinite(upper));
  int lind = ind2.size();
  // Results
  arma::vec mean0(p);  arma::mat mom20(p,p);  arma::mat var0(p,p);

  if (lind==p){ // Non-truncated variables
    mean0 = mu;  var0  = (nu/rho + 1.0 - nu)*Sigma;  mom20 = var0 + mean0*mean0.t();
  } else {

    if ((lind==0) & (p==1)){ // All variables are truncated: univariate case
      Rcpp::List momentos = CNunivar(mu, Sigma, nu, rho, lower, upper, n, burn, thinning);
      mean0(0)   = as<double>(momentos["mean"]);
      var0(0,0)  = as<double>(momentos["var"]);
      mom20(0,0) = var0(0,0) + mean0(0)*mean0(0);
    } else {

      if ((lind==0) & (p>1)){ // All variables are truncated: p-variate case
        arma::mat gen = rtCN(n, nu, rho, mu, Sigma, lower, upper, burn, thinning);
        mean0 = (mean(gen,0)).t();
        var0  = cov(gen);
        mom20 = var0 + mean0*mean0.t();
      } else {
        // Truncated variables
        arma::uvec ind1 = unique(join_vert(find_finite(lower), find_finite(upper)));

        if ((lind==(p-1)) & (p>1)){ // One variable is truncated: p-variate case
          Rcpp::List momentos = CNunivar(mu(ind1), Sigma(ind1,ind1), nu, rho, lower(ind1), upper(ind1), n, burn, thinning);
          arma::vec mx(1);   mx(0) = as<double>(momentos["mean"]);
          arma::mat vx(1,1); vx(0,0) = as<double>(momentos["var"]);
          mean0(ind1) = mx;
          mean0(ind2) = mu(ind2) + as_scalar((mean0(ind1) - mu(ind1))/Sigma(ind1,ind1))*Sigma(ind2,ind1);

          // Variable 1: X~TN(mu,Sigma/rho)
          double s11 = sqrt(as_scalar(Sigma(ind1,ind1)/rho));
          double a, b;
          if((lower(ind1)).is_finite()){ a = as_scalar((lower(ind1) - mu(ind1))/s11); } else { a = -1e40; }
          if((upper(ind1)).is_finite()){ b = as_scalar((upper(ind1) - mu(ind1))/s11); } else { b = 1e40; }
          double d1 = R::pnorm(b, 0.0, 1.0, 1, 0) - R::pnorm(a, 0.0, 1.0, 1, 0);
          // Variable 2: X~TN(mu,Sigma)
          double s22 = sqrt(as_scalar(Sigma(ind1,ind1)));
          if((lower(ind1)).is_finite()){ a = as_scalar((lower(ind1) - mu(ind1))/s22); } else { a = -1e40; }
          if((upper(ind1)).is_finite()){ b = as_scalar((upper(ind1) - mu(ind1))/s22); } else { b = 1e40; }
          double d2 = R::pnorm(b, 0.0, 1.0, 1, 0) - R::pnorm(a, 0.0, 1.0, 1, 0);
          double omega1  = (nu*d1)/(nu*d1 + (1.0-nu)*d2);
          double omega21 = omega1/rho + 1.0 - omega1;

          var0(ind1,ind1) = vx;
          var0(ind2,ind2) = omega21*Sigma(ind2,ind2) - (omega21 - as_scalar(var0(ind1,ind1)/Sigma(ind1,ind1)))/as_scalar(Sigma(ind1,ind1))*Sigma(ind2,ind1)*Sigma(ind1,ind2);
          var0(ind2,ind1) = as_scalar(var0(ind1,ind1)/Sigma(ind1,ind1))*Sigma(ind2,ind1);
          var0(ind1,ind2) = (var0(ind2,ind1)).t();
          mom20 = var0 + mean0*mean0.t();
        } else {

          // The number of truncated variables varies between 2 and p-1
          arma::mat Iden   = eye(ind1.size(), ind1.size());
          arma::mat sigInv = arma::inv(Sigma(ind1,ind1));
          arma::mat gen  = rtCN(n, nu, rho, mu(ind1), Sigma(ind1,ind1), lower(ind1), upper(ind1), burn, thinning);
          arma::vec d1   = dmvnorm1(gen, (mu(ind1)).t(), (Sigma(ind1,ind1)/rho), false);
          arma::vec d2   = dmvnorm1(gen, (mu(ind1)).t(), Sigma(ind1,ind1), false);
          double omega1  = mean(nu*d1/(nu*d1 + (1.0-nu)*d2));
          double omega21 = omega1/rho + 1.0 - omega1;
          mean0(ind1) = (mean(gen,0)).t();
          mean0(ind2) = mu(ind2) + Sigma(ind2,ind1)*sigInv*(mean0(ind1) - mu(ind1));
          var0(ind1,ind1) = cov(gen);
          var0(ind2,ind2) = omega21*Sigma(ind2,ind2) - Sigma(ind2,ind1)*sigInv*(omega21*Iden - var0(ind1,ind1)*sigInv)*Sigma(ind1,ind2);
          var0(ind2,ind1) = Sigma(ind2,ind1)*sigInv*var0(ind1,ind1);
          var0(ind1,ind2) = (var0(ind2,ind1)).t();
          mom20 = var0 + mean0*mean0.t();
        }
      }
    }
  }
  return Rcpp::List::create(Rcpp::Named("EY")   = mean0,
                            Rcpp::Named("EYY")  = 0.5*(mom20 + mom20.t()),
                            Rcpp::Named("VarY") = 0.5*(var0 + var0.t()));
}
