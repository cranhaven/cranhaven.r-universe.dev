// [[Rcpp::depends(RcppArmadillo, roptim, RcppProgress)]]

#include <RcppArmadillo.h>
#include <roptim.h>
#include <progress.hpp>
#include <progress_bar.hpp>

using namespace Rcpp;
using namespace arma;
using namespace roptim;

//R_NilValue == NULL

// Functions to be minimized considering exponential, gaussian, matern, and power exponential spatial correlation
// ------------------------------------------------------------------------------------------------------------
// EXPONENTIAL
class optimExp : public Functor {
  private:
    const arma::mat A; // Distance matrix
    const arma::mat B; // EYY matrix
    const arma::vec C; // EY
    const arma::vec D; // Mean vector
    const double sig;  // Estimate of sigma2
  public:
    optimExp(const arma::mat a, const arma::mat b, const arma::vec c, const arma::vec d, const double sig) : A(a), B(b), C(c), D(d), sig(sig) {}

  double operator()(const arma::vec &x) override {
    double ldet, sign;
    double phie = x(0); double taue = x(1);
    arma::mat Iden = eye(A.n_rows, A.n_rows);
    arma::mat Psi1 = ((taue/sig)*Iden) + exp(-A/phie);
    Psi1 = 0.50*(Psi1 + Psi1.t());
    arma::mat PsiInv1 = Psi1.i();
    log_det(ldet, sign, Psi1);
    const double f = 0.50*ldet + (0.50/sig)*(trace(B*PsiInv1) - as_scalar(D.t()*PsiInv1*C + C.t()*PsiInv1*D - D.t()*PsiInv1*D));
    return f;
  }
};
// GAUSSIAN
class optimGau : public Functor {
  private:
    const arma::mat A; // Distance matrix
    const arma::mat B; // EYY matrix
    const arma::vec C; // EY
    const arma::vec D; // Mean vector
    const double sig;  // Estimate of sigma2
  public:
    optimGau(const arma::mat a, const arma::mat b, const arma::vec c, const arma::vec d, const double sig) : A(a), B(b), C(c), D(d), sig(sig) {}

  double operator()(const arma::vec &x) override {
    double ldet, sign;
    double phie = x(0); double taue = x(1);
    arma::mat Iden = eye(A.n_rows, A.n_rows);
    arma::mat Psi1 = ((taue/sig)*Iden) + exp(-pow(A/phie,2));
    Psi1 = 0.50*(Psi1 + Psi1.t());
    arma::mat PsiInv1 = Psi1.i();
    log_det(ldet, sign, Psi1);
    const double f = 0.50*ldet + (0.50/sig)*(trace(B*PsiInv1) - as_scalar(D.t()*PsiInv1*C + C.t()*PsiInv1*D - D.t()*PsiInv1*D));
    return f;
  }
};
// POWER EXPONENTIAL
class optimPExp : public Functor {
  private:
    const arma::mat A; // Distance matrix
    const arma::mat B; // EYY matrix
    const arma::vec C; // EY
    const arma::vec D; // Mean vector
    const double sig;  // Estimate of sigma2
    const double kap;
  public:
    optimPExp(const arma::mat a, const arma::mat b, const arma::vec c, const arma::vec d, const double sig, const double kap) : A(a), B(b), C(c), D(d), sig(sig), kap(kap) {}

  double operator()(const arma::vec &x) override {
    double ldet, sign;
    double phie = x(0); double taue = x(1);
    arma::mat Iden = eye(A.n_rows, A.n_rows);
    arma::mat Psi1 = ((taue/sig)*Iden) + exp(-pow(A/phie,kap));
    Psi1 = 0.50*(Psi1 + Psi1.t());
    arma::mat PsiInv1 = Psi1.i();
    log_det(ldet, sign, Psi1);
    const double f = 0.50*ldet + (0.50/sig)*(trace(B*PsiInv1) - as_scalar(D.t()*PsiInv1*C + C.t()*PsiInv1*D - D.t()*PsiInv1*D));
    return f;
  }
};
// MATERN
arma::mat BesselK(arma::mat A, double ka){
  Environment base("package:base");
  Function besselk("besselK");
  arma::mat BA = as<arma::mat>(besselk(A,ka));
  //arma::uword p1 = A.n_rows;
  //arma::mat BA(p1, p1, fill::zeros);
  //for (uword i=0; i<p1; i++){
  //  for (uword j=(i+1); j<p1; j++){
  //    BA(i,j) = BA(j,i) = boost::math::cyl_bessel_k(ka,A(i,j));
  //  }
  //}
  return BA;
}
class optimMat : public Functor {
  private:
    const arma::mat A; // Distance matrix
    const arma::mat B; // EYY matrix
    const arma::vec C; // EY
    const arma::vec D; // Mean vector
    const double sig;  // Estimate of sigma2
    const double kap;
  public:
    optimMat(const arma::mat a, const arma::mat b, const arma::vec c, const arma::vec d, const double sig, const double kap) : A(a), B(b), C(c), D(d), sig(sig), kap(kap) {}

  double operator()(const arma::vec &x) override {
    double ldet, sign;
    double phie = x(0); double taue = x(1);
    uword p1 = A.n_rows;
    arma::mat R(p1,p1,fill::zeros);
    arma::mat Iden = eye(p1,p1);
    arma::mat Abessel = A/phie; Abessel.replace(0,1);
    R = (1.0/(pow(2.0,kap-1.0)*tgamma(kap)))*pow(A/phie,kap)%BesselK(Abessel,kap);
    R.diag().ones();
    arma::mat Psi1 = ((taue/sig)*Iden) + R;
    Psi1 = 0.50*(Psi1 + Psi1.t());
    arma::mat PsiInv1 = Psi1.i();
    log_det(ldet, sign, Psi1);
    const double f = 0.50*ldet + (0.50/sig)*(trace(B*PsiInv1) - as_scalar(D.t()*PsiInv1*C + C.t()*PsiInv1*D - D.t()*PsiInv1*D));
    return f;
  }
};

// Minimize the negative log-likelihood function with respect to phi and tau2
// ------------------------------------------------------------------------------------------------------------
arma::vec optimlL(arma::vec rhoG, arma::mat dist1, arma::mat yy1, arma::vec y1, arma::vec media1, double sigma2,
                    arma::vec lower2, arma::vec upper2, String type, double kappa) {

    arma::vec optR(2,fill::zeros);
    if (type=="exponential"){
      // Negative log likelihood
      optimExp fun(dist1,yy1,y1,media1,sigma2);
      Roptim<optimExp> opt("L-BFGS-B");
      opt.set_lower(lower2);
      opt.set_upper(upper2);
      opt.control.trace = 0;
      arma::vec x = rhoG;
      opt.minimize(fun, x);
      optR = opt.par();
    } else {

      if (type=="gaussian"){
        // Negative log likelihood
        optimGau fun(dist1,yy1,y1,media1,sigma2);
        Roptim<optimGau> opt("L-BFGS-B");
        opt.set_lower(lower2);
        opt.set_upper(upper2);
        opt.control.trace = 0;
        arma::vec x = rhoG;
        opt.minimize(fun, x);
        optR = opt.par();
      } else {

        if (type=="pow.exp"){
          // Negative log likelihood
          optimPExp fun(dist1,yy1,y1,media1,sigma2,kappa);
          Roptim<optimPExp> opt("L-BFGS-B");
          opt.set_lower(lower2);
          opt.set_upper(upper2);
          opt.control.trace = 0;
          arma::vec x = rhoG;
          opt.minimize(fun, x);
          optR = opt.par();
        } else {

          if (type=="matern"){
            // Negative log likelihood
            optimMat fun(dist1,yy1,y1,media1,sigma2,kappa);
            Roptim<optimMat> opt("L-BFGS-B");
            opt.set_lower(lower2);
            opt.set_upper(upper2);
            opt.control.trace = 0;
            arma::vec x = rhoG;
            opt.minimize(fun, x);
            optR = opt.par();
          }
        }
      }
    }
    return optR;
  }

// Compute the distance matrix
// ------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
arma::mat crossdist(arma::mat m1){
  arma::uword nrow1 = m1.n_rows;
  arma::mat out(nrow1, nrow1, fill::zeros);

  for (uword r1 = 0; r1<nrow1; r1++) {
    for (uword r2 = (r1+1); r2<nrow1; r2++) {
      out(r1,r2) = sqrt(pow(m1(r1,0)-m1(r2,0),2.0) + pow(m1(r1,1)-m1(r2,1),2.0));
      out(r2,r1) = out(r1,r2);
    }
  }
  return out;
}

// Compute Spatial correlation matrix
// ------------------------------------------------------------------------------------------------------------
arma::mat CorrSpatial(arma::mat dist, double phi, double kappa, String type){
  arma::uword p = dist.n_rows;
  arma::mat R(p, p, fill::zeros);

  if (type=="exponential"){
    R = exp(-dist/phi);
  } else {
    if (type=="gaussian"){
      R = exp(-pow(dist/phi,2.0));
    } else {
      if (type=="pow.exp"){
        R = exp(-pow(dist/phi,kappa));
      } else {
        if (type == "matern"){
          arma::mat Abessel = dist/phi;
          Abessel.replace(0,1);
          R = (1.0/(pow(2.0,kappa-1.0)*tgamma(kappa)))*pow(dist/phi,kappa)%BesselK(Abessel,kappa);
          R.diag().ones();
        }
      }
    }
  }
  return R;
}

// Compute Spatial variance-covariance matrix
// ------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
List varianceMat(double phi, double tau2, double sigma2, double kappa, arma::mat dist, String type){
  arma::mat Iden = eye(dist.n_rows, dist.n_rows);
  arma::mat R = CorrSpatial(dist, phi, kappa, type);
  arma::mat Psi = (tau2/sigma2)*Iden + R;
  Psi = 0.50*(Psi + Psi.t());
  List output;
  output["Inv"] = Psi.i();
  output["Sigma"] = sigma2*Psi;
  return output;
}

// First and second derivative of spatial correlation matrix
// ------------------------------------------------------------------------------------------------------------
List DevCorMatrix(arma::mat H, double phi, double kappa, String type){
  arma::uword n = H.n_rows;
  arma::mat H1(n,n); H1.zeros();
  arma::mat H2(n,n); H2.zeros();

  if (type=="exponential"){
    H1 = (abs(H)/pow(phi,2.0))%exp(-(abs(H)/phi));
    H2 = abs(H)%(abs(H)-2.0*phi)%exp(-(abs(H)/phi))/pow(phi,4.0);
  }
  if (type=="gaussian"){
    H1 = (2.0*pow(abs(H),2.0)/pow(phi,3.0))%exp(-pow(abs(H)/phi,2.0));
    H2 = pow(abs(H),2.0)%(4.0*pow(abs(H),2.0) - 6.0*pow(phi,2.0))%exp(-pow(abs(H)/phi,2.0))/pow(phi,6.0);
  }
  if (type=="matern"){
    arma::mat Ak(n,n); arma::mat Bk(n,n);
    H.replace(0,1);
    Ak = BesselK(abs(H)/phi,(kappa-1.0)) + BesselK(abs(H)/phi,(kappa+1));
    Bk = BesselK(abs(H)/phi,(kappa-2.0)) + 2.0*BesselK(abs(H)/phi,kappa) + BesselK(abs(H)/phi,(kappa+2.0));
    H1 = (-1.0/(pow(2.0,kappa)*pow(phi,2.0)*tgamma(kappa)))*pow(abs(H)/phi,kappa)%(2.0*kappa*phi*BesselK(abs(H)/phi,kappa) - abs(H)%Ak);
    H2 = pow(abs(H),kappa)/(pow(2.0,(kappa+1.0))*tgamma(kappa)*pow(phi,(kappa+4.0)))%(4.0*kappa*(kappa+1.0)*pow(phi,2.0)*BesselK(abs(H)/phi,kappa) - 4.0*(kappa+1.0)*phi*abs(H)%Ak + pow(abs(H),2.0)%Bk);
  }
  if (type=="pow.exp"){
    H1 = (kappa/phi)*pow(abs(H)/phi,(kappa))%exp(-pow(abs(H)/phi,(kappa)));
    H2 = H1%(kappa*pow(abs(H),kappa)/pow(phi,(kappa+1.0)) - (kappa+1.0)/phi);
  }
  H1 = H1 - diagmat(H1);   // First derivative correlation matrix
  H2 = H2 - diagmat(H2);   // Second derivative correlation matrix
  List derSpatial;
  derSpatial["dev1"] = H1;
  derSpatial["dev2"] = H2;

  return derSpatial;
}

// Information matrix approximation for non-censored data
// --------------------------------------------------------------------------------------------------------
arma::mat Information(double sigma2, double phi, arma::mat X, arma::vec y, arma::mat PsiInv, arma::vec mean1,
                      arma::mat distM, double kappa, String typeS){
  arma::uword q = X.n_cols;
  arma::mat R = CorrSpatial(distM, phi, kappa, typeS);
  List deriv  = DevCorMatrix(distM, phi, kappa, typeS);
  arma::mat dev1 = as<arma::mat>(deriv["dev1"]);
  arma::mat dev2 = as<arma::mat>(deriv["dev2"]);
  arma::vec diff1 = y - mean1;
  arma::mat SigmaInv = PsiInv/sigma2;
  arma::mat E0 = SigmaInv*SigmaInv;
  arma::mat E1 = SigmaInv*R*SigmaInv;
  arma::mat E2 = SigmaInv*dev1*SigmaInv;

  arma::mat Hessian(q+3,q+3,fill::zeros);
  Hessian.submat(0,0,q-1,q-1) = X.t()*SigmaInv*X;
  Hessian.submat(0,q,q-1,q) = X.t()*E1*diff1;
  Hessian.submat(q,0,q,q-1) = (Hessian.submat(0,q,q-1,q)).t();
  Hessian.submat(0,q+1,q-1,q+1) = sigma2*X.t()*E2*diff1;
  Hessian.submat(q+1,0,q+1,q-1) = (Hessian.submat(0,q+1,q-1,q+1)).t();
  Hessian.submat(0,q+2,q-1,q+2) = X.t()*E0*diff1;
  Hessian.submat(q+2,0,q+2,q-1) = (Hessian.submat(0,q+2,q-1,q+2)).t();
  Hessian(q,q) = 0.50*(trace(-E1*R) + 2.0*as_scalar(diff1.t()*(E1*R*SigmaInv)*diff1));
  Hessian(q,q+1) = Hessian(q+1,q) = 0.50*(trace(SigmaInv*dev1 - sigma2*E2*R) - as_scalar(diff1.t()*(E2 - sigma2*E2*R*SigmaInv - sigma2*SigmaInv*R*E2)*diff1));
  Hessian(q,q+2) = Hessian(q+2,q) = 0.50*(trace(-E0*R) + as_scalar(diff1.t()*(SigmaInv*E1 + E1*SigmaInv)*diff1));
  Hessian(q+1,q+1) = (0.50*sigma2)*(trace(SigmaInv*dev2 - sigma2*E2*dev1) - as_scalar(diff1.t()*((SigmaInv*dev2 - 2.0*sigma2*E2*dev1)*SigmaInv)*diff1));
  Hessian(q+1,q+2) = Hessian(q+2,q+1) = (0.50*sigma2)*(trace(-E2) + as_scalar(diff1.t()*(SigmaInv*E2 + E2*SigmaInv)*diff1));
  Hessian(q+2,q+2) = 0.50*(trace(-E0) + 2.0*as_scalar(diff1.t()*E0*SigmaInv*diff1));

  return Hessian;
}

// Information matrix for the EM - MCEM algorithm for censored data
// ------------------------------------------------------------------------------------------------------------
arma::mat rtnormal(int n, arma::vec mu, arma::mat Sigma, arma::vec a, arma::vec b, int burn, int lag){
  int m = lag*n + burn;
  int p = Sigma.n_cols;
  arma::vec s = sqrt(Sigma.diag());
  arma::mat R = Sigma%(1.0/(s * s.t()));
  arma::mat Rinv = R.i();
  arma::mat X(n, p, fill::zeros);

  Rcpp::NumericVector l1 = Rcpp::wrap((a - mu)/s);
  Rcpp::NumericVector u1 = Rcpp::wrap((b - mu)/s);
  arma::vec pa = Rcpp::pnorm(l1,0,1,1,0);
  arma::vec pb = Rcpp::pnorm(u1,0,1,1,0);
  arma::vec x0 = randu<arma::vec>(p);
  Rcpp::NumericVector x1 = Rcpp::wrap(pa + (pb - pa)%x0);
  arma::colvec x = Rcpp::qnorm(x1,0,1,1,0);
  arma::vec lower = as<arma::vec>(l1);
  arma::vec upper = as<arma::vec>(u1);

  arma::uvec q1 = find_nonfinite(x);
  x.elem(q1) = lower.elem(q1);
  q1 = find_nonfinite(x);
  x.elem(q1) = upper.elem(q1);

  umat minusj(p-1, p, fill::zeros);
  for(int j=0; j<p; j++){
    int k=0;
    for(int l=0; l<p; l++){
      if(l!=j){
        minusj(k,j) = l;
        k++;
      }
    }
  }
  double delta, kap, mj, tj, lv, rv, xij;
  arma::uvec pj; arma::rowvec a1; arma::vec xj;
  int count = 1;
  for(int i=0; i<m; i++){
    delta = as_scalar(x.t()*Rinv*x);
    kap = -2.0*log(arma::randu<double>()) + delta;
    for(int j=0; j<p; j++){
      pj = minusj.col(j);
      xj = x(pj);
      a1 = xj.t()*Rinv.rows(pj);
      mj = -a1(j)/Rinv(j,j);
      tj = sqrt(mj*mj + (kap-as_scalar(a1.cols(pj)*xj))/Rinv(j,j));
      lv = std::max(lower(j),(mj-tj));
      rv = std::min(upper(j),(mj+tj));
      xij = lv + (rv - lv)*arma::randu<double>();
      x(j) = xij;
    }
    if (i==(burn + count*lag - 1)){
      X.row(count-1) = x.t();
      count++;
    }
  }
  X = X.t();
  X = X.each_col()%s;
  X = (X.each_col() + mu).t();
  X.replace(arma::datum::inf,arma::datum::nan);
  X.replace(-arma::datum::inf,arma::datum::nan);
  return X;
}

arma::mat InformationEM(arma::vec beta, double sigma2, double phi, double tau2, arma::mat X, arma::mat y, String typeS,
                        double kappa, arma::uvec ind0, arma::uvec ind1, arma::mat distM, arma::vec lower, arma::vec upper){
  arma::uword q = X.n_cols;
  int n = 10000;
  arma::mat R = CorrSpatial(distM, phi, kappa, typeS);
  List VarMat = varianceMat(phi, tau2, sigma2, kappa, distM, typeS);
  arma::mat Sigma = as<arma::mat>(VarMat["Sigma"]);
  arma::mat SigmaInv = as<arma::mat>(VarMat["Inv"])/sigma2;
  List deriv = DevCorMatrix(distM, phi, kappa, typeS);
  arma::mat dev1 = as<arma::mat>(deriv["dev1"]);
  arma::mat dev2 = as<arma::mat>(deriv["dev2"]);
  arma::vec media = X*beta;

  arma::mat invS00 = (Sigma(ind0,ind0)).i();
  arma::vec mu21 = media(ind1) + Sigma(ind1,ind0)*invS00*(y(ind0) - media(ind0));
  arma::mat Sigma21 = Sigma(ind1,ind1) - Sigma(ind1,ind0)*invS00*Sigma(ind0,ind1);
  Sigma21 = 0.50*(Sigma21 + Sigma21.t());
  arma::mat samplesY = rtnormal(n, mu21, Sigma21, lower(ind1), upper(ind1), 0, 1);
  arma::vec mean0 = (mean(samplesY,0)).t();
  arma::vec EY = y;
  arma::mat EYY = y*y.t();
  EY(ind1) = mean0;
  EYY(ind0,ind1) = y(ind0)*(mean0).t();
  EYY(ind1,ind0) = (EYY(ind0,ind1)).t();
  EYY(ind1,ind1) = cov(samplesY) + mean0*mean0.t();
  EYY = 0.50*(EYY + EYY.t());

  arma::vec diff1 = EY - media;
  arma::vec diff2 = 2.0*EY - media;
  arma::mat E0 = SigmaInv*SigmaInv;
  arma::mat E1 = SigmaInv*R*SigmaInv;
  arma::mat E2 = SigmaInv*dev1*SigmaInv;
  arma::mat d2Q(q+3,q+3,fill::zeros);

  // Compute Q''(theta)
  d2Q.submat(0,0,q-1,q-1) = -X.t()*SigmaInv*X;
  d2Q.submat(0,q,q-1,q) = -X.t()*E1*diff1;
  d2Q.submat(q,0,q,q-1) = (d2Q.submat(0,q,q-1,q)).t();
  d2Q.submat(0,q+1,q-1,q+1) = -sigma2*X.t()*E2*diff1;
  d2Q.submat(q+1,0,q+1,q-1) = (d2Q.submat(0,q+1,q-1,q+1)).t();
  d2Q.submat(0,q+2,q-1,q+2) = -X.t()*E0*diff1;
  d2Q.submat(q+2,0,q+2,q-1) = (d2Q.submat(0,q+2,q-1,q+2)).t();
  d2Q(q,q) = -0.50*(trace(-E1*R) + 2.0*trace(EYY*E1*R*SigmaInv) - 2.0*as_scalar(media.t()*E1*R*SigmaInv*diff2));
  d2Q(q,q+1) = d2Q(q+1,q) = -0.50*(trace(SigmaInv*dev1 - sigma2*E2*R) - trace(EYY*(E2 - 2.0*sigma2*E2*R*SigmaInv)) + as_scalar(media.t()*(E2 - sigma2*E2*R*SigmaInv - sigma2*SigmaInv*R*E2)*diff2));
  d2Q(q,q+2) = d2Q(q+2,q) = -0.50*(trace(-E1) + 2.0*trace(EYY*E1*SigmaInv) - as_scalar(media.t()*(SigmaInv*E1 + E1*SigmaInv)*diff2));
  d2Q(q+1,q+1) = (-0.50*sigma2)*(trace(SigmaInv*dev2 - sigma2*E2*dev1) - trace(EYY*(SigmaInv*dev2 - 2.0*sigma2*E2*dev1)*SigmaInv) + as_scalar(media.t()*(SigmaInv*dev2 - 2.0*sigma2*E2*dev1)*SigmaInv*diff2));
  d2Q(q+1,q+2) = d2Q(q+2,q+1) = (-0.50*sigma2)*(trace(-E2) + 2.0*trace(EYY*E2*SigmaInv) - as_scalar(media.t()*(SigmaInv*E2 + E2*SigmaInv)*diff2));
  d2Q(q+2,q+2) = -0.50*(trace(-E0) + 2.0*trace(EYY*E0*SigmaInv) - 2.0*as_scalar(media.t()*E0*SigmaInv*diff2));
  // Compute Q'(theta)
  arma::vec d1Q(q+3,fill::zeros);
  d1Q.subvec(0,q-1) = X.t()*SigmaInv*diff1;
  d1Q(q) = -0.50*(trace(SigmaInv*R) - trace(EYY*E1) + as_scalar(media.t()*E1*diff2));
  d1Q(q+1) = (-0.50*sigma2)*(trace(SigmaInv*dev1) - trace(EYY*E2) + as_scalar(media.t()*E2*diff2));
  d1Q(q+2) = -0.50*(trace(SigmaInv) - trace(EYY*E0) + as_scalar(media.t()*E0*diff2));
  // Compute E(S*St|Yo)
  arma::vec S1(q+3,fill::zeros);
  arma::mat ESS(q+3,q+3,fill::zeros);
  arma::vec yb = y;
  arma::vec auxMean(y.size(),fill::zeros);
  for (int i=0; i<n; i++){
    yb(ind1) = (samplesY.row(i)).t();
    auxMean = yb - media;
    S1.subvec(0,q-1) = X.t()*SigmaInv*auxMean;
    S1(q) = -0.50*(trace(SigmaInv*R) - as_scalar(auxMean.t()*E1*auxMean));
    S1(q+1) = -0.50*sigma2*(trace(SigmaInv*dev1) - as_scalar(auxMean.t()*E2*auxMean));
    S1(q+2) = -0.50*(trace(SigmaInv) - as_scalar(auxMean.t()*E0*auxMean));
    ESS = ESS + S1*S1.t();
  }
  arma::mat IF = d1Q*d1Q.t() - d2Q - ESS/(1.0*n);
  return IF;
}

// Estimate parameters in Gaussian spatial model with non-censoring
// ------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
List Spatial_model(arma::vec y, arma::mat X, arma::mat coords, double init_phi, double init_tau, arma::vec lowerp,
                   arma::vec upperp, String type, double kappa, arma::uword Maxiter, double tol, bool infM){
  Progress time(Maxiter, true);
  uword p = y.n_elem;
  uword q = X.n_cols;
  arma::uvec indexs = arma::regspace<arma::uvec>(0,1,q-1);
  arma::vec theta(q+3, fill::zeros);
  arma::vec theta1(q+3, fill::zeros);
  // Initial values
  arma::mat EYY = y*y.t();
  arma::vec beta = ((X.t()*X).i())*X.t()*y;
  theta(indexs) = beta;
  arma::vec media = X*beta;
  double sigma2 = as_scalar(sum(pow(y-media,2.0)))/p;
  theta(q)   = sigma2;
  double phi = init_phi;
  theta(q+1) = phi;
  double tau2 = init_tau;
  theta(q+2) = tau2;
  arma::vec optP(2,fill::zeros); optP(0) = phi; optP(1) = tau2;
  arma::mat Theta = theta.t();
  arma::mat distanceM = crossdist(coords);
  List VarMat = varianceMat(phi, tau2, sigma2, kappa, distanceM, type);
  arma::mat PsiInv = VarMat["Inv"];
  // Stopping criteria
  double criterio = 10.0;
  arma::uword count = 0;

  // ALGORITHM ----------------------------------------------------------------
  while (criterio>tol) {
    time.increment();
    count += 1;
    // M-step
    beta = (X.t()*PsiInv*X).i()*X.t()*PsiInv*y;
    media = X*beta;
    sigma2 = as_scalar((y-media).t()*PsiInv*(y-media))/p;
    optP = optimlL(optP, distanceM, EYY, y, media, sigma2, lowerp, upperp, type, kappa);
    phi = optP(0);
    tau2 = optP(1);
    VarMat = varianceMat(phi, tau2, sigma2, kappa, distanceM, type);
    PsiInv = as<arma::mat>(VarMat["Inv"]);
    theta1(indexs) = beta; theta1(q) = sigma2; theta1(q+1) = phi; theta1(q+2) = tau2;
    criterio = as_scalar(sqrt(sum((theta1/theta-1.0)%(theta1/theta-1.0))));
    if (count==Maxiter){ criterio = 1e-12; }
    theta = theta1;
    Theta = join_vert(Theta, theta.t());
  }
  List output;
  output["Theta"] = Theta; output["theta"] = theta; output["beta"] = beta; output["sigma2"] = sigma2;
  output["phi"] = phi; output["tau2"] = tau2; output["EY"] = y; output["EYY"] = EYY;

  if (infM){
    arma::mat IF = Information(sigma2, phi, X, y, PsiInv, media, distanceM, kappa, type);
    IF = 0.50*(IF + IF.t());
    arma::mat invIF = IF.i();
    output["SE"] = sqrt(invIF.diag());
    output["InfMat"] = IF;
  }
  output["Iterations"] = count;
  return output;
}

// MCEM - Estimate parameters in Gaussian spatial model with censored data
// ------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
List MCEMspatial(arma::vec y, arma::mat X, arma::vec cc, arma::vec lower, arma::vec upper, arma::mat coords,
                 double init_phi, double init_tau, arma::vec lowerp, arma::vec upperp, String type,
                 double kappa, arma::uword Maxiter, arma::uword nMin, arma::uword nMax, double tol, bool infM){
  Environment pkg = Environment::namespace_env("relliptical");
  Function Nmoment = pkg["mvtelliptical"];

  Progress time(Maxiter,true);
  uword p = y.size();
  uword q = X.n_cols;
  arma::uvec indexs = arma::regspace<arma::uvec>(0,1,q-1);
  arma::uvec ind0 = find(cc==0);  // Index of uncensored observations
  arma::uvec ind1 = find(cc==1);  // Index of censored observations
  arma::uword p0 = ind0.size();   // Number of uncensored observations
  arma::uword p1 = ind1.size();   // Number of censored observations
  arma::vec mu21(p1, fill::zeros);          // Conditional mean
  arma::mat Sigma21(p1, p1, fill::zeros);    // Conditional variance matrix
  arma::vec lower1 = lower(ind1);          // Lower bound of censored observations
  arma::vec upper1 = upper(ind1);          // Upper bound of censored observations
  arma::mat invSigma(p0, p0, fill::zeros);
  arma::vec beta(q);
  arma::vec media(p);
  double sigma2;
  if (y.has_nan()){
    arma::uvec indFin = find_finite(y);
    beta = (((X.rows(indFin)).t()*X.rows(indFin)).i())*(X.rows(indFin)).t()*y(indFin);
    media = X*beta;
    sigma2 = as_scalar(sum(pow(y(indFin)-media(indFin),2.0)))/(indFin.size());
  } else {
    beta = ((X.t()*X).i())*X.t()*y;
    media = X*beta;
    sigma2 = as_scalar(sum(pow(y-media,2.0)))/p;
  }
  arma::vec EY = y;          // Estimate of first conditional moment
  arma::mat EYY = y*y.t();   // Estimate of second conditional moment
  double phi = init_phi;
  double tau2 = init_tau;
  arma::mat distanceM = crossdist(coords);  // Distance matrix
  List VarMat = varianceMat(phi, tau2, sigma2, kappa, distanceM, type);
  arma::mat PsiInv = VarMat["Inv"];        // Inverse of Psi=(tau2/sigma2)*I + R
  arma::mat Sigma = VarMat["Sigma"];       // Variance matrix sigma2*Psi
  arma::vec theta(q+3, fill::zeros);
  arma::vec theta1(q+3, fill::zeros);
  theta(indexs) = beta; theta(q) = sigma2; theta(q+1) = phi; theta(q+2) = tau2;
  List moments; arma::vec optP(2, fill::zeros);
  optP(0) = phi; optP(1) = tau2;
  arma::mat Theta = theta.t();
  // Stopping criteria
  double criterio = 10.0;
  arma::uword count = 0;

  // MCEM ALGORITHM -----------------------------------------------------------
  arma::vec ss = arma::round(arma::linspace(nMin, nMax, Maxiter));
  int n1;
  while (criterio>tol) {
    time.increment();
    n1 = ss(count);
    count += 1;
    // MC E-step
    invSigma = (Sigma(ind0,ind0)).i();
    mu21 = media(ind1) + Sigma(ind1,ind0)*invSigma*(y(ind0) - media(ind0));
    Sigma21 = Sigma(ind1,ind1) - Sigma(ind1,ind0)*invSigma*Sigma(ind0,ind1);
    Sigma21 = 0.50*(Sigma21 + Sigma21.t());
    moments = Nmoment(lower1, upper1, mu21, Sigma21, "Normal", R_NilValue, n1, 0, 1);
    EY(ind1) = as<arma::vec>(moments["EY"]);
    EYY(ind1,ind1) = as<arma::mat>(moments["EYY"]);
    EYY(ind1,ind0) = as<arma::vec>(moments["EY"])*(y(ind0)).t();
    EYY(ind0,ind1) = (EYY(ind1,ind0)).t();
    EYY = 0.50*(EYY + EYY.t());
    // M-step
    beta = (X.t()*PsiInv*X).i()*X.t()*PsiInv*EY;
    media = X*beta;
    sigma2 = (trace(EYY*PsiInv) - as_scalar(EY.t()*PsiInv*media + media.t()*PsiInv*EY - media.t()*PsiInv*media))/p;
    optP = optimlL(optP,distanceM,EYY,EY,media,sigma2,lowerp,upperp,type,kappa);
    phi = optP(0);
    tau2 = optP(1);
    VarMat = varianceMat(phi, tau2, sigma2, kappa, distanceM, type);
    PsiInv = as<arma::mat>(VarMat["Inv"]);
    Sigma = as<arma::mat>(VarMat["Sigma"]);
    theta1(indexs) = beta; theta1(q) = sigma2; theta1(q+1) = phi; theta1(q+2) = tau2;
    criterio = as_scalar(sqrt(sum((theta1/theta-1.0)%(theta1/theta-1.0))));
    if (count==Maxiter){ criterio = 1e-12; }
    theta = theta1;
    Theta = join_vert(Theta, theta.t());
  }
  arma::uword n2 = Theta.n_rows;
  if (n2 > 5){
    arma::uvec ind3 = arma::regspace<arma::uvec>(round(0.5*n2)+1,3,n2-1);
    theta = (mean(Theta.rows(ind3),0)).t();
  } else {
    theta = (mean(Theta,0)).t();
  }
  beta = theta(indexs); sigma2 = theta(q); phi = theta(q+1); tau2 = theta(q+2);
  List output;
  output["Theta"] = Theta; output["theta"] = theta; output["beta"] = beta; output["sigma2"] = sigma2;
  output["phi"] = phi; output["tau2"] = tau2; output["EY"] = EY; output["EYY"] = EYY;

  if (infM){
    arma::mat IF = InformationEM(beta, sigma2, phi, tau2, X, y, type, kappa, ind0, ind1, distanceM, lower, upper);
    IF = 0.50*(IF + IF.t());
    arma::mat invIF = IF.i();
    output["SE"] = sqrt(invIF.diag());
    output["InfMat"] = IF;
  }
  output["Iterations"] = count;
  return output;
}

// EM - Estimate parameters in Gaussian spatial model
// ------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
List EMspatial(arma::vec y, arma::mat X, arma::vec cc, arma::vec lower, arma::vec upper, arma::mat coords,
               double init_phi, double init_tau, arma::vec lowerp, arma::vec upperp, String type,
               double kappa, arma::uword Maxiter, double tol, bool infM){
  Environment pkg = Environment::namespace_env("MomTrunc");
  Function mvTnorm = pkg["meanvarTMD"];

  Progress time(Maxiter, true);
  arma::uword p = y.size();
  arma::uword q = X.n_cols;
  arma::uvec indexs = arma::regspace<arma::uvec>(0,1,q-1);
  arma::uvec ind0 = find(cc==0);  // Index of uncensored observations
  arma::uvec ind1 = find(cc==1);  // Index of censored observations
  arma::uword p0 = ind0.size();   // Number of uncensored observations
  arma::uword p1 = ind1.size();   // Number of censored observations
  arma::vec mu21(p1, fill::zeros);          // Conditional mean
  arma::mat Sigma21(p1, p1, fill::zeros);    // Conditional variance matrix
  arma::vec lower1 = lower(ind1);          // Lower bound of censored observations
  arma::vec upper1 = upper(ind1);          // Upper bound of censored observations
  arma::mat invSigma(p0, p0, fill::zeros);
  arma::mat distanceM = crossdist(coords);  // Distance matrix
  arma::vec beta(q); arma::vec media(p); double sigma2;
  if (y.has_nan()){
    arma::uvec indFin = find_finite(y);
    beta = (((X.rows(indFin)).t()*X.rows(indFin)).i())*(X.rows(indFin)).t()*y(indFin);
    media = X*beta;
    sigma2 = as_scalar(sum(pow(y(indFin) - media(indFin),2.0)))/(indFin.size());
  } else {
    beta = ((X.t()*X).i())*X.t()*y;
    media = X*beta;
    sigma2 = as_scalar(sum(pow(y - media,2.0)))/p;
  }
  double phi = init_phi;
  double tau2 = init_tau;
  List VarMat = varianceMat(phi, tau2, sigma2, kappa, distanceM, type);
  arma::mat PsiInv = VarMat["Inv"];        // Inverse of Psi=(tau2/sigma2)*I + R
  arma::mat Sigma = VarMat["Sigma"];       // Variance matrix sigma2*Psi
  arma::vec theta(q+3,fill::zeros);
  arma::vec theta1(q+3,fill::zeros);
  theta(indexs) = beta; theta(q) = sigma2; theta(q+1) = phi; theta(q+2) = tau2;
  arma::vec optP(2,fill::zeros);
  optP(0) = phi; optP(1) = tau2;
  arma::mat Theta = theta.t();
  // Stopping criteria
  double criterio = 10.0;
  arma::uword count = 0;

  // EM ALGORITHM -------------------------------------------------------------
  List moments;
  arma::vec EY = y;          // Estimate of first conditional moment
  arma::mat EYY = y*y.t();   // Estimate of second conditional moment
  while (criterio>tol) {
    time.increment();
    count += 1;
    // E-step
    invSigma = (Sigma(ind0,ind0)).i();
    mu21 = media(ind1) + Sigma(ind1,ind0)*invSigma*(y(ind0) - media(ind0));
    Sigma21 = Sigma(ind1,ind1) - Sigma(ind1,ind0)*invSigma*Sigma(ind0,ind1);
    moments = mvTnorm(lower1, upper1, mu21, Sigma21, R_NilValue, R_NilValue, R_NilValue, R_NilValue, "normal");
    EY(ind1) = as<arma::vec>(moments["mean"]);
    EYY(ind1,ind1) = as<arma::mat>(moments["EYY"]);
    EYY(ind1,ind0) = as<arma::vec>(moments["mean"])*(y(ind0)).t();
    EYY(ind0,ind1) = (EYY(ind1,ind0)).t();
    EYY = 0.50*(EYY + EYY.t());
    // M-step
    beta = (X.t()*PsiInv*X).i()*X.t()*PsiInv*EY;
    media = X*beta;
    sigma2 = (trace(EYY*PsiInv) - as_scalar(EY.t()*PsiInv*media + media.t()*PsiInv*EY - media.t()*PsiInv*media))/p;
    optP = optimlL(optP, distanceM, EYY, EY, media, sigma2, lowerp, upperp, type, kappa);
    phi = optP(0);
    tau2 = optP(1);
    VarMat = varianceMat(phi, tau2, sigma2, kappa, distanceM, type);
    PsiInv = as<arma::mat>(VarMat["Inv"]);
    Sigma = as<arma::mat>(VarMat["Sigma"]);
    theta1(indexs) = beta; theta1(q) = sigma2; theta1(q+1) = phi; theta1(q+2) = tau2;
    criterio = as_scalar(sqrt(sum((theta1/theta-1.0)%(theta1/theta-1.0))));
    if (count==Maxiter){ criterio = 1e-12; }
    theta = theta1;
    Theta = join_vert(Theta,theta.t());
  }
  List output;
  output["Theta"] = Theta; output["theta"] = theta; output["beta"] = beta; output["sigma2"] = sigma2;
  output["phi"] = phi; output["tau2"] = tau2; output["EY"] = EY; output["EYY"] = EYY;

  if (infM){
    arma::mat IF = InformationEM(beta, sigma2, phi, tau2, X, y, type, kappa, ind0, ind1, distanceM, lower, upper);
    IF = 0.50*(IF + IF.t());
    arma::mat invIF = IF.i();
    output["SE"] = sqrt(invIF.diag());
    output["InfMat"] = IF;
  }
  output["Iterations"] = count;
  return output;
}

// SAEM - Estimate parameters in Gaussian spatial model
// ------------------------------------------------------------------------------------------------------------
// [[Rcpp::export]]
List SAEMspatial(arma::vec y, arma::mat X, arma::vec cc, arma::vec lower, arma::vec upper, arma::mat coords,
                 double init_phi, double init_tau, arma::vec lowerp, arma::vec upperp, String type,
                 double kappa, arma::uword Maxiter, double pc, arma::uword m, double tol, bool infM){
  Progress time(Maxiter, true);
  arma::uword p = y.size();
  arma::uword q = X.n_cols;
  arma::uvec indexs = arma::regspace<arma::uvec>(0,1,q-1);
  arma::uvec ind0 = find(cc==0);  // Index of uncensored observations
  arma::uvec ind1 = find(cc==1);  // Index of censored observations
  arma::uword p0 = ind0.size();   // Number of uncensored observations
  arma::uword p1 = ind1.size();   // Number of censored observations
  arma::vec mu21(p1, fill::zeros);          // Conditional mean
  arma::mat Sigma21(p1, p1, fill::zeros);    // Conditional variance matrix
  arma::vec lower1 = lower(ind1);          // Lower bound of censored observations
  arma::vec upper1 = upper(ind1);          // Upper bound of censored observations
  arma::mat invSigma(p0, p0, fill::zeros);
  arma::vec beta(q); arma::vec media(p); double sigma2;
  if (y.has_nan()){
    arma::uvec indFin = find_finite(y);
    beta = (((X.rows(indFin)).t()*X.rows(indFin)).i())*(X.rows(indFin)).t()*y(indFin);
    media = X*beta;
    sigma2 = as_scalar(sum(pow(y(indFin)-media(indFin),2.0)))/(indFin.size());
  } else {
    beta = ((X.t()*X).i())*X.t()*y;
    media = X*beta;
    sigma2 = as_scalar(sum(pow(y-media,2.0)))/p;
  }
  double phi = init_phi;
  double tau2 = init_tau;
  arma::mat distanceM = crossdist(coords);  // Distance matrix
  List VarMat = varianceMat(phi, tau2, sigma2, kappa, distanceM, type);
  arma::mat PsiInv = VarMat["Inv"];        // Inverse of Psi=(tau2/sigma2)*I + R
  arma::mat Sigma = VarMat["Sigma"];       // Variance matrix sigma2*Psi
  // Information matrix
  List deriv = DevCorMatrix(distanceM, phi, kappa, type);
  arma::mat dev1 = as<arma::mat>(deriv["dev1"]);
  arma::mat R = CorrSpatial(distanceM, phi, kappa, type);
  arma::mat ESS(q+3, q+3, fill::zeros);
  arma::vec score(q+3, fill::zeros);
  arma::vec auxMean(p, fill::zeros);
  arma::mat Hessian(q+3, q+3, fill::zeros);
  // Estimates
  arma::vec theta(q+3, fill::zeros);
  arma::vec theta1(q+3, fill::zeros);
  theta(indexs) = beta; theta(q) = sigma2; theta(q+1) = phi; theta(q+2) = tau2;
  arma::vec optP(2,fill::zeros);
  optP(0) = phi; optP(1) = tau2;
  arma::mat Theta = theta.t();
  // Stopping criteria
  double criterio = 10.0;
  arma::uword count = 0;

  // SAEM ALGORITHM ------------------------------------------------------------
  arma::mat gibbs(1, p1, fill::zeros);
  arma::vec SAEMY(p, fill::zeros);          // Estimate of first conditional moment
  arma::mat SAEMYY(p, p, fill::zeros);       // Estimate of second conditional moment
  arma::vec EY(p, fill::zeros);
  arma::vec auxY = y;
  arma::mat EYY(p, p, fill::zeros);
  arma::vec delta(Maxiter, fill::ones);
  if (pc<1){
    arma::vec nonM = 1.0/arma::regspace<arma::vec>(1,1,Maxiter*(1-pc));
    arma::uvec nonI = arma::regspace<arma::uvec>(Maxiter-nonM.n_elem,1,Maxiter-1);
    delta(nonI) = nonM;
  }
  while (criterio>tol) {
    time.increment();
    count += 1;
    // SA E-step
    invSigma = (Sigma(ind0,ind0)).i();
    mu21 = media(ind1) + Sigma(ind1,ind0)*invSigma*(y(ind0) - media(ind0));
    Sigma21 = Sigma(ind1,ind1) - Sigma(ind1,ind0)*invSigma*Sigma(ind0,ind1);
    Sigma21 = 0.50*(Sigma21 + Sigma21.t());
    for (uword i=0; i<m; i++){
      gibbs = rtnormal(1, mu21, Sigma21, lower1, upper1, 3, 1);
      auxY(ind1) = (gibbs).t();
      EY = EY + auxY;
      EYY = EYY + auxY*auxY.t();

      if (infM){
        auxMean = auxY - media;
        score(indexs) = (X.t()*PsiInv*auxMean)/sigma2;
        score(q) = -0.50*(trace(PsiInv*R)/sigma2 - as_scalar(auxMean.t()*PsiInv*R*PsiInv*auxMean)/(sigma2*sigma2));
        score(q+1) = -0.50*(trace(PsiInv*dev1) - as_scalar(auxMean.t()*PsiInv*dev1*PsiInv*auxMean)/sigma2);
        score(q+2) = -0.50*(trace(PsiInv)/sigma2 - as_scalar(auxMean.t()*PsiInv*PsiInv*auxMean)/(sigma2*sigma2));
        ESS = ESS + score*score.t();
      }
    }
    SAEMY = SAEMY + delta(count-1)*(EY/m - SAEMY);
    SAEMYY = SAEMYY + delta(count-1)*(EYY/m - SAEMYY);
    SAEMYY = 0.50*(SAEMYY + SAEMYY.t());
    if (infM){  Hessian = Hessian + delta(count-1)*(ESS/m - Hessian);  }
    // M-step
    beta = (X.t()*PsiInv*X).i()*X.t()*PsiInv*SAEMY;
    media = X*beta;
    sigma2 = (trace(SAEMYY*PsiInv) - as_scalar(SAEMY.t()*PsiInv*media + media.t()*PsiInv*SAEMY - media.t()*PsiInv*media))/p;
    optP = optimlL(optP,distanceM,SAEMYY,SAEMY,media,sigma2,lowerp,upperp,type,kappa);
    phi = optP(0);
    tau2 = optP(1);
    VarMat = varianceMat(phi, tau2, sigma2, kappa, distanceM, type);
    PsiInv = as<arma::mat>(VarMat["Inv"]);
    Sigma = as<arma::mat>(VarMat["Sigma"]);
    if (infM){
      deriv = DevCorMatrix(distanceM, phi, kappa, type);
      dev1 = as<arma::mat>(deriv["dev1"]);
      R = CorrSpatial(distanceM, phi, kappa, type);
      ESS.zeros(q+3, q+3);
      score.zeros(q+3);
      auxMean.zeros(q+3);
    }
    theta1(indexs) = beta; theta1(q) = sigma2; theta1(q+1) = phi; theta1(q+2) = tau2;
    criterio = as_scalar(sqrt(sum((theta1/theta-1)%(theta1/theta-1))));
    if (count==Maxiter){ criterio = 1e-12; }
    theta = theta1;
    Theta = join_vert(Theta, theta.t());
    EY.zeros(p);
    EYY.zeros(p,p);
  }
  List output;
  output["Theta"] = Theta; output["theta"] = theta; output["beta"] = beta; output["sigma2"] = sigma2;
  output["phi"] = phi; output["tau2"] = tau2; output["EY"] = SAEMY; output["EYY"] = SAEMYY;

  if (infM){
    arma::mat SigmaInv = PsiInv/sigma2;
    arma::mat E0 = SigmaInv*SigmaInv;
    arma::mat E1 = SigmaInv*R*SigmaInv;
    arma::mat E2 = SigmaInv*dev1*SigmaInv;
    arma::mat dev2 = as<arma::mat>(deriv["dev2"]);
    auxMean = SAEMY - media;
    arma::vec diff2 = 2.0*SAEMY - media;
    // Q'(theta)
    arma::vec d1Q(q+3, fill::zeros);
    d1Q(indexs) = X.t()*SigmaInv*auxMean;
    d1Q(q) = -0.50*(trace(SigmaInv*R) - trace(SAEMYY*E1) + as_scalar(media.t()*E1*diff2));
    d1Q(q+1) = (-0.50*sigma2)*(trace(SigmaInv*dev1) - trace(SAEMYY*E2) + as_scalar(media.t()*E2*diff2));
    d1Q(q+2) = -0.50*(trace(SigmaInv) - trace(SAEMYY*E0) + as_scalar(media.t()*E0*diff2));
    // Q''(theta)
    arma::mat d2Q(q+3, q+3, fill::zeros);
    d2Q(indexs,indexs) = -X.t()*SigmaInv*X;
    d2Q.submat(0,q,q-1,q) = -X.t()*E1*auxMean;
    d2Q.submat(q,0,q,q-1) = (d2Q.submat(0,q,q-1,q)).t();
    d2Q.submat(0,q+1,q-1,q+1) = -sigma2*X.t()*E2*auxMean;
    d2Q.submat(q+1,0,q+1,q-1) = (d2Q.submat(0,q+1,q-1,q+1)).t();
    d2Q.submat(0,q+2,q-1,q+2) = -X.t()*E0*auxMean;
    d2Q.submat(q+2,0,q+2,q-1) = (d2Q.submat(0,q+2,q-1,q+2)).t();
    d2Q(q,q) = -0.50*(trace(-E1*R) + 2.0*trace(SAEMYY*E1*R*SigmaInv) - 2.0*as_scalar(media.t()*E1*R*SigmaInv*diff2));
    d2Q(q,q+1) = d2Q(q+1,q) = -0.50*(trace(SigmaInv*dev1 - sigma2*E2*R) - trace(SAEMYY*(E2 - 2.0*sigma2*E2*R*SigmaInv)) + as_scalar(media.t()*(E2 - sigma2*E2*R*SigmaInv - sigma2*SigmaInv*R*E2)*diff2));
    d2Q(q,q+2) = d2Q(q+2,q) = -0.50*(trace(-E1) + 2.0*trace(SAEMYY*E1*SigmaInv) - as_scalar(media.t()*(SigmaInv*E1 + E1*SigmaInv)*diff2));
    d2Q(q+1,q+1) = (-0.50*sigma2)*(trace(SigmaInv*dev2 - sigma2*E2*dev1) - trace(SAEMYY*(SigmaInv*dev2 - 2.0*sigma2*E2*dev1)*SigmaInv) + as_scalar(media.t()*(SigmaInv*dev2 - 2.0*sigma2*E2*dev1)*SigmaInv*diff2));
    d2Q(q+1,q+2) = d2Q(q+2,q+1) = (-0.50*sigma2)*(trace(-E2) + 2.0*trace(SAEMYY*E2*SigmaInv) - as_scalar(media.t()*(SigmaInv*E2 + E2*SigmaInv)*diff2));
    d2Q(q+2,q+2) = -0.50*(trace(-E0) + 2.0*trace(SAEMYY*E0*SigmaInv) - 2.0*as_scalar(media.t()*E0*SigmaInv*diff2));
    // Information matrix
    arma::mat IF = d1Q*d1Q.t() - d2Q - Hessian;
    arma::mat invH = IF.i();
    output["SE"] = sqrt(invH.diag());
    output["InfMat"] = 0.50*(IF + IF.t());
  }
  output["Iterations"] = count;
  return output;
}
