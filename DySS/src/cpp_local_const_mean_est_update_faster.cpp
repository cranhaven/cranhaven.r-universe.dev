#include <RcppArmadillo.h>
using namespace Rcpp;
//' @keywords internal
//' @noRd
//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
arma::vec local_const_mean_est_update_faster(
    arma::mat yyij,
    arma::imat ttij,
    arma::ivec nobs,
    arma::ivec alltimepoints,
    const int hh,
    arma::mat covariance){
  
  const int nind=ttij.n_rows;
  const int ntimepoints=alltimepoints.n_elem;
  
  const double omega=0.001;
  const double hh_double=double(hh)*omega;
  
  arma::vec mu(ntimepoints);
  
  int tt,ttdiff,ii,jj,ttidx;
  double tt_double,ttdiff_double;
  double U0,V0;
  
  arma::vec allkvalues(2*hh+1);
  arma::vec allsqrtkvalues(2*hh+1);
  arma::vec allttdiff(2*hh+1);
  for(tt=-hh;tt<=hh;tt++){
    ttdiff_double=double(tt)*omega;
    allttdiff(tt+hh)=ttdiff_double;
    allkvalues(tt+hh)=(1.0-(ttdiff_double/hh_double)*(ttdiff_double/hh_double)); //hht_double
    allsqrtkvalues(tt+hh)=std::sqrt(allkvalues(tt+hh));
  }
  
  arma::vec X0i;
  // arma::vec X1i;
  arma::vec Yi;
  arma::mat Sigmai;
  arma::mat Wi;
  arma::ivec ttdiffvec;
  arma::vec fullKisqrt;
  arma::vec Ki;
  arma::uvec idxvec;
  arma::vec ttsubvec;
  arma::vec temp;
  
  arma::field<arma::ivec> F_ttij(nind);
  arma::field<arma::uvec> F_ttij_uword(nind);
  arma::field<arma::vec> F_ttij_double(nind);
  arma::field<arma::vec> F_yyij(nind);
  arma::field<arma::mat> F_Sigmai(nind);
  for(ii=0;ii<nind;ii++){
    F_ttij(ii)=ttij.row(ii).subvec(0,nobs(ii)-1).t();
    F_ttij_double(ii)=omega*arma::conv_to<arma::vec>::from(F_ttij(ii));
    F_yyij(ii)=yyij.row(ii).subvec(0,nobs(ii)-1).t();
    idxvec.set_size(nobs(ii));
    for(jj=0;jj<nobs(ii);jj++)idxvec(jj)=ttij(ii,jj)-1;
    F_Sigmai(ii)=covariance.submat(idxvec,idxvec);
    F_ttij_uword(ii)=idxvec;
  }
  
  for(ttidx=0;ttidx<ntimepoints;++ttidx){
    tt=alltimepoints(ttidx);
    tt_double=omega*double(tt);
    U0=0.0;V0=0.0;
    for(ii=0;ii<nind;ii++){
      ttdiffvec=F_ttij(ii)-tt;
      fullKisqrt.zeros(nobs(ii));
      for(jj=0;jj<nobs(ii);jj++){
        ttdiff=ttij(ii,jj)-tt;
        if(std::abs(ttdiff)<hh){
          fullKisqrt(jj)=allsqrtkvalues(hh+ttdiff);
        }
      }
      idxvec=arma::find(arma::abs(ttdiffvec)<hh);
      // X1i=F_ttij_double(ii).elem(idxvec)-tt_double;
      X0i.ones(idxvec.n_elem);
      Yi=F_yyij(ii).elem(idxvec);
      Sigmai=F_Sigmai(ii).submat(idxvec,idxvec);
      Ki=fullKisqrt.elem(idxvec);
      Wi=arma::pinv(Sigmai,1e-10,"std");
      Wi=Wi.each_col()%Ki;
      Wi=Wi.each_row()%Ki.t();
      V0=V0+arma::dot(X0i,Wi*X0i);
      U0=U0+arma::dot(X0i,Wi*Yi);
    }
    mu(ttidx)=U0/V0;
  }
  return mu;
}
