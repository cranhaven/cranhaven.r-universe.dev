#include <RcppArmadillo.h>
using namespace Rcpp;

//' @keywords internal
//' @noRd
//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]

List chart_CUSUM_univariate_sprint(
    arma::mat epsij,
    arma::umat ttij,
    arma::uvec nobs,
    arma::mat Sigma,
    const double kk,
    const double ll){
  
  const int nind=ttij.n_rows;
  const int nmaxobs=epsij.n_cols;
  int ii,jj,sl;
  
  arma::mat CC(nind,nmaxobs);CC.fill(arma::datum::nan);
  arma::mat ee(nind,nmaxobs);ee.fill(arma::datum::nan);
  
  arma::vec ee_vec(nmaxobs);
  arma::vec ll_vec(nmaxobs);
  arma::vec sig_vec(nmaxobs);
  arma::mat UU_mat(nmaxobs,nmaxobs,arma::fill::zeros);
  arma::mat Sigmai;
  arma::uvec tttemp;
  
  double CCij;
  double dd;
  
  for(ii=0;ii<nind;ii++){
    tttemp=ttij.row(ii).cols(0,nobs(ii)-1).t()-1;
    Sigmai=Sigma(tttemp,tttemp);
    // Rcpp::Rcout<<ii<<" ";
    sl=-1;
    for(jj=0;jj<nobs(ii);jj++){
      if(sl==-1){
        UU_mat(0,0)=1.0/std::sqrt(Sigmai(jj,jj));
        ee_vec(0)=UU_mat(0,0)*epsij(ii,jj);
        CCij=std::max(0.0,ee_vec(0)-kk);
        
        ee(ii,jj)=ee_vec(0);
        CC(ii,jj)=CCij;
        if(CCij>=ll)break;
        if(CCij<=0.0){sl=-1;}else{sl=sl+1;}
      }else{
        sig_vec.subvec(0,sl)=Sigmai.submat(jj-sl-1,jj,jj-1,jj);
        ll_vec.subvec(0,sl)=UU_mat.submat(0,0,sl,sl)*sig_vec.subvec(0,sl);
        dd=std::sqrt(Sigmai(jj,jj)-arma::dot(ll_vec.subvec(0,sl),ll_vec.subvec(0,sl)));
        UU_mat.submat(sl+1,0,sl+1,sl)=(ll_vec.subvec(0,sl).t()*UU_mat.submat(0,0,sl,sl))/(-dd);
        UU_mat(sl+1,sl+1)=1.0/dd;
        ee_vec(sl+1)=(epsij(ii,jj)-arma::dot(ll_vec.subvec(0,sl),ee_vec.subvec(0,sl)))/dd;
        CCij=std::max(0.0,CCij+ee_vec(sl+1)-kk);
        
        ee(ii,jj)=ee_vec(sl+1);
        CC(ii,jj)=CCij;
        if(CCij>=ll)break;
        if(CCij<=0.0){sl=-1;}else{sl=sl+1;}
      }
    }
  }
  
  List result(2);
  result(0)=CC;
  result(1)=ee;
  
  return(result);
}
