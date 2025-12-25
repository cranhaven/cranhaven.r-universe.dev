#include <RcppArmadillo.h>
using namespace Rcpp;
//' @keywords internal
//' @noRd
//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
List mchart1_decorrelation_EWMA_multivariate_both(
    arma::vec eps_A,arma::mat varcov_A,int nobs_A,int numdim,double lambda,double limit){
  arma::vec tempe;
  arma::mat Sigmacofactor,AA,AAinv,Sigmacov,Sigmavar;
  arma::mat e_A(numdim,nobs_A);
  arma::mat S_A(numdim,nobs_A);
  arma::vec C_A(nobs_A);
  e_A.fill(arma::datum::nan);
  S_A.fill(arma::datum::nan);
  C_A.fill(arma::datum::nan);
  List result(3);
  
  Sigmacofactor=varcov_A.submat(0,0,numdim-1,numdim-1);
  e_A.col(0)=arma::solve(arma::sqrtmat_sympd(Sigmacofactor),eps_A.subvec(0,numdim-1));
  S_A.col(0)=lambda*e_A.col(0);
  C_A(0)=(2.0-lambda)/lambda*arma::dot(S_A.col(0),S_A.col(0));
  if(nobs_A==1||C_A(0)>limit){
    result(0)=C_A;
    result(1)=S_A;
    result(2)=e_A;
    return(result);
  }
  for(int jj=1;jj<nobs_A;jj++){
    AA=varcov_A.submat(0,0,jj*numdim-1,jj*numdim-1);
    AAinv=arma::inv_sympd(AA);
    Sigmacov=varcov_A.submat(jj*numdim,0,(jj+1)*numdim-1,jj*numdim-1);
    Sigmavar=varcov_A.submat(jj*numdim,jj*numdim,(jj+1)*numdim-1,(jj+1)*numdim-1);
    tempe=-Sigmacov*AAinv*eps_A.subvec(0,jj*numdim-1)+eps_A.subvec(jj*numdim,(jj+1)*numdim-1);
    Sigmacofactor=Sigmavar-Sigmacov*AAinv*Sigmacov.t();
    e_A.col(jj)=arma::solve(arma::sqrtmat_sympd(Sigmacofactor),tempe);
    S_A.col(jj)=lambda*e_A.col(jj)+(1.0-lambda)*S_A.col(jj-1);
    C_A(jj)=(2.0-lambda)/lambda*arma::dot(S_A.col(jj),S_A.col(jj));
    if(C_A(jj)>limit)break;
  }
  
  result(0)=C_A;
  result(1)=S_A;
  result(2)=e_A;
  return(result);
}

//[[Rcpp::export]]
List mchart1_decorrelation_EWMA_multivariate_upward(
    arma::vec eps_A,arma::mat varcov_A,int nobs_A,int numdim,double lambda,double limit){
  arma::vec tempe;
  arma::mat Sigmacofactor,AA,AAinv,Sigmacov,Sigmavar;
  arma::mat e_A(numdim,nobs_A);
  arma::mat S_A(numdim,nobs_A);
  arma::vec C_A(nobs_A);
  e_A.fill(arma::datum::nan);
  S_A.fill(arma::datum::nan);
  C_A.fill(arma::datum::nan);
  List result(3);
  
  Sigmacofactor=varcov_A.submat(0,0,numdim-1,numdim-1);
  e_A.col(0)=arma::solve(arma::sqrtmat_sympd(Sigmacofactor),eps_A.subvec(0,numdim-1));
  S_A.col(0)=lambda*e_A.col(0);
  C_A(0)=std::sqrt((2.0-lambda)/lambda)*arma::max(S_A.col(0));
  if(nobs_A==1||C_A(0)>limit){
    result(0)=C_A;
    result(1)=S_A;
    result(2)=e_A;
    return(result);
  }
  for(int jj=1;jj<nobs_A;jj++){
    AA=varcov_A.submat(0,0,jj*numdim-1,jj*numdim-1);
    AAinv=arma::inv_sympd(AA);
    Sigmacov=varcov_A.submat(jj*numdim,0,(jj+1)*numdim-1,jj*numdim-1);
    Sigmavar=varcov_A.submat(jj*numdim,jj*numdim,(jj+1)*numdim-1,(jj+1)*numdim-1);
    tempe=-Sigmacov*AAinv*eps_A.subvec(0,jj*numdim-1)+eps_A.subvec(jj*numdim,(jj+1)*numdim-1);
    Sigmacofactor=Sigmavar-Sigmacov*AAinv*Sigmacov.t();
    e_A.col(jj)=arma::solve(arma::sqrtmat_sympd(Sigmacofactor),tempe);
    S_A.col(jj)=lambda*e_A.col(jj)+(1.0-lambda)*S_A.col(jj-1);
    C_A(jj)=std::sqrt((2.0-lambda)/lambda)*arma::max(S_A.col(jj));
    if(C_A(jj)>limit)break;
  }
  
  result(0)=C_A;
  result(1)=S_A;
  result(2)=e_A;
  return(result);
}
