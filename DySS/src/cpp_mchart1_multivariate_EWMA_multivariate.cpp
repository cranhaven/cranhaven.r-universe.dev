#include <RcppArmadillo.h>
using namespace Rcpp;
//' @keywords internal
//' @noRd
//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
List mchart1_multivariate_EWMA_multivariate_both(
    arma::mat eps_A,int nobs_A,arma::cube var_cube_A,int numdim,double lambda,double limit){
  arma::vec S(numdim);
  arma::mat a_var_mat(numdim,numdim);
  
  arma::mat e_A(numdim,nobs_A);
  arma::mat S_A(numdim,nobs_A);
  arma::vec C_A(nobs_A);
  e_A.fill(arma::datum::nan);
  S_A.fill(arma::datum::nan);
  C_A.fill(arma::datum::nan);
  List result(3);
  
  S.fill(0.0);
  for(int jj=0;jj<nobs_A;jj++){
    a_var_mat=var_cube_A.row(jj);
    e_A.col(jj)=arma::solve(arma::sqrtmat_sympd(a_var_mat),eps_A.col(jj));
    S=lambda*e_A.col(jj)+(1.0-lambda)*S;
    S_A.col(jj)=S;
    C_A(jj)=((2.0-lambda)/lambda)*arma::dot(S,S);
    if(C_A(jj)>limit)break;
  }
  
  result(0)=C_A;
  result(1)=S_A;
  result(2)=e_A;
  return(result);
}

//[[Rcpp::export]]
List mchart1_multivariate_EWMA_multivariate_upward(
    arma::mat eps_A,int nobs_A,arma::cube var_cube_A,int numdim,double lambda,double limit){
  arma::vec S(numdim);
  arma::mat a_var_mat(numdim,numdim);
  
  arma::mat e_A(numdim,nobs_A);
  arma::mat S_A(numdim,nobs_A);
  arma::vec C_A(nobs_A);
  e_A.fill(arma::datum::nan);
  S_A.fill(arma::datum::nan);
  C_A.fill(arma::datum::nan);
  List result(3);
  
  S.fill(0.0);
  for(int jj=0;jj<nobs_A;jj++){
    a_var_mat=var_cube_A.row(jj);
    e_A.col(jj)=arma::solve(arma::sqrtmat_sympd(a_var_mat),eps_A.col(jj));
    S=lambda*e_A.col(jj)+(1.0-lambda)*S;
    S_A.col(jj)=S;
    C_A(jj)=std::sqrt((2.0-lambda)/lambda)*arma::max(S);
    if(C_A(jj)>limit)break;
  }
  
  result(0)=C_A;
  result(1)=S_A;
  result(2)=e_A;
  return(result);
}
