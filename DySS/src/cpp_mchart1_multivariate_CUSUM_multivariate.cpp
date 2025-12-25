#include <RcppArmadillo.h>
using namespace Rcpp;
//' @keywords internal
//' @noRd
//[[Rcpp::depends(RcppArmadillo)]]
//[[Rcpp::export]]
List mchart1_multivariate_CUSUM_multivariate_both(
    arma::mat eps_A,int nobs_A,arma::cube var_cube_A,int numdim,double kk,double limit){
  arma::vec S(numdim),pot_shift(numdim);//,zero_vec(numdim,arma::fill::zeros)
  arma::mat a_var_mat(numdim,numdim);
  
  arma::mat S_A(numdim,nobs_A);
  arma::vec T_A(nobs_A);
  arma::vec C_A(nobs_A);
  S_A.fill(arma::datum::nan);
  C_A.fill(arma::datum::nan);
  List result(2);
  
  S.fill(0.0);
  for(int jj=0;jj<nobs_A;jj++){
    a_var_mat=var_cube_A.row(jj);
    pot_shift=S+eps_A.col(jj);
    T_A(jj)=std::sqrt(arma::dot(pot_shift,arma::solve(a_var_mat,pot_shift)));
    if(T_A(jj)<=kk){
      S.fill(0.0);
    }else{
      S=(1.0-kk/T_A(jj))*pot_shift;
    }
    S_A.col(jj)=S;
    C_A(jj)=std::sqrt(arma::dot(S,arma::solve(a_var_mat,S)));
    if(C_A(jj)>limit)break;
  }
  
  result(0)=C_A;
  result(1)=S_A;
  return(result);
}

//[[Rcpp::export]]
List mchart1_multivariate_CUSUM_multivariate_upward(
    arma::mat eps_A,int nobs_A,arma::cube var_cube_A,int numdim,double kk,double limit){
  arma::vec S(numdim),pot_shift(numdim);
  arma::vec zero_vec(numdim,arma::fill::zeros);
  arma::mat a_var_mat(numdim,numdim);
  
  arma::mat S_A(numdim,nobs_A);
  arma::vec T_A(nobs_A);
  arma::vec C_A(nobs_A);
  S_A.fill(arma::datum::nan);
  C_A.fill(arma::datum::nan);
  List result(2);
  
  S.fill(0.0);
  for(int jj=0;jj<nobs_A;jj++){
    a_var_mat=var_cube_A.row(jj);
    pot_shift=S+eps_A.col(jj);
    T_A(jj)=std::sqrt(arma::dot(pot_shift,arma::solve(a_var_mat,pot_shift)));
    if(T_A(jj)<=kk){
      S.fill(0.0);
    }else{
      S=arma::max((1.0-kk/T_A(jj))*pot_shift,zero_vec);
    }
    S_A.col(jj)=S;
    C_A(jj)=std::sqrt(arma::dot(S,arma::solve(a_var_mat,S)));
    if(C_A(jj)>limit)break;
  }
  
  result(0)=C_A;
  result(1)=S_A;
  return(result);
}
