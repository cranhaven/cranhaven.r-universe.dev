// This script implement spatial covariate-augumented Poisson factor model.
// Date: 2022-12-27


#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include<ctime>
//#include<boost/math/tools/minima.hpp>

#ifndef INT_MIN
#define INT_MIN (-INT_MAX - 1)
#endif

using namespace Rcpp;
using namespace arma;
using namespace std;
// using boost::math::tools::brent_find_minima;


// Define global variables
//double X_count_ij, a_i, invLambda_j,Mu_x_ij;
/*
 * Auxiliary
 */
//' @keywords internal
//' @noRd
//' 
// Calculate the weighted matrix based on distance.


// diag(W0* Cki * W0^t)
vec decomp(const mat& Cki, const mat& W0){
  vec s, tmp1;
  mat U, V, WC12;
  svd(U, s, V, Cki);
  WC12 = W0 * (U * diagmat(sqrt(s))); // p * q
  tmp1 = sum(WC12 % WC12, 1);
  return tmp1;
}  

// arma::mat get_weightedmean(const arma::mat& V, const arma::sp_mat& weiAdj){
//   int i, n = V.n_rows, q= V.n_cols;
//   vec m(n);
//   mat Uv(n, q, fill::zeros);
//   for (i = 0; i < n; i++)
//   {
//     arma::rowvec row(weiAdj.row(i)); // the class label of neighbors of i-th sample.
//     uvec q1 = find(row > 0);
//     // cout<<q1.n_rows<<endl;
//     if( q1.n_rows>0){
//       Uv.row(i) = sum(V.rows(q1) % repmat(row(q1),1, q)) / accu(row(q1));
//     }
//     
//   }
//   return Uv;
// }

arma::mat get_weightedmean(const arma::mat& V, const arma::sp_mat& weiAdj) {
  int n = V.n_rows;
  int q = V.n_cols;
  arma::mat Uv(n, q, arma::fill::zeros);

  // 预先分配一个用于存储邻居权重的向量
  arma::rowvec weights(n, arma::fill::zeros);

  for (int i = 0; i < n; ++i) {
    // 直接从稀疏矩阵中获取权重，并计算它们的和
    weights = weiAdj.row(i);
    arma::uvec q1 = arma::find(weights > 0);

    if (q1.n_elem > 0) {
      // 使用更高效的索引方法
      arma::mat V_subset = V.rows(q1);
      // 直接在V_subset上应用权重（权重已经是一个向量）
      Uv.row(i) =  (weights(q1).t()*V_subset) / arma::accu(weights(q1));
    }
  }

  return Uv;
}

arma::mat get_weightedmean1(const arma::mat& V, const arma::sp_mat& weiAdj) {  
  int n = V.n_rows;  
  int q = V.n_cols;  
  arma::mat Uv(n, q, arma::fill::zeros);  
  
  for (int i = 0; i < n; ++i) {  
    double sum_weights = 0.0;  
    arma::rowvec weighted_sum(q, arma::fill::zeros);  
    
    // 遍历稀疏矩阵weiAdj的第i行  
    for (arma::sp_mat::const_row_iterator	 it = weiAdj.begin_row(i); it != weiAdj.end_row(i); ++it) {  
      int col_index = it.col(); // 邻居的索引；访问it行的非零元的索引指标:j_1  
      double weight = (*it); // 对应的权重;指针用法，访问it行的非零元  
      
      // 累加加权和  
      weighted_sum += V.row(col_index) * weight;  
      sum_weights += weight;  
    }  
    
    // 如果sum_weights非零，则计算加权均值  
    if (sum_weights > 0.0) {  
      Uv.row(i) = weighted_sum / sum_weights;  
    }  
  }  
  
  return Uv;  
}

double calELBO(const mat& X, const sp_mat& weiAdj, const vec& w_plus, const vec& mu, 
               const mat& B, const vec& Lam, const mat& Phi, const mat& M, 
               const mat& Mu_h, const cube& R){
  int s, S = X.n_rows, q = B.n_cols;
  mat dX = (X - repmat(mu.t(), S, 1) - M*B.t()) % repmat(1/sqrt(Lam.t()), S, 1);
  mat BLBt = B.t()* (B % repmat(1.0/Lam, 1, q));
  mat Rs = sum(R, 2);
  mat R_sc_sum(q, q, fill::zeros);
  double entro_unscale = 0.0;
  for(s=0; s<S; ++s){
    R_sc_sum += R.slice(s)*w_plus(s);
    entro_unscale += log(det(R.slice(s)));
  }
  double logPx = accu(dX % dX) + trace(BLBt* Rs) + S*accu(log(Lam));
  logPx = -0.5* logPx;
  // P_h
  vec logPh_vec = decomp(Phi.i(), (M-Mu_h));
  double logPh = -0.5*(accu(logPh_vec % w_plus) + trace(R_sc_sum* Phi.i()) + S*log(det(Phi)));
  
  //entropy
  double entropy = entro_unscale*0.5;
  
  return (logPx+logPh + entropy);
}


void run_Estep(const mat& X, const sp_mat& weiAdj, const vec& w_plus, const vec& mu, 
               const mat& B, const vec& Lam, const mat& Phi, mat& M, 
               mat& Mu_h, cube& R){
  int s, S = X.n_rows, q = B.n_cols;
  mat BLBt = B.t()* (B % repmat(1.0/Lam, 1, q));
  mat XLB =  (X - repmat(mu.t(), S, 1)) * (B % repmat(1.0/Lam, 1, q)); // n*q
  // double elbo1 = calELBO( X, weiAdj, w_plus, mu, B, Lam, Phi, M, Mu_h, R);
  for(s=0; s<S; ++s){
    R.slice(s) = (BLBt + Phi.i()*w_plus(s)).i();
    M.row(s) = (XLB.row(s) +  Mu_h.row(s) * Phi.i()*w_plus(s) ) * R.slice(s) ;
  }
  // double elbo2 = calELBO( X, weiAdj, w_plus, mu, B, Lam, Phi, M, Mu_h, R);
  // Rprintf("dR_M= %4f \n", elbo2 - elbo1);
  
  // update Mu_h
  Mu_h = get_weightedmean(M, weiAdj);
}

// [[Rcpp::export]]
Rcpp::List imFactorCpp(const arma::mat& X, const arma::sp_mat& weiAdj, const arma::vec& w_plus,
                       const arma::vec& mu_int, const arma::mat& B_int,
                       const arma::vec& Lam_int, const arma::mat& Phi_int, const arma::mat& M_int,
                       const arma::cube& R_int, const int& maxIter, const double& epsELBO, 
                       const bool& verbose, const bool& Phi_diag=true){
  int s, S = X.n_rows,  n = X.n_rows, q = B_int.n_cols;
  vec mu(mu_int), Lam(Lam_int);
  mat M(M_int), Phi(Phi_int), B(B_int);
  mat Mu_h = 0.5*M, dX2;
  cube R(R_int);
  vec elbo_vec(maxIter);
  elbo_vec(0) = 1e-20;
  int iter;
  Rprintf("Finish the initialization! \n");
  for(iter=1; iter < maxIter; iter++){
    Rprintf("Start E-step \n");
    run_Estep(X, weiAdj, w_plus, mu, B, Lam, Phi, M, Mu_h, R);
    Rprintf("Finish E-step \n");
    
    // double elbo1 = calELBO( X, weiAdj, w_plus, mu, B, Lam, Phi, M, Mu_h, R);
    Rprintf("Update mu \n");
    mu = trans(mean(X - M * B.t()));
    // double elbo2 = calELBO( X, weiAdj, w_plus, mu, B, Lam, Phi, M, Mu_h, R);
    // Rprintf("dmu= %4f \n", elbo2 - elbo1);
    Rprintf("Update B \n");
    mat R_sum = sum(R, 2);
    B = trans(X.each_row() - mu.t()) * M * inv(M.t()*M + R_sum);
    // double elbo3 = calELBO( X, weiAdj, w_plus, mu, B, Lam, Phi, M, Mu_h, R);
    // Rprintf("dB= %4f \n", elbo3 - elbo2);
    
    Rprintf("Update Lambda \n");
    dX2 =(X - M * B.t() - repmat(mu.t(), n, 1)) % (X - M * B.t() - repmat(mu.t(), n, 1));
    vec svec = decomp(R_sum, B);
    mat R_sc_sum = zeros(q, q);
    for(s=0; s<S; ++s){
      R_sc_sum += R.slice(s) * w_plus(s);
    }
    Lam = trans(mean(dX2)) + svec/S;
    // double elbo4 = calELBO( X, weiAdj, w_plus, mu, B, Lam, Phi, M, Mu_h, R);
    // Rprintf("dLam= %4f \n", elbo4 - elbo3);
    
    Rprintf("Update Phi \n");
    mat dM = (M-Mu_h) %  repmat(sqrt(w_plus), 1, q);
    Phi = (dM.t() * dM + R_sc_sum)/S;
    if(Phi_diag){
      Phi = diagmat(Phi);
    }
    // double elbo5 = calELBO( X, weiAdj, w_plus, mu, B, Lam, Phi, M, Mu_h, R);
    // Rprintf("dPhi= %4f \n", elbo5 - elbo4);
    
    
    elbo_vec(iter) = calELBO(X, weiAdj, w_plus, mu, B, Lam, Phi, M, Mu_h, R);
    // output algorithm info.
    if(verbose){
      Rprintf("iter = %d, elbo= %4f, delbo=%4f \n", 
              iter +1, elbo_vec(iter), (elbo_vec(iter)  - elbo_vec(iter-1))/ abs(elbo_vec(iter-1)));
    }
    if(abs((elbo_vec(iter)  - elbo_vec(iter-1))/ elbo_vec(iter-1)) < epsELBO) break;
  }
  
  // output return value
  List resList = List::create(
    Rcpp::Named("M") = M,
    Rcpp::Named("B") = B,
    Rcpp::Named("mu") = mu,
    Rcpp::Named("Lam") = Lam,
    Rcpp::Named("Phi") = Phi,
    Rcpp::Named("R") = R,
    Rcpp::Named("ELBO") = elbo_vec(iter-1),
    //Rcpp::Named("dELBO") = elbo_vec(iter-1)  - elbo_vec(iter-2),
    Rcpp::Named("ELBO_seq") = elbo_vec.subvec(0, iter-1)
  );
  return(resList);
}



double calELBO_approxPhi(const mat& X,  const vec& mu, 
               const mat& B, const vec& Lam, const mat& Phi, const mat& M, 
               const mat& Mu_h, const mat& R){
  int  S = X.n_rows, q = B.n_cols;
  mat dX = (X - repmat(mu.t(), S, 1) - M*B.t()) % repmat(1/sqrt(Lam.t()), S, 1);
  mat BLBt = B.t()* (B % repmat(1.0/Lam, 1, q));
  mat Rs = R * S;
  
  double logPx = accu(dX % dX) + trace(BLBt* Rs) + S*accu(log(Lam));
  logPx = -0.5* logPx;
  // P_h
  vec logPh_vec = decomp(Phi.i(), (M-Mu_h));
  double logPh = -0.5*(accu(logPh_vec) + trace(Rs* Phi.i()) + S*log(det(Phi)));
  
  //entropy
  double entropy = 0.5 * S*log(det(R));
  
  return (logPx+logPh + entropy);
}


void run_Estep_approxPhi(const mat& X, const sp_mat& weiAdj,  const vec& mu, 
               const mat& B, const vec& Lam, const mat& Phi, mat& M, 
               mat& Mu_h, mat& R){
  
  int S = X.n_rows, q = B.n_cols;
  
  mat BLBt = B.t()* (B % repmat(1.0/Lam, 1, q));
  Rprintf("Calculate XLB...");
  mat XLB =  (X - repmat(mu.t(), S, 1)) * (B % repmat(1.0/Lam, 1, q)); // n*q
  // Rprintf("dR_M= good");
  // double elbo1 = calELBO_approxPhi(X,  mu, B, Lam, Phi, M, Mu_h, R);
  R = (BLBt + Phi.i()).i();
  Rprintf("Calculate M...");
  M = (XLB +  Mu_h * Phi.i() ) * R ;
  // Rprintf("dR_M= good");
  // double elbo2 = calELBO_approxPhi( X,  mu, B, Lam, Phi, M, Mu_h, R);
  // Rprintf("dR_M= %4f \n", elbo2 - elbo1);
  
  // update Mu_h
  Rprintf("Calculate Mu_h...");
  Mu_h = get_weightedmean(M, weiAdj);
}


// Approximate the Phi_s use the same Phi to speed up the computation when the  number of spots is very large.
// [[Rcpp::export]]
Rcpp::List approxPhi_imFactorCpp(const arma::mat& X, const arma::sp_mat& weiAdj, 
                       const arma::vec& mu_int, const arma::mat& B_int,
                       const arma::vec& Lam_int, const arma::mat& Phi_int, const arma::mat& M_int,
                       const arma::mat& R_int, const int& maxIter, const double& epsELBO, 
                       const bool& verbose, const bool& Phi_diag=true){
  int  S = X.n_rows, n = X.n_rows;
  vec mu(mu_int), Lam(Lam_int);
  mat M(M_int), Phi(Phi_int), B(B_int);
  mat Mu_h = 0.5*M, dX2;
  mat R(R_int);
  vec elbo_vec(maxIter);
  elbo_vec(0) = 1e-20;
  int iter;
  Rprintf("Finish the initialization! \n");
  for(iter=1; iter < maxIter; iter++){
    Rprintf("Start E-step \n");
    run_Estep_approxPhi(X, weiAdj,  mu, B, Lam, Phi, M, Mu_h, R);
    Rprintf("Finish E-step \n");
    
    // double elbo1 = calELBO_approxPhi(X,  mu, B, Lam, Phi, M, Mu_h, R);
    Rprintf("Update mu \n");
    mu = trans(mean(X - M * B.t()));
    // double elbo2 = calELBO_approxPhi(X,  mu, B, Lam, Phi, M, Mu_h, R);
    // Rprintf("dmu= %4f \n", elbo2 - elbo1);
    Rprintf("Update B \n");
    mat R_sum = S * R;
    B = trans(X.each_row() - mu.t()) * M * inv(M.t()*M + R_sum);
    // double elbo3 = calELBO_approxPhi(X,  mu, B, Lam, Phi, M, Mu_h, R);
    // Rprintf("dB= %4f \n", elbo3 - elbo2);
    
    Rprintf("Update Lambda \n");
    dX2 =(X - M * B.t() - repmat(mu.t(), n, 1)) % (X - M * B.t() - repmat(mu.t(), n, 1));
    vec svec = decomp(R_sum, B);
    Lam = trans(mean(dX2)) + svec/S;
    // double elbo4 = calELBO_approxPhi(X,  mu, B, Lam, Phi, M, Mu_h, R);
    // Rprintf("dLam= %4f \n", elbo4 - elbo3);
    
    Rprintf("Update Phi \n");
    mat dM = (M-Mu_h);
    Phi = (dM.t() * dM )/S + R;
    if(Phi_diag){
      Phi = diagmat(Phi);
    }
    // double elbo5 = calELBO_approxPhi(X,  mu, B, Lam, Phi, M, Mu_h, R);
    // Rprintf("dPhi= %4f \n", elbo5 - elbo4);
    
    
    elbo_vec(iter) = calELBO_approxPhi(X,  mu, B, Lam, Phi, M, Mu_h, R);
    // output algorithm info.
    if(verbose){
      Rprintf("iter = %d, elbo= %4f, delbo=%4f \n", 
              iter +1, elbo_vec(iter), (elbo_vec(iter)  - elbo_vec(iter-1))/ abs(elbo_vec(iter-1)));
    }
    if(abs((elbo_vec(iter)  - elbo_vec(iter-1))/ elbo_vec(iter-1)) < epsELBO) break;
  }
  
  // output return value
  List resList = List::create(
    Rcpp::Named("M") = M,
    Rcpp::Named("B") = B,
    Rcpp::Named("mu") = mu,
    Rcpp::Named("Lam") = Lam,
    Rcpp::Named("Phi") = Phi,
    Rcpp::Named("R") = R,
    Rcpp::Named("ELBO") = elbo_vec(iter-1),
    //Rcpp::Named("dELBO") = elbo_vec(iter-1)  - elbo_vec(iter-2),
    Rcpp::Named("ELBO_seq") = elbo_vec.subvec(0, iter-1)
  );
  return(resList);
}
