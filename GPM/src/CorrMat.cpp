#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace std;
using namespace arma;

// [[Rcpp::export]]
arma::mat CorrMat_Sym(arma::mat X, std::string CorrType, arma::rowvec Omega) {
  int n = X.n_rows;
  int dx = X.n_cols;
  int p = Omega.n_elem;
  arma::mat R = arma::zeros(n, n);
  if(CorrType == "G" && p == dx){
    Omega = arma::sqrt(arma::exp10(Omega));
    X = X.each_row()%Omega;
    for (int i = 0; i < n - 1; ++i){
      for (int j = i + 1; j < n; ++j){
        R(i, j) = arma::sum(arma::pow(X.row(i) - X.row(j), 2));
      }
    }
    R = arma::exp(-(R + trans(R)));
  }else if (CorrType == "PE" && p == (dx + 1)){
    arma::rowvec A = arma::pow(arma::exp10(Omega.head(dx)), 1./Omega(dx));
    X = X.each_row()%A;
    for (int i = 0; i < n - 1; ++i){
      for (int j = i + 1; j < n; ++j){
        R(i, j) = arma::sum(arma::pow(arma::abs(X.row(i) - X.row(j)), Omega(dx)));
      }
    }
    R = arma::exp(-(R + arma::trans(R)));
  }else if (CorrType == "LBG" && p == (dx + 1)){
    rowvec Ap = sqrt(exp10(Omega.head(dx)));
    colvec X22 = pow(1 + sum(pow(X.each_row()%Ap, 2), 1), Omega(dx));
    for (int i = 0; i < n; ++i){
      for (int j = i; j < n; ++j){
        R(i, j) = X22(i) + X22(j) - 
          pow(1 + sum(pow((X.row(i) - X.row(j))%Ap, 2)), Omega(dx)) - 1;
      }
    }
    R = R + trans(R) - diagmat(R);
  }else if (CorrType == "LB" && p == (dx + 2)){
    rowvec Ap = sqrt(exp10(Omega.head(dx)));
    colvec X22 = pow(1 + pow(sum(pow(X.each_row()%Ap, 2), 1), Omega(dx + 1)), Omega(dx));
    for (int i = 0; i < n; ++i){
      for (int j = i; j < n; ++j){
        R(i, j) = X22(i) + X22(j) - 
          pow(1 + pow(sum(pow((X.row(i) - X.row(j))%Ap, 2)), Omega(dx + 1)), Omega(dx)) - 1;
      }
    }
    R = R + trans(R) - diagmat(R);
  }
  return R;
}
// [[Rcpp::export]]
arma::mat CorrMat_Vec(arma::mat X1, arma::mat X2, std::string CorrType, arma::rowvec Omega) {
  int n = X1.n_rows;
  int m = X2.n_rows;
  int dx = X1.n_cols;
  int p = Omega.n_elem;
  mat R = zeros(n, m);
  if(CorrType == "G" && p == dx){
    Omega = sqrt(exp10(Omega));
    X1 = X1.each_row()%Omega;
    X2 = X2.each_row()%Omega;
    if(n >= m){
      for(int i = 0; i < m; ++i){
        R.col(i) = sum(pow(X1.each_row() - X2.row(i), 2), 1);
      }
    }else{
      for(int i = 0; i < n; ++i){
        R.row(i) = trans(sum(pow(X2.each_row() - X1.row(i), 2), 1));
      }
    }
    R = exp(-R);
  }else if (CorrType == "PE" && p == (dx + 1)){
    double power = Omega(dx);
    Omega = pow(exp10(Omega.head(dx)), 1./power);
    X1 = X1.each_row()%Omega;
    X2 = X2.each_row()%Omega;    
    if(n >= m){
      for(int i = 0; i < m; ++i){
        R.col(i) = sum(pow(abs(X1.each_row() - X2.row(i)), power), 1);
      }
    }else{
      for(int i = 0; i < n; ++i){
        R.row(i) = trans(sum(pow(abs(X2.each_row() - X1.row(i)), power), 1));
      }
    }
    R = exp(-R);    
  }else if (CorrType == "LBG" && p == (dx + 1)){
    rowvec Ap = sqrt(exp10(Omega.head(dx)));
    X1 = X1.each_row()%Ap;
    X2 = X2.each_row()%Ap;      
    colvec part1 = pow(1 + sum(pow(X1, 2), 1),  Omega(dx));
    colvec part2 = pow(1 + sum(pow(X2, 2), 1), Omega(dx));    
    if(n >= m){
      for(int i = 0; i < m; ++i){
        R.col(i) = part2(i) - 
          pow(1 + sum(pow(X1.each_row() - X2.row(i), 2), 1), Omega(dx));
      }
      R = (R.each_col() + part1) - 1;
    }else{
      for(int i = 0; i < n; ++i){
        R.row(i) = part1(i) - 
          trans(pow(1 + sum(pow(X2.each_row() - X1.row(i), 2), 1), Omega(dx)));
      }
      R = (R.each_row() + trans(part2)) - 1;
    }    
  }else if (CorrType == "LB" && p == (dx + 2)){
    rowvec Ap = sqrt(exp10(Omega.head(dx)));
    X1 = X1.each_row()%Ap;
    X2 = X2.each_row()%Ap;      
    colvec part1 = pow(1 + pow(sum(pow(X1, 2), 1), Omega(dx + 1)),  Omega(dx));
    colvec part2 = pow(1 + pow(sum(pow(X2, 2), 1), Omega(dx + 1)), Omega(dx));
    if(n >= m){
      for(int i = 0; i < m; ++i){
        R.col(i) = part2(i) - 
          pow(1 + pow(sum(pow(X1.each_row() - X2.row(i), 2), 1), Omega(dx + 1)), Omega(dx));
      }
      R = (R.each_col() + part1) - 1;
    }else{
      for(int i = 0; i < n; ++i){
        R.row(i) = part1(i) - 
          trans(pow(1 + pow(sum(pow(X2.each_row() - X1.row(i), 2), 1), Omega(dx + 1)), Omega(dx)));
      }
      R = (R.each_row() + trans(part2)) - 1;
    }    
  }
  return R;
}

/*
arma::mat CorrMat_For(mat X1, mat X2, std::string CorrType, rowvec Omega) {
  int n = X1.n_rows;
  int m = X2.n_rows;
  int dx = X1.n_cols;
  int p = Omega.n_elem;
  mat R = zeros(n, m);
  if(CorrType == "G" && p == dx){
    Omega = sqrt(exp10(Omega));
    X1 = X1.each_row()%Omega;
    X2 = X2.each_row()%Omega;
    for (int i = 0; i < n; ++i){
      for (int j = 0; j < m; ++j){
        R(i, j) = sum(pow(X1.row(i) - X2.row(j), 2));
      }
    }
    R = exp(-R);
  }else if (CorrType == "PE" && p == (dx + 1)){
    rowvec A = pow(exp10(Omega.head(dx)), 1./Omega(dx));
    X1 = X1.each_row()%A;
    X2 = X2.each_row()%A;
    for (int i = 0; i < n; ++i){
      for (int j = 0; j < m; ++j){
        R(i, j) = sum(pow(abs(X1.row(i) - X2.row(j)), Omega(dx)));
      }
    }
    R = exp(-R);
  }else if (CorrType == "LBG" && p == (dx + 1)){
    rowvec Ap = sqrt(exp10(Omega.head(dx)));
    colvec X12 = pow(1 + sum(pow(X1.each_row()%Ap, 2), 1), Omega(dx));
    colvec X22 = pow(1 + sum(pow(X2.each_row()%Ap, 2), 1), Omega(dx));
    for (int i = 0; i < n; ++i){
      for (int j = 0; j < m; ++j){
        R(i, j) = X12(i) + X22(j) - 
          pow(1 + sum(pow((X1.row(i) - X2.row(j))%Ap, 2)), Omega(dx)) - 1;
      }
    }
  }else if (CorrType == "LB" && p == (dx + 2)){
    rowvec Ap = sqrt(exp10(Omega.head(dx)));
    colvec X12 = pow(1 + pow(sum(pow(X1.each_row()%Ap, 2), 1), Omega(dx + 1)), Omega(dx));
    colvec X22 = pow(1 + pow(sum(pow(X2.each_row()%Ap, 2), 1), Omega(dx + 1)), Omega(dx));
    for (int i = 0; i < n; ++i){
      for (int j = 0; j < m; ++j){
        R(i, j) = X12(i) + X22(j) - 
          pow(1 + pow(sum(pow((X1.row(i) - X2.row(j))%Ap, 2)), Omega(dx + 1)), Omega(dx)) - 1;
      }
    }
  }
  return R;
}
 */
