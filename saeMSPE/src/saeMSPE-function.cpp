#include <RcppArmadillo.h>
#include <iostream>
#include <armadillo>
#include <cmath>
#include <gsl/gsl_multimin.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace std;

// BaseModel class for handling missing data (NaN) cleanup
class BaseModel {
public:
  // Clean NaN values from a vector
  arma::vec clean_nan_from_vector(arma::vec vec) const {
    if (vec.has_nan()) {
      vec = vec.elem(arma::find_finite(vec));
    }
    return vec;
  }
  
  // Clean NaN values from a matrix
  arma::mat clean_nan_from_matrix(arma::mat mat) const {
    if (mat.has_nan()) {
      mat = mat.elem(arma::find_finite(mat));
    }
    return mat;
  }
  
  // Handle missing data based on flags (na_rm and na_omit)
  void handle_missing_data(arma::mat& X, arma::vec& Y, arma::vec& D, bool na_rm, bool na_omit) const {
    if (na_rm) {
      X = clean_nan_from_matrix(X);
      Y = clean_nan_from_vector(Y);
      D = clean_nan_from_vector(D);
    } else if (na_omit) {
      if (X.has_nan() || Y.has_nan() || D.has_nan()) {
        Rcpp::stop("Input contains missing values (NaN). Please handle missing data before proceeding.");
      }
    }
  }
  
  void handle_missing_data(arma::mat& X, arma::vec& Y, arma::vec& ni, arma::mat& Xmean, bool na_rm, bool na_omit) const {
    if (na_rm) {
      X = clean_nan_from_matrix(X);
      Y = clean_nan_from_vector(Y);
      ni = clean_nan_from_vector(ni);
      Xmean = clean_nan_from_matrix(Xmean);
    } else if (na_omit) {
      if (X.has_nan() || Y.has_nan() || ni.has_nan() || Xmean.has_nan()) {
        Rcpp::stop("Input contains missing values (NaN). Please handle missing data before proceeding.");
      }
    }
  }
  
  // Helper function: extract matrix X and vector Y from formulas and data
  List parseFormula(const Formula& formula, const DataFrame& data) {
    Environment stats = Environment::namespace_env("stats");
    Function model_matrix = stats["model.matrix"];
    Function model_frame = stats["model.frame"];
    Function model_response = stats["model.response"];
    
    SEXP mf = model_frame(formula, data);
    arma::mat X = as<arma::mat>(model_matrix(formula, mf));
    arma::vec Y = as<arma::vec>(model_response(mf));
    
    return List::create(Named("X") = X, Named("Y") = Y);
  }
  
  arma::vec BetahatFHFun(arma::mat X, arma::vec Y, double Ahat, arma::vec D, int p){
    arma::mat p1 = arma::zeros(p, p); arma::mat p2 = arma::zeros(p, 1); 
    int n = X.n_rows;
    double AD;
    for(int t = 0;t < n;t++){
      AD = Ahat + D(t);
      p1 = p1 + X.row(t).t() * X.row(t) / AD;
      p2 = p2 + X.row(t).t() * Y(t) / AD;
    }
    return(arma::solve(p1, p2));
  }
  
  arma::vec BetahatNERFun(arma::mat XtX, arma::mat XtY, arma::mat xdot, arma::vec Y, arma::vec ydot, arma::mat Vmat, int m, arma::vec ni){
    // calculate \hat\beta based on \hat{e} and \hat{v} for NER model
    int p = XtX.n_cols;
    arma::mat p1 = arma::zeros(p, p); arma::mat p2 = arma::zeros(p, 1); 
    
    for(int i = 0; i < m; i++){
      int nstart = sum(ni(arma::span(0,i))) - ni(i);
      arma::mat Vtemp = Vmat.submat(nstart, nstart + ni(i) - 1, nstart, nstart + ni(i) - 1);
      
      p1 = p1 + xdot.rows(nstart, nstart + ni(i) - 1).t() * Vtemp * xdot.rows(nstart, nstart + ni(i) - 1);
      p2 = p2 + xdot.rows(nstart, nstart + ni(i) - 1).t() * Vtemp * ydot.subvec(nstart, nstart + ni(i) - 1);
    }
    return(arma::solve(XtX - p1, XtY - p2));
  }
};


double varfh(arma::vec Y, arma::mat X, arma::vec D, int method){
  // main variance estimation function for FH model
  if (X.n_cols <= 0) {
    Rcpp::stop("X must have at least one column.");
  }
  if (method < 1 || method > 4) {
    Rcpp::stop("Method must be between 1 and 4.");
  }
  
  List result;
  Rcpp::Environment base("package:smallarea");
  if (method == 1){
    Rcpp::Function prasadraoestC = base["prasadraoest"];
    result = prasadraoestC(Y, X, D);
  }
  if (method == 2){
    Rcpp::Function resimaxilikelihoodC = base["resimaxilikelihood"];
    result = resimaxilikelihoodC(Y, X, D, 100);
  }
  if (method == 3){
    Rcpp::Function maximlikelihoodC = base["maximlikelihood"];
    result = maximlikelihoodC(Y, X, D);
  }
  if (method == 4){
    Rcpp::Function fayherriotC = base["fayherriot"];
    result = fayherriotC(Y, X, D);
  }
  double varEST = result["estimate"];
  if(varEST <= 0){varEST = 0;}
  return varEST;
}


// [[Rcpp::export]]
double varfh(Rcpp::Formula formula, Rcpp::DataFrame data, arma::vec D, int method, bool na_rm = false, bool na_omit = false){
  // main variance estimation function for FH model
  BaseModel model;
  List parsedData = model.parseFormula(formula, data);
  arma::mat X = parsedData["X"];
  arma::vec Y = parsedData["Y"];
  model.handle_missing_data(X, Y, D, na_rm, na_omit);
  
  if (X.n_cols <= 0) {
    Rcpp::stop("X must have at least one column.");
  }
  if (method < 1 || method > 4) {
    Rcpp::stop("Method must be between 1 and 4.");
  }
  
  List result;
  Rcpp::Environment base("package:smallarea");
  
  if (method == 1){
    Rcpp::Function prasadraoestC = base["prasadraoest"];
    result = prasadraoestC(Y, X, D);
  } else if (method == 2){
    Rcpp::Function resimaxilikelihoodC = base["resimaxilikelihood"];
    result = resimaxilikelihoodC(Y, X, D, 100);
  } else if (method == 3){
    Rcpp::Function maximlikelihoodC = base["maximlikelihood"];
    result = maximlikelihoodC(Y, X, D);
  } else if (method == 4){
    Rcpp::Function fayherriotC = base["fayherriot"];
    result = fayherriotC(Y, X, D);
  }
  
  double varEST = result["estimate"];
  if (varEST <= 0) {
    varEST = 0;
  }
  
  return varEST;
}

List mseu(arma::vec D,int index, int m, int p, arma::mat X, arma::mat Y, int method){
  // sub-function for "mspeFHjack()"
  BaseModel model;
  
  arma::vec biu(m); arma::vec thetaHatu(m);
  for(int u = 0;u < m;u++){
    arma::vec Du = D; Du.shed_rows(u, u);
    arma::mat dataX = X; dataX.shed_rows(u, u);
    arma::mat dataY = Y; dataY.shed_rows(u, u);
    double Ahatu = varfh(dataY, dataX, Du, method);
    arma::vec bhatu = model.BetahatFHFun(dataX, dataY, Ahatu, Du, p);
    double Bu = D(index) / (Ahatu+D(index));
    biu(u) = Ahatu * Bu;
    thetaHatu(u) = as_scalar(X.row(index) * bhatu + (1 - Bu) * (Y(index) - X.row(index) * bhatu));
  }
  return List::create(Named("bhatu") = biu, Named("thetahatu") = thetaHatu);
}

// [[Rcpp::export]]
List mspeFHjack(Rcpp::Formula formula, Rcpp::DataFrame data, arma::vec D, int method = 1, bool na_rm = false, bool na_omit = false){
  // main funtion of jackknife MSPE estimation method for FH model
  BaseModel model;
  List parsedData = model.parseFormula(formula, data);
  arma::mat X = parsedData["X"];
  arma::vec Y = parsedData["Y"];
  model.handle_missing_data(X, Y, D, na_rm, na_omit);
  
  // check arguments
  if (method < 1 || method > 3) {
    Rcpp::stop("Method must be between 1 and 3.");
  }
  
  int m = Y.size(); int p = X.n_cols;
  if(method == 4){
    return 0;
  }else{
    double Ahat = varfh(formula, data, D, method, na_rm, na_omit);
    arma::vec bhat = model.BetahatFHFun(X, Y, Ahat, D, p);
    arma::vec mseFH(m);
    arma::vec B = D / (D+Ahat);
    arma::vec b = Ahat * B;
    arma::vec thetahat = X * bhat + (1 - B) % (Y - X * bhat);
    for(int i = 0;i < m;i++){
      List uhat = mseu(D,i,m,p,X,Y,method);
      arma::vec bhatu =uhat["bhatu"];
      arma::vec thetahatu = uhat["thetahatu"];
      mseFH(i) = b(i) - (m - 1) * sum(bhatu - b(i))/m + (m - 1) * sum(pow(thetahatu - thetahat(i),2))/m;
    }
    return List::create(Named("MSPE") = mseFH, Named("bhat") = bhat, Named("Ahat") = Ahat);
  }
}


//[[Rcpp::export]]
List mspeFHpb(Rcpp::Formula formula, Rcpp::DataFrame data, arma::vec D, int K = 50, int method = 4, bool na_rm = false, bool na_omit = false){
  // main funtion of parameter bootstrap MSPE estimation method for FH model
  BaseModel model;
  List parsedData = model.parseFormula(formula, data);
  arma::mat X = parsedData["X"];
  arma::vec Y = parsedData["Y"];
  model.handle_missing_data(X, Y, D, na_rm, na_omit);
  
  // check arguments
  if (K <= 0) {
    Rcpp::stop("K must be greater than 0.");
  }
  
  int m = Y.size(); int p = X.n_cols;
  double psi_FH = varfh(Y, X, D, method);
  arma::vec bhat = model.BetahatFHFun(X, Y, psi_FH, D, p);
  arma::vec g1 = arma::vec(m); arma::vec g2 = arma::vec(m); arma::vec g3 = arma::vec(m);
  arma::vec theta = arma::vec(m); 
  
  arma::mat XtinvVX = arma::zeros(p, p);
  for(int i=0; i<m; i++){
    XtinvVX = XtinvVX + X.row(i).t() * X.row(i) / (psi_FH + D(i));
  }
  arma::mat inv_XtinvVX = inv(XtinvVX);
  
  for(int i=0; i<m; i++){
    g1(i) = psi_FH * D(i) / (psi_FH + D(i));
    g2(i) = as_scalar(((pow(D(i), 2) / pow(psi_FH + D(i), 2)) * X.row(i) * inv_XtinvVX * X.row(i).t()));
    theta(i) = as_scalar(X.row(i) * bhat + (Y(i) - X.row(i) * bhat) * psi_FH / (psi_FH + D(i)));
  }
  
  arma::mat g1_star = arma::zeros(m, K); arma::mat g2_star = arma::zeros(m, K); arma::mat theta_star = arma::zeros(m, K);
  arma::vec g1_stark(m), g2_stark(m), thetak_star(m);
  double psi_FHk = 0;
  arma::vec Xb = X*bhat;
  arma::mat XtinvVXk = arma::zeros(p, p);
  for(int k=0; k<K; k++){
    arma::vec Y_star = Xb + pow(psi_FH, 0.5) * arma::randn(m,1) + pow(D, 0.5) % arma::randn(m,1);
    
    psi_FHk = varfh(Y_star, X, D, method);
    arma::vec bhatk = model.BetahatFHFun(X, Y, psi_FHk, D, p);
    
    for(int i=0; i<m; i++){
      XtinvVXk = XtinvVXk + X.row(i).t() * X.row(i) / (psi_FHk + D(i));
    }
    arma::mat inv_XtinvVXk = inv(XtinvVXk);
    for(int i=0; i<m; i++){
      g1_stark(i) = psi_FHk * D(i) / (psi_FHk + D(i));
      g2_stark(i) = as_scalar(((pow(D(i), 2) / pow(psi_FHk + D(i), 2)) * X.row(i) * inv_XtinvVXk * X.row(i).t()));
      thetak_star(i) = as_scalar(X.row(i) * bhatk + (Y(i) - X.row(i) * bhatk) * psi_FHk / (psi_FHk + D(i)));
    }
    g1_star.col(k) = g1_stark;
    g2_star.col(k) = g2_stark;
    theta_star.col(k) = thetak_star;
  }
  arma::mat summat = arma::zeros(m,K);
  for(int i=0; i<K; i++){
    for(int j=0; j<m; j++){
      summat(j,i) = pow(theta_star(j, i) - theta(j), 2);
    }
  }
  arma::mat tempmat = g1_star + g2_star;
  arma::vec mspe_theta = 2.0 * (g1 + g2) - mean(tempmat, 1) + mean(summat, 1);
  return List::create(Named("MSPE") = mspe_theta, Named("bhat") = bhat, Named("Ahat") = psi_FH);
}


List FHdbootstrap(int m, int p, arma::mat X, arma::vec Y, arma::vec D, int method){
  // sub-function for "mspeFHdb()"
  BaseModel model;
  double Ahat = varfh(Y, X, D, method);
  arma::vec bhat = model.BetahatFHFun(X, Y, Ahat, D, p);
  arma::vec theta = X * bhat + (Y - X * bhat) % (Ahat/(Ahat + D));
  return List::create(Named("theta") = theta, Named("bhat") = bhat, Named("Ahat") = Ahat);
}

//[[Rcpp::export]]
List mspeFHdb(Rcpp::Formula formula, Rcpp::DataFrame data, arma::vec D, int K = 50, int C = 50, int method = 1, bool na_rm = true, bool na_omit = false){
  // main funtion of double bootstrap MSPE estimation method for FH model
  BaseModel model;
  List parsedData = model.parseFormula(formula, data);
  arma::mat X = parsedData["X"];
  arma::vec Y = parsedData["Y"];
  model.handle_missing_data(X, Y, D, na_rm, na_omit);
  
  // check arguments
  if (method < 1 || method > 3) {
    Rcpp::stop("Method must be between 1 and 3.");
  }
  if (K <= 0) {
    Rcpp::stop("K must be greater than 0.");
  }
  if (C <= 0) {
    Rcpp::stop("C must be greater than 0.");
  }         
  
  int m = Y.size(); int p = X.n_cols;
  if(method == 4){
    return 0;
  }else {
    double Ahat = varfh(Y, X, D, method);
    arma::vec b = model.BetahatFHFun(X, Y, Ahat, D, p);
    arma::mat umat = arma::zeros(m, K); arma::mat vmat = arma::zeros(m, K);
    arma::mat temp_mat = arma::zeros(m, K);
    arma::vec Xb = X * b;
    for(int k=0; k<K; k++){
      arma::vec thetak = Xb +  pow(Ahat, 0.5) * arma::randn(m,1);
      arma::vec Y_star1 = thetak + pow(D, 0.5) % arma::randn(size(D));
      List temp1 = FHdbootstrap(m, p, X, Y_star1, D, method);
      
      double Ahatk = varfh(Y_star1, X, D, method);
      arma::vec bhatk = model.BetahatFHFun(X, Y_star1, Ahat, D, p);
      arma::vec Xbk = X * bhatk;
      arma::vec thetahatk = Xbk + (Y_star1 - Xbk) % (Ahatk/(Ahatk + D));
      umat.col(k) = pow(thetahatk - thetak, 2);
      
      for(int c=0; c<C; c++){
        arma::vec thetac = Xbk +  pow(Ahatk, 0.5) * arma::randn(m,1);
        arma::vec Y_star2 = thetac + pow(D, 0.5) % arma::randn(size(D));
        double Ahatkc = varfh(Y_star2, X, D, method);
        arma::vec bhatkc = model.BetahatFHFun(X, Y_star2, Ahatkc, D, p);
        arma::vec Xbkc = X * bhatk;
        arma::vec thetahatkc = Xbkc + (Y_star2 - Xbkc) % (Ahatkc/(Ahatkc + D));
        temp_mat.col(c) = pow(thetahatkc - thetac, 2);
      }
      vmat.col(k) = mean(temp_mat,1);
    }
    arma::vec u = mean(umat, 1); arma::vec v = mean(vmat, 1);
    arma::vec mspe_DB(m);
    for(int ii=0; ii<m; ii++){
      if(u(ii) >= v(ii)){
        mspe_DB(ii) = u(ii) + atan(m * (u(ii) - v(ii)))/m;
      }else{
        mspe_DB(ii) = pow(u(ii),2)/(u(ii) + atan(m * (v(ii) - u(ii)))/m);
      }
    }
    return List::create(Named("MSPE") = mspe_DB, Named("bhat") = b, Named("Ahat") = Ahat);
  }
}

// [[Rcpp::export]]
List mspeFHsumca(Rcpp::Formula formula, Rcpp::DataFrame data, arma::vec D, int K = 50, int method = 1, bool na_rm = false, bool na_omit = false){
  // main funtion of Sumca MSPE estimation method for FH model
  BaseModel model;
  
  List parsedData = model.parseFormula(formula, data);
  arma::mat X = parsedData["X"];
  arma::vec Y = parsedData["Y"];
  
  model.handle_missing_data(X, Y, D, na_rm, na_omit);
  
  // check arguments
  if (method < 1 || method > 4) {
    Rcpp::stop("Method must be between 1 and 4.");
  }
  if (K <= 0) {
    Rcpp::stop("K must be greater than 0.");
  }
  
  int m = Y.size(); int p = X.n_cols;
  if(method == 4){
    return 0;
  }else {
    double Ahat = varfh(Y, X, D, method);
    arma::vec bhat = model.BetahatFHFun(X, Y, Ahat, D, p);
    arma::vec a_yAhat = Ahat * D/(Ahat + D);
    arma::vec a_ykAhat = a_yAhat;
    arma::vec Ahat_K(K);
    arma::mat Xb = X * bhat;
    for(int i = 0;i < K;i++){
      arma::vec Y_K = Xb + sqrt(Ahat) * arma::randn(m,1) + sqrt(D) % arma::randn(m,1);
      Ahat_K(i) = varfh(Y_K, X, D, method);
    }
    arma::mat a_ykAhatk = arma::mat(m, K);
    for(int j = 0;j < K;j++){
      a_ykAhatk.col(j) = Ahat_K(j) * D/(Ahat_K(j)+D);
    }
    arma::vec mseFH = 2 * a_yAhat - mean(a_ykAhatk,1);
    return List::create(Named("MSPE") = mseFH, Named("bhat") = bhat, Named("Ahat") = Ahat);
  }
}

List MeanFun(arma::vec ni, arma::mat X, arma::vec Y){
  // calculate xbar(mean of sample x), ybar(mean of sample y),
  // xstar(mean of non-sample x), ystar(mean of non-sample y),
  // xdot(sum of sample x), ydot(sum of sample y) by X,Y
  int m = ni.size(); int n = sum(ni); int p = X.n_cols;
  arma::mat xbar(m, p); arma::vec ybar(m); arma::vec ystar(n); arma::mat xstar(n, p);
  arma::vec ydot(m); arma::mat xdot(m, p);
  for(int i = 0; i < m; i++){
    int nstart = sum(ni(arma::span(0,i))) - ni(i);
    if(ni(i) == 1){
      xbar.row(i) = X.row(nstart);
      ybar(i) = Y(nstart);
    }
    else {
      xbar.row(i) = mean(X.rows(nstart, nstart + ni(i) - 1), 0);
      ybar(i) = mean(Y(arma::span(nstart, nstart + ni(i) - 1)));
    }
    ystar(arma::span(nstart, nstart + ni(i) - 1)) = Y(arma::span(nstart, nstart + ni(i) - 1)) - ybar(i);
    xstar.rows(nstart, nstart + ni(i) - 1) = X.rows(nstart, nstart + ni(i) - 1) - arma::ones(ni(i), 1) * xbar.row(i);
  }
  for(int i = 0; i < m; i++){
    xdot.row(i) = ni(i) * xbar.row(i);
    ydot(i) = ni(i) * ybar(i);
  }
  return List::create(Named("ystar") = ystar, Named("xstar") = xstar, Named("ybar") = ybar, Named("xbar") = xbar, Named("ydot") = ydot, Named("xdot") = xdot);
}

List varnerMOM(arma::vec ni, arma::mat X, arma::vec Y){
  // calculate the moment (MOM) estimator for NER model
  int m = ni.size(); int n = sum(ni); int p = X.n_cols;
  List MeanXY = MeanFun(ni, X, Y);
  arma::mat xbar = MeanXY["xbar"];
  arma::mat xstar = MeanXY["xstar"]; arma::vec ystar = MeanXY["ystar"];
  arma::mat xdot = MeanXY["xdot"]; arma::vec ydot = MeanXY["ydot"];
  arma::mat tXX = X.t() * X;
  arma::mat tXY = X.t() * Y;
  arma::mat tXX_inv = inv(tXX);
  
  arma::mat xmat = arma::zeros(p, p);
  for(int j = 0; j < m; j++){
    xmat = xmat + pow(ni(j),2) * xbar.row(j).t() * xbar.row(j);
  }
  arma::mat xstar1 = xstar.cols(1,p-1);
  arma::vec estar = ystar - xstar1 * inv(xstar1.t() * xstar1) * xstar1.t() * ystar;
  arma::vec u = Y - X * tXX_inv * tXY;
  double nstar = n - sum(diagvec(tXX_inv * xmat));
  arma::vec temp = pow(estar,2);
  double sige2 = sum(temp) / (n - m - p + 1.0); double sigv2;
  arma::vec temp1 = pow(u, 2);
  double temp2 = sum(temp1) - (n - 2.0) * sige2;
  if(temp2 < 0){sigv2 = 0;}
  else{sigv2 = temp2/nstar;}
  arma::mat mata = diagmat(sigv2/(sige2 + ni * sigv2));
  
  arma::vec bhat = inv(tXX - xdot.t() * mata * xdot) * (tXY - xdot.t() * mata * ydot);
  return List::create(Named("bhat") = bhat, Named("sigehat2") = sige2, Named("sigvhat2") = sigv2);
}

double oMLfun(arma::vec ni, arma::mat X, arma::vec Y, arma::mat xbar, arma::vec ybar, arma::mat xdot, arma::vec ydot, double gamma){
  // object function for "varnerML()"
  int m = ni.size(); int n = sum(ni);
  arma::mat amata = diagmat(gamma / (1.0 + ni * gamma));
  arma::vec btil = inv(X.t() * X - xdot.t() * amata * xdot) * (X.t() * Y - xdot.t() * amata * ydot);
  double sige2 = (sum(pow(Y - X * btil, 2)) - sum(pow(ydot - xdot * btil, 2) % (gamma / (1 + ni * gamma)))) / n;
  double val = n * log(abs(sige2)) + m * log(1 + gamma * n / m);
  return val;
}

double wrapped_oMLfun(double gamma, arma::vec ni, arma::mat X, arma::vec Y, arma::mat xbar, arma::vec ybar, arma::mat xdot, arma::vec ydot) {
  return oMLfun(ni, X, Y, xbar, ybar, xdot, ydot, gamma);
}

List varnerML(arma::vec ni, arma::mat X, arma::vec Y) {
  List MeanXY = MeanFun(ni, X, Y);
  arma::mat xbar = MeanXY["xbar"];
  arma::vec ybar = MeanXY["ybar"];
  arma::mat xdot = MeanXY["xdot"];
  arma::vec ydot = MeanXY["ydot"];
  
  Function nlminb("nlminb");
  double gamma_start = 0.5;
  
  List result = nlminb(gamma_start, Rcpp::Named("objective") = Rcpp::InternalFunction(&wrapped_oMLfun),
                       Rcpp::Named("lower") = 0.0, Rcpp::Named("upper") = 10.0,
                       Rcpp::Named("ni") = ni, Rcpp::Named("X") = X, Rcpp::Named("Y") = Y,
                                   Rcpp::Named("xbar") = xbar, Rcpp::Named("ybar") = ybar,
                                   Rcpp::Named("xdot") = xdot, Rcpp::Named("ydot") = ydot);
  
  double ghat = as<double>(result["par"]);
  
  arma::vec a = ghat / (1 + ni * ghat);
  arma::mat amata = diagmat(a);
  arma::vec bhat = inv(X.t() * X - xdot.t() * amata * xdot) * (X.t() * Y - xdot.t() * amata * ydot);
  int n = sum(ni);
  double sige2 = (sum(pow(Y - X * bhat, 2)) - sum((pow(ydot - xdot * bhat, 2)) % a)) / n;
  double sigv2 = ghat * sige2;
  
  return List::create(Named("bhat") = bhat, Named("sigehat2") = sige2, Named("sigvhat2") = sigv2);
}

double oREMLfun(double gamma, int m, int p, int n, arma::vec ni, arma::mat X, arma::mat Y, arma::mat xdot, arma::mat ydot,
                arma::mat XdtXXXd_inv, arma::mat temp1, arma::mat temp2){
  // object function for "varnerREML()"
  arma::mat tempd1 = diagmat(ni + 1.0 / gamma);
  arma::mat tempd2 = diagmat(1.0 + ni * gamma);
  arma::mat temp3 = inv(tempd1 - XdtXXXd_inv);
  double temp4 = as_scalar((temp1 - temp2 * temp3 * temp2.t())/(n-p));
  double temp5 = det(tempd2 - gamma * XdtXXXd_inv);
  double val = (n-p) * log(temp4) + log(temp5);
  return val;
}

double wrapped_oREMLfun(double gamma, arma::vec ni, arma::mat X, arma::vec Y, arma::mat xdot, arma::vec ydot, arma::mat XdtXXXd_inv, arma::mat temp1, arma::mat temp2) {
  int m = ni.size();
  int p = X.n_cols;
  int n = sum(ni);
  return oREMLfun(gamma, m, p, n, ni, X, Y, xdot, ydot, XdtXXXd_inv, temp1, temp2);
}


List varnerREML(arma::vec ni, arma::mat X, arma::vec Y) {
  List MeanXY = MeanFun(ni, X, Y);
  arma::mat xdot = MeanXY["xdot"];
  arma::vec ydot = MeanXY["ydot"];
  arma::mat tXX = X.t() * X;
  arma::mat tXX_inv = inv(tXX);
  arma::mat XtXX_inv = X * tXX_inv;
  int n = sum(ni);
  int p = X.n_cols;
  arma::mat temp1 = Y.t() * (arma::eye(n, n) - XtXX_inv * X.t()) * Y;
  arma::mat temp2 = ydot.t() - Y.t() * XtXX_inv * xdot.t();
  
  Function nlminb("nlminb");
  double gamma_start = 0.5;
  
  arma::mat XdtXXXd_inv = xdot * tXX_inv * xdot.t();
  List result = nlminb(gamma_start, 
                       Rcpp::Named("objective") = Rcpp::InternalFunction(&wrapped_oREMLfun),
                       Rcpp::Named("lower") = 0.0, 
                       Rcpp::Named("upper") = 10.0,
                       Rcpp::Named("ni") = ni, 
                       Rcpp::Named("X") = X, 
                       Rcpp::Named("Y") = Y,
                       Rcpp::Named("xdot") = xdot, 
                       Rcpp::Named("ydot") = ydot, 
                       Rcpp::Named("XdtXXXd_inv") = XdtXXXd_inv,
                       Rcpp::Named("temp1") = temp1, 
                       Rcpp::Named("temp2") = temp2);
  
  double ghat = as<double>(result["par"]);
  
  
  arma::mat tempg = diagmat(ni + 1.0 / ghat);
  arma::mat temp6 = inv(tempg - XdtXXXd_inv);
  double sige2 = as_scalar((temp1 - temp2 * temp6 * temp2.t()) / (n - p));
  double sigv2 = ghat * sige2;
  
  
  arma::vec a = ghat / (1 + ni * ghat);
  arma::mat amata = diagmat(a);
  arma::vec bhat = inv(tXX - xdot.t() * amata * xdot) * (X.t() * Y - xdot.t() * amata * ydot);
  
  
  return List::create(Named("bhat") = bhat, 
                      Named("sigehat2") = sige2, 
                      Named("sigvhat2") = sigv2);
}


List varnerEB(arma::vec ni, arma::mat X, arma::vec Y){
  // calculate the empirical bayesian estimator for NER model
  int m = ni.size(); int p = X.n_cols; int n = sum(ni);
  List MeanXY = MeanFun(ni, X, Y);
  arma::mat xbar = MeanXY["xbar"]; arma::vec ybar = MeanXY["ybar"];
  arma::mat xdot = MeanXY["xdot"]; arma::vec ydot = MeanXY["ydot"];
  arma::vec Yseq(n); arma::mat Xseq = arma::ones(n, p);
  int ii = 0;
  for(int i = 0; i<m; i++){
    for(int j=0; j<ni(i); j++){
      Yseq(ii) = Y(ii) - ybar(i);
      Xseq.row(ii) = X.row(ii) - xbar.row(i);
      ii = ii + 1;
    }
  }
  Xseq.col(0) = arma::ones(n, 1);
  arma::vec betaols = inv(Xseq.t() * Xseq) * Xseq .t() * Yseq;
  arma::vec ehat = Yseq - Xseq * betaols;
  double sige2 = as_scalar(ehat.t() * ehat /(n - m -2));
  arma::vec b(m); arma::vec u_delta(m); arma::vec d(m);
  arma::mat tXX = X.t() * X;
  arma::mat tXY = X.t() * Y;
  arma::mat tXX_inv = inv(tXX);
  arma::mat ni2mat = diagmat(pow(ni,2));
  for(int i=0; i<m; i++){
    u_delta(i) = as_scalar(ybar(i) - xbar.row(i) * tXX_inv * tXY);
    b(i) = as_scalar((1 - 2 * ni(i) * xbar.row(i) * tXX_inv * xbar.row(i).t() + xbar.row(i) *
      tXX_inv  * (xbar.t() * ni2mat * xbar) * tXX_inv * xbar.row(i).t()));
    d(i) = as_scalar((1 - ni(i) * xbar.row(i) * tXX_inv * xbar.row(i).t())/ni(i));
  }
  double temphat = sum(ni % b);
  double  sigv2 = sum(ni % pow(u_delta,2))/temphat - sum(ni % d)/ temphat * sige2;
  if(sigv2 < 0){sigv2 = 0;}
  arma::mat mata = diagmat(sigv2/(sige2 + ni * sigv2));
  arma::vec bhat = inv(tXX - xdot.t() * mata * xdot) * (tXY - xdot.t() * mata * ydot);
  return List::create(Named("bhat") = bhat, Named("sigehat2") = sige2, Named("sigvhat2") = sigv2);
}

List varner(arma::vec ni, arma::mat X, arma::vec Y, int method){
  // main variance estimation function for NER model
  if (X.n_cols <= 0) {
    Rcpp::stop("X must have at least one column.");
  }
  if (!arma::all(ni > 0)) {
    Rcpp::stop("All elements of ni must be greater than 0.");
  }
  if (method < 1 || method > 4) {
    Rcpp::stop("Method must be between 1 and 4.");
  }
  
  List varEST;
  if (method == 1){
    varEST = varnerMOM(ni, X, Y);
  }
  if (method == 2){
    varEST = varnerREML(ni, X, Y);
  }
  if (method == 3){
    varEST = varnerML(ni, X, Y);
  }
  if (method == 4){
    varEST = varnerEB(ni, X, Y);
  }
  return varEST;
}

// [[Rcpp::export]]
List varner(arma::vec ni, Rcpp::Formula formula, Rcpp::DataFrame data, int method, bool na_rm = false, bool na_omit = false){
  // main variance estimation function for NER model
  BaseModel model;
  List parsedData = model.parseFormula(formula, data);
  arma::mat X = parsedData["X"];
  arma::vec Y = parsedData["Y"];
  
  model.handle_missing_data(X, Y, ni, na_rm, na_omit);
  
  // check arguments
  if (X.n_cols <= 0) {
    Rcpp::stop("X must have at least one column.");
  }
  if (method < 1 || method > 4) {
    Rcpp::stop("Method must be between 1 and 4.");
  }
  
  List varEST;
  if (method == 1){
    varEST = varnerMOM(ni, X, Y);
  }
  if (method == 2){
    varEST = varnerREML(ni, X, Y);
  }
  if (method == 3){
    varEST = varnerML(ni, X, Y);
  }
  if (method == 4){
    varEST = varnerEB(ni, X, Y);
  }
  return varEST;
}

// [[Rcpp::export]]
List mspeNERjack(arma::vec ni, Rcpp::Formula formula, Rcpp::DataFrame data, arma::mat Xmean, int method = 1, bool na_rm = false, bool na_omit = false){
  // main funtion of jackknife MSPE estimation method for NER model
  BaseModel model;
  List parsedData = model.parseFormula(formula, data);
  arma::mat X = parsedData["X"];
  arma::vec Y = parsedData["Y"];
  model.handle_missing_data(X, Y, ni, Xmean, na_rm, na_omit);
  
  // check arguments
  if (method < 1 || method > 3) {
    Rcpp::stop("Method must be between 1 and 3.");
  }
  
  int m = ni.size(); arma::vec msetheta(m);
  List MeanXY = MeanFun(ni, X, Y);
  arma::mat xbar = MeanXY["xbar"]; arma::vec ybar = MeanXY["ybar"];
  if(method == 4){
    return 0;
  }else {
    List phat = varner(ni, X, Y, method);
    arma::vec bhat = phat["bhat"]; double sige2 = phat["sigehat2"]; double sigv2 = phat["sigvhat2"];
    arma::vec Bi = sige2/(sige2 + ni * sigv2); arma::vec bi = sigv2 * Bi;
    arma::vec thetahat = Xmean * bhat + (1 - Bi) % (ybar - xbar * bhat);
    for(int i = 0; i < m; i++){
      arma::vec biu(m); arma::vec thetahatu(m);
      for(int j = 0; j < m; j++){
        int nstart = sum(ni(arma::span(0,j))) - ni(j);
        arma::mat Xu = X; Xu.shed_rows(nstart, nstart + ni(j) - 1);
        arma::vec Yu = Y; Yu.shed_rows(nstart, nstart + ni(j) - 1);
        arma::vec niu = ni; niu.shed_rows(j, j);
        List phatu = varner(niu, Xu, Yu, method);
        arma::vec bhatu = phatu["bhat"]; double sige2u = phatu["sigehat2"]; double sigv2u = phatu["sigvhat2"];
        double Biu = sige2u/(sige2u + ni(i) * sigv2u); biu(j) = sigv2u* Biu;
        thetahatu(j) = as_scalar(Xmean.row(i) * bhatu + (1 - Biu)*(ybar(i) - xbar.row(i) * bhatu));
      }
      msetheta(i) = bi(i) - (m - 1) * sum(biu - bi(i))/m + (m - 1) * sum(pow(thetahatu - thetahat(i),2))/m;
    }
    return List::create(Named("MSPE") = msetheta, Named("bhat") = bhat, Named("sigvhat2") = sigv2, Named("sigehat2") = sige2);
  }
}

// [[Rcpp::export]]
List mspeNERpb(arma::vec ni, Rcpp::Formula formula, Rcpp::DataFrame data, arma::mat Xmean, int K = 50, int method = 4, bool na_rm = false, bool na_omit = false){
  // main function of parameter bootstrap MSPE estimation method for NER model
  BaseModel model;
  List parsedData = model.parseFormula(formula, data);
  arma::mat X = parsedData["X"];
  arma::vec Y = parsedData["Y"];
  model.handle_missing_data(X, Y, ni, Xmean, na_rm, na_omit);
  
  // check arguments
  if (K <= 0) {
    Rcpp::stop("K must be greater than 0.");
  }
  
  if (method != 4) {
    Rcpp::stop("Only Empirical Bayesian method is avaiable");
  }
  
  int m = ni.size(); int n = sum(ni); int p = X.n_cols;
  List MeanXY = MeanFun(ni, X, Y);
  arma::mat ybar = MeanXY["ybar"]; arma::mat xbar = MeanXY["xbar"];
  List phat = varner(ni, X, Y, method);
  arma::vec bhat = phat["bhat"]; double sige2 = phat["sigehat2"]; double sigv2 = phat["sigvhat2"];
  arma::mat V = arma::zeros(n, n); arma::mat Vtemp(n,n); 
  arma::mat XtVX = arma::zeros(p, p);
  arma::mat InvXtVX(p, p);
  
  for(int i = 0; i < m; i++){
    arma::mat Vtemp = arma::eye(ni(i),ni(i))/sige2 - sigv2/((ni(i)*sigv2+sige2)*sige2) * arma::ones(ni(i),ni(i));
    int nstart = sum(ni(arma::span(0,i))) - ni(i);
    V.submat(arma::span(nstart, nstart + ni(i) - 1), arma::span(nstart, nstart + ni(i) - 1)) = Vtemp;
    XtVX = XtVX + X.rows(nstart, nstart + ni(i) - 1).t() * Vtemp * X.rows(nstart, nstart + ni(i) - 1);
  }
  arma::vec gama = ni * sigv2 / (ni * sigv2 + sige2);
  arma::vec g1 = (1 - gama) * sigv2;
  arma::vec uhat = ybar - xbar * bhat;
  arma::vec ghat = sigv2/(sigv2 + sige2/ni);
  arma::vec g2(m), theta(m);
  InvXtVX = inv(XtVX);
  for(int i = 0; i < m; i++){
    g2(i) = as_scalar((Xmean.row(i) - gama(i) * xbar.row(i)) * InvXtVX * (Xmean.row(i) - gama(i) * xbar.row(i)).t()) ;
    theta(i) = as_scalar(Xmean.row(i) * bhat + uhat(i) * ghat(i));
  }
  
  arma::mat g1star(m, K); arma::mat g2star(m, K); arma::mat thetastar(m, K);
  arma::mat Xbhat = X * bhat;
  
  arma::mat vi(m,1);
  arma::vec vistar(n); 
  int vv = 0;
  arma::mat Yk(n,1);
  List phatk;
  arma::vec gamak(m), g1k(m), uhatk(m), ghatk(m), g2k(m), thetak(m), mspe(m);
  
  
  for(int k=0; k<K; k++){
    vi = pow(sigv2, 0.5) * arma::randn(m,1);
    vv = 0;
    for(int v=0; v<m; v++){
      for(int ii=0; ii<ni(v); ii++){
        vistar(vv) = vi(v);
        vv = vv+1;
      }
    }
    Yk = Xbhat + vistar + pow(sige2, 0.5) * arma::randn(n,1);
    
    phatk = varner(ni, X, Yk, method);
    arma::vec bhatk = phatk["bhat"]; double sige2k = phatk["sigehat2"]; double sigv2k = phatk["sigvhat2"];
    
    for(int i = 0; i < m; i++){
      Vtemp = arma::eye(ni(i),ni(i))/sige2k - sigv2k/((ni(i)*sigv2k+sige2k)*sige2k) * arma::ones(ni(i),ni(i));
      int nstart = sum(ni(arma::span(0,i))) - ni(i);
      V.submat(arma::span(nstart, nstart + ni(i) - 1), arma::span(nstart, nstart + ni(i) - 1)) = Vtemp;
      XtVX = XtVX + X.rows(nstart, nstart + ni(i) - 1).t() * Vtemp * X.rows(nstart, nstart + ni(i) - 1);
    }
    
    gamak = ni * sigv2k / (ni * sigv2k + sige2k);
    g1k = (1 - gamak) * sigv2k;
    uhatk = ybar - xbar * bhatk;
    ghatk = sigv2k/(sigv2k + sige2k/ni);
    InvXtVX = inv(XtVX);
    for(int i = 0; i < m; i++){
      g2k(i) = as_scalar((Xmean.row(i) - gamak(i) * xbar.row(i)) * InvXtVX * (Xmean.row(i) - gamak(i) * xbar.row(i)).t()) ;
      thetak(i) = as_scalar(Xmean.row(i) * bhatk + uhatk(i) * ghatk(i));
    }
    g1star.col(k) = g1k;
    g2star.col(k) = g2k;
    thetastar.col(k) = thetak;
  }
  mspe = 2*(g1 + g2) - mean(g1star, 1) - mean(g2star, 1) + mean(thetastar, 1) - theta;
  return List::create(Named("MSPE") = mspe, Named("bhat") = bhat, Named("sigvhat2") = sigv2, Named("sigehat2") = sige2);
}

List dbootstrap(arma::vec ni, arma::mat X, arma::vec Y, arma::mat xbar, arma::mat Xmean, int method){
  // sub-function for "mspeNERdb()"
  int m = ni.size(); arma::vec ybar(m);
  for(int i = 0; i < m; i++){
    int nstart = sum(ni(arma::span(0,i))) - ni(i);
    if(ni(i) == 1){
      ybar(i) = Y(nstart);
    } else {
      ybar(i) = mean(Y(arma::span(nstart, nstart + ni(i) - 1)));
    }
  }
  List temp = varner(ni, X, Y, method);
  arma::vec bhat = temp["bhat"]; double sige2 = temp["sigehat2"]; double sigv2 = temp["sigvhat2"];
  arma::vec theta(m);
  for(int i = 0; i < m; i++){
    theta(i) = as_scalar(Xmean.row(i) * bhat) + (ybar(i) - as_scalar(xbar.row(i) * bhat)) * ni(i) * sigv2/(sige2+sigv2*ni(i));
  }
  return List::create(Named("theta") = theta, Named("bhat") = bhat, Named("sigvhat2") = sigv2, Named("sigehat2") = sige2);
}

// [[Rcpp::export]]
List mspeNERdb(arma::vec ni, Rcpp::Formula formula, Rcpp::DataFrame data, arma::mat Xmean, int K = 50, int C = 50, int method = 1, bool na_rm = false, bool na_omit = false){
  // main funtion of double bootstrap MSPE estimation method for NER model
  BaseModel model;
  List parsedData = model.parseFormula(formula, data);
  arma::mat X = parsedData["X"];
  arma::vec Y = parsedData["Y"];
  model.handle_missing_data(X, Y, ni, Xmean, na_rm, na_omit);
  // check arguments
  if (K <= 0) {
    Rcpp::stop("K must be greater than 0.");
  }
  if (C <= 0) {
    Rcpp::stop("C must be greater than 0.");
  }
  
  int m = ni.size(); int n = sum(ni);
  // check arguments
  if (method < 1 || method > 3) {
    Rcpp::stop("Method must be between 1 and 3.");
  }
  List phat = varner(ni, X, Y, method);
  arma::vec bhat = phat["bhat"]; double sige2 = phat["sigehat2"]; double sigv2 = phat["sigvhat2"];
  List MeanXY = MeanFun(ni, X, Y);
  arma::mat ybar = MeanXY["ybar"]; arma::mat xbar = MeanXY["xbar"];
  arma::mat umat = arma::zeros(m, K); arma::mat vmat = arma::zeros(m, K); arma::mat tempmat = arma::zeros(m, C);
  arma::mat Xbhat = X * bhat;
  for(int k=0; k<K; k++){
    // first bootstrap procedure
    arma::vec vi = pow(sigv2, 0.5) * arma::randn(m,1);
    arma::vec vistar(n);
    int vv = 0;
    for(int v=0; v<m; v++){
      for(int ii=0; ii<ni(v); ii++){
        vistar(vv) = vi(v);
        vv = vv+1;
      }
    }
    arma::vec thetak = Xmean * bhat + vi;
    arma::vec Yk = Xbhat + vistar + pow(sige2, 0.5) * arma::randn(n,1);
    List temp = dbootstrap(ni, X, Yk, xbar, Xmean, method);
    arma::vec temptheta = temp["theta"];
    arma::vec bhatc = temp["bhat"];
    double sige2hat = temp["sigehat2"];
    double sigv2hat = temp["sigvhat2"];
    umat.col(k) = pow(temptheta - thetak, 2);
    arma::vec thetac(m);
    for(int c=0; c<C; c++){
      // second bootstrap procedure
      arma::mat vik = pow(sigv2hat, 0.5) * arma::randn(m,1);
      arma::vec vikstar(n);
      int vv = 0;
      for(int v=0; v<m; v++){
        for(int ii=0; ii<ni(v); ii++){
          vikstar(vv) = vik(v);
          vv = vv+1;
        }
      }
      thetac = Xmean * bhatc + vik;
      arma::vec Ykc = X * bhatc + vikstar + pow(sige2hat, 0.5) * arma::randn(n,1);
      List temp = dbootstrap(ni, X, Ykc, xbar, Xmean, method);
      arma::vec temptheta1 = temp["theta"];
      tempmat.col(c) = pow(temptheta1 - thetac, 2);
    }
    vmat.col(k) = mean(thetac,1);
  }
  arma::vec u = mean(umat,1);
  arma::vec v = mean(vmat,1);
  arma::vec mspe_DB(m);
  for(int ii=0; ii<m; ii++){
    if(u(ii) >= v(ii)){
      mspe_DB(ii) = u(ii) + atan(m * (u(ii) - v(ii)))/m;
    }else{
      mspe_DB(ii) = pow(u(ii),2)/(u(ii) + atan(m * (v(ii) - u(ii)))/m);
    }
  }
  return List::create(Named("MSPE") = mspe_DB, Named("bhat") = bhat, Named("sigvhat2") = sigv2, Named("sigehat2") = sige2);
  
}

// [[Rcpp::export]]
List mspeNERsumca(arma::vec ni, Rcpp::Formula formula, Rcpp::DataFrame data, arma::mat Xmean, int K = 50, int method = 1, bool na_rm = false, bool na_omit = false){
  // main funtion of Sumca MSPE estimation method for NER model
  BaseModel model;
  List parsedData = model.parseFormula(formula, data);
  arma::mat X = parsedData["X"];
  arma::vec Y = parsedData["Y"];
  model.handle_missing_data(X, Y, ni, Xmean, na_rm, na_omit);
  // check arguments
  if (K <= 0) {
    Rcpp::stop("K must be greater than 0.");
  }
  
  if (method < 1 || method > 3) {
    Rcpp::stop("Method must be between 1 and 3.");
  }
  
  int m = ni.size(); int n = sum(ni);
  
  List phat = varner(ni, X, Y, method);
  arma::vec bhat = phat["bhat"]; double sige2 = phat["sigehat2"]; double sigv2 = phat["sigvhat2"];
  arma::vec ay = (sige2 * sigv2)/(ni * sigv2 + sige2);
  arma::mat ayk = arma::zeros(m, K);
  for(int k=0;k<K;k++){
    arma::vec Yk(n); int count=0; int kk=0;
    arma::vec v = arma::randn(m,1)*sqrt(sigv2);
    for(int i=0;i<m;i++){
      count++; double nii = ni(i);
      for(int j=0;j< nii;j++){
        Yk(kk)= arma::as_scalar(X.row(kk) * bhat) + v(i) + arma::as_scalar(arma::randn(1,1))*sqrt(sige2);
        kk++;
      }
    }
    List phatk = varner(ni, X, Yk, method);
    double sige2k = phatk("sigehat2");
    double sigv2k = phatk("sigvhat2");
    ayk.col(k) = (sige2k * sigv2k)/(ni * sigv2k + sige2k);
  }
  arma::vec mseNER = 2*ay - mean(ayk,1);
  return List::create(Named("MSPE") = mseNER, Named("bhat") = bhat, Named("sigvhat2") = sigv2, Named("sigehat2") = sige2);
  
}
