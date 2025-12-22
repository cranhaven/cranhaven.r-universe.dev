
// #define RCPP_ARMADILLO_RETURN_COLVEC_AS_VECTOR
#include <RcppArmadillo.h>
#include <string>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat norm_fdata_c(List v){
  arma::mat data = v[0];
  arma::vec grid=v[1];
  arma::mat data_2=data%data;
  arma::mat integrale=trapz(grid,trans(data_2));
  arma::mat norm =sqrt(integrale);
  return(norm);
}
// [[Rcpp::export]]
arma::mat norm_fdata_c_sur(List v){
  arma::cube data = v[0];
  int n=data.n_rows;
  List grid=v[1];
  arma::vec grid_s=grid[0];
  arma::vec grid_t=grid[1];
  double n_points_t=(grid_t.n_rows);
  double delta_t=1/(n_points_t-1);
  double n_points_s=(grid_s.n_rows);
  double delta_s=1/(n_points_s-1);
  arma::mat integrale(n,1);
  for (int i = 0; i < n; i++) {
    arma::mat A=data.row(i);
    integrale(i,0)=sqrt(delta_t*delta_s*accu(A%A));
  }
  return(integrale);
}
// [[Rcpp::export]]
arma::mat wfun_c(arma::mat x,int k,double ktun){
  arma::mat ww;
  if (k == 1){
    ww=square(1 - square(x/ktun))%(abs(x) <= ktun);
  }
  if(k==2){
    ww = (abs(x) <= ktun) + (abs(x) > ktun)%(ktun/(abs(x) + 1e-20));
  }
  if(k==5){
    ww = 1/(abs(x) + 1e-10);
  }
  arma::mat out= ww;
  return out;
}
// [[Rcpp::export]]
List sum_fdata_c(List x){
  List out(clone(x));
  arma::mat tmp = x[0];
  arma::mat tmp2 =sum(tmp,0);
  out[0] = tmp2;
  return out;
}
// [[Rcpp::export]]
List sum_fdata_c_sur(List x){
  arma::cube data = x[0];
  int n=data.n_rows;
  int n_row=data.n_cols;
  int n_col=data.n_slices;
  List out(clone(x));
  arma::cube tmp2(1,n_row,n_col);
  tmp2.row(0)=data.row(0);
  arma::cube tmp = x[0];
  for (int i = 1; i < n; i++) {
    tmp2.row(0) =tmp2.row(0)+data.row(i);
  }
  out[0] = tmp2;
  return out;
}
// [[Rcpp::export]]
List div_fdata_c(List x,double k){
  List out(clone(x));
  arma::mat tmp = x[0];
  arma::mat tmp2 =tmp/k;
  out[0] = tmp2;
  return out;
}
// [[Rcpp::export]]
List div_fdata_c_sur(List x,double k){
  List out(clone(x));
  arma::cube tmp = x[0];
  arma::cube tmp2 =tmp/k;
  out[0] = tmp2;
  return out;
}
// [[Rcpp::export]]
List stdandar(List x,List mu,List sig){
  List out(clone(x));
  arma::mat tmp1 = x[0];
  int d1=tmp1.n_rows;
  arma::rowvec mu_vec = mu[0];
  arma::mat tmp2=repmat( mu_vec, d1, 1 );
  arma::rowvec sig_vec = sig[0];
  arma::mat tmp3=repmat( sig_vec, d1, 1 );
  arma::mat sum=(tmp1-tmp2)/tmp3;
  out[0] = sum;
  return out;
}
// [[Rcpp::export]]
List stdandar_sur(List x,List mu,List sig){
  arma::cube data = x[0];
  arma::cube data_mu = mu[0];
  arma::cube data_sig = sig[0];
  int n=data.n_rows;
  int n_row=data.n_cols;
  int n_col=data.n_slices;
  List out(clone(x));
  arma::mat A;
  arma::cube tmp1(n,n_row,n_col);
  arma::mat mu_mat = data_mu.row(0);
  arma::mat sig_mat = data_sig.row(0);
  sig_mat.replace(0, pow(10,-20));
  for (int i = 0; i < n; i++) {
    A=data.row(i);
    tmp1.row(i)= (A-mu_mat)/sig_mat;
  }
  out[0] = tmp1;
  return out;
}
// [[Rcpp::export]]
List center_sur(List x,List mu){
  arma::cube data = x[0];
  arma::cube data_mu = mu[0];
  int n=data.n_rows;
  int n_row=data.n_cols;
  int n_col=data.n_slices;
  List out(clone(x));
  arma::cube tmp1(n,n_row,n_col);
  arma::mat mu_mat = data_mu.row(0);
  arma::mat A;
  for (int i = 0; i < n; i++) {
    A=data.row(i);
    tmp1.row(i)= (A-mu_mat);
  }
  out[0] = tmp1;
  return out;
}
// [[Rcpp::export]]
arma::mat dife( arma::mat resi_new, arma::mat resi){
  arma::mat out=abs(mean(resi_new,0) -mean(resi,0))/mean(resi,0);
  return out;
}
// [[Rcpp::export]]
List iteration(List x,List mu0,List sig0,double kpsi,double ktun,double tol, int maxit){
  arma::mat  dife = {1e+10};
  arma::mat  tol_mat= {tol};
  int iter = 0;
  arma::mat resi;
  arma::mat resi_new;
  arma::mat ww;
  arma::mat data=x[0];
  List data_std;
  List mu_it;
  List somma_fdata;
  double sum_ww;
  while ((dife(0) > tol_mat(0)) & (iter < maxit)) {
    ++iter;
    data_std=stdandar(x,mu0,sig0);
    resi= norm_fdata_c(data_std);
    ww = wfun_c(resi, kpsi,ktun);
    List prodotto(clone(x));
    prodotto[0]=diagmat(ww)*data;
    sum_ww=accu(ww);
    somma_fdata=sum_fdata_c(prodotto);
    mu_it = div_fdata_c(somma_fdata,sum_ww);
    data_std=stdandar(x,mu_it,sig0);
    resi_new =norm_fdata_c(data_std);
    dife = abs(mean(resi_new,1) -mean(resi,1))/mean(resi,1);
    mu0=mu_it;
  }
  List out(clone(mu_it));
  return out;
}
// [[Rcpp::export]]
List iteration_sur(List x,List mu0,List sig0,int kpsi,double ktun,double tol, int maxit){
  arma::cube data = x[0];
  int n=data.n_rows;
  int n_row=data.n_cols;
  int n_col=data.n_slices;
  arma::mat  dife = {1e+10};
  arma::mat  tol_mat= {tol};
  int iter = 0;
  arma::mat resi;
  arma::mat resi_new;
  arma::mat ww(n,1);
  List data_std;
  List prodotto(clone(x));
  arma::cube data_prodotto(n,n_row,n_col);
  List mu_it;
  List somma_fdata;
  double sum_ww;
  while ((dife(0) > tol_mat(0)) & (iter < maxit)) {
    ++iter;
    data_std=stdandar_sur(x,mu0,sig0);
    resi= norm_fdata_c_sur(data_std);
    ww = wfun_c(resi, kpsi,ktun);
    for (int i = 0; i < n; i++) {
      data_prodotto.row(i)= ww(i,0)*data.row(i);
    }
    sum_ww=accu(ww);
    prodotto[0]=data_prodotto;
    somma_fdata=sum_fdata_c_sur(prodotto);
    mu_it = div_fdata_c_sur(somma_fdata,sum_ww);
    data_std=stdandar_sur(x,mu_it,sig0);
    resi_new =norm_fdata_c_sur(data_std);
    dife = abs(mean(resi_new,1) -mean(resi,1))/mean(resi,1);
    mu0=mu_it;
  }
  List out(clone(mu_it));
  return out;
}
// [[Rcpp::export]]
arma::mat Mwgt_r(arma::mat x,arma::mat cc, Rcpp::StringVector  family){
  Rcpp::Environment base("package:robustbase");
  Rcpp::Function Mwgt_ri = base["Mwgt"];
  arma::mat out=Rcpp :: as < arma :: mat >(Mwgt_ri(x,cc,family));
  return out;
}
// [[Rcpp::export]]
List iteration_ho(List x,List mu0,List sig0,arma::mat cc,Rcpp::StringVector family,double tol, int maxit){
  arma::mat  dife = {1e+10};
  arma::mat  tol_mat= {tol};
  int iter = 0;
  arma::mat resi;
  arma::mat resi_new;
  arma::mat ww;
  arma::mat data=x[0];
  List data_std;
  List mu_it;
  List somma_fdata;
  double sum_ww;
  while ((dife(0) > tol_mat(0)) & (iter < maxit)) {
    ++iter;
    data_std=stdandar(x,mu0,sig0);
    resi= norm_fdata_c(data_std);
    ww = Mwgt_r(resi, cc,family);
    List prodotto(clone(x));
    prodotto[0]=diagmat(ww)*data;
    sum_ww=accu(ww);
    somma_fdata=sum_fdata_c(prodotto);
    mu_it = div_fdata_c(somma_fdata,sum_ww);
    data_std=stdandar(x,mu_it,sig0);
    resi_new =norm_fdata_c(data_std);
    dife = abs(mean(resi_new,1) -mean(resi,1))/mean(resi,1);
    mu0=mu_it;
  }
  List out(clone(mu_it));
  return out;
}
// [[Rcpp::export]]
List iteration_ho_sur(List x,List mu0,List sig0,arma::mat cc,Rcpp::StringVector family,double tol, int maxit){
  arma::cube data = x[0];
  int n=data.n_rows;
  int n_row=data.n_cols;
  int n_col=data.n_slices;
  arma::mat  dife = {1e+10};
  arma::mat  tol_mat= {tol};
  int iter = 0;
  arma::mat resi;
  arma::mat resi_new;
  arma::mat ww;
  List data_std;
  List prodotto(clone(x));
  arma::cube data_prodotto(n,n_row,n_col);
  List mu_it;
  List somma_fdata;
  double sum_ww;
  while ((dife(0) > tol_mat(0)) & (iter < maxit)) {
    ++iter;
    data_std=stdandar_sur(x,mu0,sig0);
    resi= norm_fdata_c_sur(data_std);
    ww = Mwgt_r(resi, cc,family);
    for (int i = 0; i < n; i++) {
      data_prodotto.row(i)= ww(i,0)*data.row(i);
    }
    sum_ww=accu(ww);
    prodotto[0]=data_prodotto;
    somma_fdata=sum_fdata_c_sur(prodotto);
    mu_it = div_fdata_c_sur(somma_fdata,sum_ww);
    data_std=stdandar_sur(x,mu_it,sig0);
    resi_new =norm_fdata_c_sur(data_std);
    dife = abs(mean(resi_new,1) -mean(resi,1))/mean(resi,1);
    mu0=mu_it;
  }
  List out(clone(mu_it));
  return out;
}
