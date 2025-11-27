

// #define RCPP_ARMADILLO_RETURN_COLVEC_AS_VECTOR
#include <RcppArmadillo.h>
#include <string>
using namespace Rcpp;


// [[Rcpp::export]]

arma::mat get_sigma(arma::mat x,arma::vec curve,arma::vec time, arma::mat S, arma::mat piigivej, arma::mat gcov,arma::vec n_i,arma::cube gamma,arma::mat mu){
  arma::mat Si;
  int N = piigivej.n_rows;
  int K = piigivej.n_cols;
  int q = gcov.n_rows;
  arma::vec n_vec(n_i.n_elem+1);
  n_vec(0)=-1;
  arma::vec rep1(n_i.n_elem);
  rep1.ones();
  n_vec.subvec(1,n_i.n_elem)=cumsum(n_i)-rep1;
  mu=trans(mu);
  arma::mat sigma(1,1);
  sigma(0,0)=0;
  for (int i=0;i<N;++i){
    arma::mat pro= x.rows((n_vec(i))+1,(n_vec(i+1)));
    arma::mat y = pro;
    Si= S.rows((n_vec(i))+1,(n_vec(i+1)));
    for (int j=0;j<K;++j){
      arma::mat pp=gamma.tube(i,j);
      sigma(0,0)= sigma(0,0)+piigivej(i,j)*(dot(y-Si*(mu.col(j))-Si*trans(pp),y-Si*(mu.col(j))-Si*trans(pp))+trace(Si*gcov.cols((i)*(q),((i+1)*(q)-1))*trans(Si)));
    }

  }

  return(sigma);
}

// [[Rcpp::export]]

List get_numden(arma::mat x,arma::vec curve,arma::vec time, arma::mat S, arma::mat piigivej, arma::mat gcov,arma::vec n_i,arma::cube gamma){

  int N = piigivej.n_rows;
  int K = piigivej.n_cols;
  int q = gcov.n_rows;
  arma::mat   sigma(1,1), gammai_vec(K*q,1),y ,sum_num(K*q,1), pij_j(1,1),pp, pro;
  arma::vec n_vec(n_i.n_elem+1),rep1(n_i.n_elem),rep2(n_i.n_elem),pii_vec(K*q);
  arma::mat Si,matp(K*q,K*q),sum_den(K*q,K*q);


  matp.eye();
  rep1.ones();
  rep1.ones();
  n_vec(0)=-1;
  n_vec.subvec(1,n_i.n_elem)=cumsum(n_i)-rep1;

  sigma(0,0)=0;
  sum_num.zeros();
  sum_den.zeros();

  for (int i=0;i<N;++i){
    pro= x.rows((n_vec(i))+1,(n_vec(i+1)));
    y = repmat(pro,K,1);
    Si= S.rows((n_vec(i))+1,(n_vec(i+1)));
    arma::mat Si_mat(n_i(i)*K,K*q) ;
    Si_mat.zeros();
    for(int j=0;j<K;++j){
      Si_mat.submat(j*n_i(i),j*q,(j+1)*n_i(i)-1,(j+1)*q-1)= Si;
      pij_j=piigivej(i,j);
      pii_vec.subvec(j*q,(j+1)*q-1)=repmat(pij_j,q,1);

    }
    pp=gamma.row(i);
    gammai_vec=vectorise(trans(pp));
    sum_num=sum_num + pii_vec%(trans(Si_mat)*y-(trans(Si_mat)*(Si_mat))*gammai_vec);
    matp=diagmat(pii_vec);
    sum_den=sum_den+  matp*(trans(Si_mat)*(Si_mat));

  }
  List out=List::create(sum_num,sum_den);

  return(out);
}




// [[Rcpp::export]]

List get_Estep(List par,List data,List vars, arma::mat S, bool hard,arma::vec n_i){


  int m;
  arma::cube gamma=vars[0];
  int N = gamma.n_rows;
  int K = gamma.n_cols;
  int q = gamma.n_slices;
  arma::mat x=data[0];
  arma::mat pi=par[2];
  arma::mat piigivej=vars[1];
  arma::vec rep1(n_i.n_elem);
  arma::mat gprod(q,q*N),gcov(q,q*N),y,centx,yrep,covx,diag2,d,gprodi,gamma_row,pirep;
  arma::vec n_vec(n_i.n_elem+1);
  arma::mat Si;
  arma::mat Gamma=par[3];
  arma::mat Cgamma=Gamma;
  Cgamma.zeros();
  arma::mat mu(q,K);
  arma::mat mu1=par[0];
  mu=trans(mu1);
  rep1.ones();
  n_vec(0)=-1;
  n_vec.subvec(1,n_i.n_elem)=cumsum(n_i)-rep1;
  arma::mat sigma=par[1];

  for (int i=0;i<N;++i){
    arma::mat invar(n_i(i),n_i(i)),rep2(n_i(i),n_i(i));
    rep2.eye();
    Si= S.rows((n_vec(i))+1,(n_vec(i+1)));
    y= x.rows((n_vec(i))+1,(n_vec(i+1)));
    yrep=repmat(y,1,K);
    invar=diagmat(1/repmat(sigma,n_i(i),1));
    Cgamma = Gamma - Gamma * trans(Si) * inv_sympd(rep2 + Si * Gamma* trans(Si)* invar) * invar * Si * Gamma;
    centx = yrep - Si * mu;
    gamma.row(i)= trans(Cgamma * trans(Si) * invar * centx);
    covx=Si * Gamma * trans(Si) + inv_sympd(invar);
    diag2=(trans(centx) * inv_sympd(covx) * centx);
    d = exp( - diag2.diag()/2) % pi;
    if(d.is_zero()){
      d(0)=1e-200;
    }
    piigivej.row(i) = trans(d)/accu(d);
    if(hard) {
      m = d.index_max();
      piigivej.row(i).zeros();
      piigivej(i,m)=1;
    }
    gamma_row=gamma.row(i);
    pirep=repmat(piigivej.row(i),q,1);
    gprodi =  trans(gamma_row) * (gamma_row % trans(pirep)) + Cgamma;
    gprod.submat(0,i*q,q-1,(i+1)*q-1)=gprodi;
    gcov.submat(0,i*q,q-1,(i+1)*q-1)=Cgamma;

  }


  List out=List::create(gamma,piigivej,gprod,gcov);
  return(out);
}

// // [[Rcpp::export]]
//
// arma::mat norm_fdata_c(List v){
//
//
//   arma::mat data = v[0];
//   arma::vec grid=v[1];
//   arma::mat data_2=data%data;
//   arma::mat integrale=trapz(grid,trans(data_2));
//   arma::mat norm =sqrt(integrale);
//   return(norm);
//
//
// }
//
// // [[Rcpp::export]]
// double sum_mat(arma::mat x,arma::mat y,arma::mat z){
//   double sum=accu(diagvec(x * y * z));
//   return(sum);
//
//
// }
//
// // [[Rcpp::export]]
// arma::mat wfun_c(arma::mat x,int k,double ktun){
//   arma::mat ww;
//   if (k == 1){
//     ww=square(1 - square(x/ktun))%(abs(x) <= ktun);
//   }
//   if(k==2){
//     ww = (abs(x) <= ktun) + (abs(x) > ktun)%(ktun/(abs(x) + 1e-20));
//   }
//   if(k==5){
//     ww = 1/(abs(x) + 1e-10);
//   }
//   arma::mat out= ww;
//   return out;
// }
//
// // [[Rcpp::export]]
// List sum_fdata_c(List x){
//   List out(clone(x));
//   arma::mat tmp = x[0];
//   arma::mat tmp2 =sum(tmp,0);
//   out[0] = tmp2;
//   return out;
// }
//
// // [[Rcpp::export]]
// List div_fdata_c(List x,double k){
//   List out(clone(x));
//   arma::mat tmp = x[0];
//   arma::mat tmp2 =tmp/k;
//   out[0] = tmp2;
//   return out;
// }
//
//
// // [[Rcpp::export]]
// List stdandar(List x,List mu,List sig){
//   List out(clone(x));
//   arma::mat tmp1 = x[0];
//   int d1=tmp1.n_rows;
//
//   arma::rowvec mu_vec = mu[0];
//   arma::mat tmp2=repmat( mu_vec, d1, 1 );
//   arma::rowvec sig_vec = sig[0];
//   arma::mat tmp3=repmat( sig_vec, d1, 1 );
//   arma::mat sum=(tmp1-tmp2)/tmp3;
//   out[0] = sum;
//   return out;
//
// }
//
// // [[Rcpp::export]]
// arma::mat dife( arma::mat resi_new, arma::mat resi){
//   arma::mat out=abs(mean(resi_new,0) -mean(resi,0))/mean(resi,0);
//   return out;
//
// }
//
// // [[Rcpp::export]]
// List iteration(List x,List mu0,List sig0,double kpsi,double ktun,double tol, int maxit){
//   arma::mat  dife = {1e+10};
//   arma::mat  tol_mat= {tol};
//   int iter = 0;
//   arma::mat resi;
//   arma::mat resi_new;
//   arma::mat ww;
//   arma::mat data=x[0];
//   List data_std;
//
//   List mu_it;
//   List somma_fdata;
//
//   double sum_ww;
//   while ((dife(0) > tol_mat(0)) & (iter < maxit)) {
//     ++iter;
//     data_std=stdandar(x,mu0,sig0);
//     resi= norm_fdata_c(data_std);
//     ww = wfun_c(resi, kpsi,ktun);
//     List prodotto(clone(x));
//     prodotto[0]=diagmat(ww)*data;
//     sum_ww=accu(ww);
//     somma_fdata=sum_fdata_c(prodotto);
//     mu_it = div_fdata_c(somma_fdata,sum_ww);
//     data_std=stdandar(x,mu_it,sig0);
//     resi_new =norm_fdata_c(data_std);
//     dife = abs(mean(resi_new,1) -mean(resi,1))/mean(resi,1);
//     mu0=mu_it;
//   }
//   List out(clone(mu_it));
//   return out;
// }
//
//
//
// // [[Rcpp::export]]
//
// arma::mat Mwgt_r(arma::mat x,arma::mat cc, Rcpp::StringVector  family){
//   // Obtain environment containing function
//   Rcpp::Environment base("package:robustbase");
//   Rcpp::Function Mwgt_ri = base["Mwgt"];
//   arma::mat out=Rcpp :: as < arma :: mat >(Mwgt_ri(x,cc,family));
//   return out;
// }
// // [[Rcpp::export]]
// List iteration_ho(List x,List mu0,List sig0,arma::mat cc,Rcpp::StringVector family,double tol, int maxit){
//   arma::mat  dife = {1e+10};
//   arma::mat  tol_mat= {tol};
//   int iter = 0;
//   arma::mat resi;
//   arma::mat resi_new;
//   arma::mat ww;
//   arma::mat data=x[0];
//   List data_std;
//
//   List mu_it;
//   List somma_fdata;
//
//   double sum_ww;
//   while ((dife(0) > tol_mat(0)) & (iter < maxit)) {
//     // Rcout<<iter<<"\n";
//     ++iter;
//     data_std=stdandar(x,mu0,sig0);
//     resi= norm_fdata_c(data_std);
//     ww = Mwgt_r(resi, cc,family);
//     List prodotto(clone(x));
//     prodotto[0]=diagmat(ww)*data;
//     sum_ww=accu(ww);
//     somma_fdata=sum_fdata_c(prodotto);
//     mu_it = div_fdata_c(somma_fdata,sum_ww);
//     data_std=stdandar(x,mu_it,sig0);
//     resi_new =norm_fdata_c(data_std);
//     dife = abs(mean(resi_new,1) -mean(resi,1))/mean(resi,1);
//     mu0=mu_it;
//   }
//   List out(clone(mu_it));
//   return out;
// }
//
