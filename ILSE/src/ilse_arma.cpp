#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
#include <Rcpp.h>
//#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>


using namespace Rcpp;
using namespace sugar;
using namespace arma;
using namespace std;


//[[Rcpp::export]]
arma::vec kernel(const arma::vec& z, int Ktype = 1L){ // normal kernel function

  switch(Ktype){
  case 2: // ekp
    return (0.75*(1-z%z) % (z<=1 && z>=-1));
  case 3: //biweight
    return (15/16.0)* pow((1-z%z),2) % (z<=1 && z>=-1);
  case 4: // triweight
    return (35/32.0)* pow((1-z%z),3) % (z<=1 && z>=-1);
  case 5: // tricube
    return(70/81.0 * pow((1- pow(abs(z),3)), 3) % (z<=1 && z>=-1));
  case 6: // cosine
    return(datum::pi/4.0 * arma::cos(datum::pi * z / 2.0));
  case 7: // uniform
    {vec y = 0.5 * ones(size(z)) % (z<=1 && z>=-1);
      return (y);
    }
  case 8: // triangle
    return( (1- abs(z)) % (z<=1 && z>=-1));
  default: // gaussian
    return exp(-0.5 * pow(z, 2)) / sqrt(2*M_PI);
  }
}



arma::field<uvec> obsindex(arma::mat Xmis){

  // Find the observed index for each variable
  int p = Xmis.n_cols, j;
  field<uvec> indexfield(p);
  for(j= 0; j< p; ++j){
    indexfield(j) = find_finite(Xmis.col(j));
  }
  return(indexfield);
}



arma::uvec getipeer(const arma::uvec& Pi, const arma::field<uvec>& indexfield){

  // obtain the i' such that Pi \subset Pi'
  int j, m_Pi = Pi.n_elem;
  uvec ipeer  = indexfield(Pi(0));
  for(j=1; j< m_Pi; ++j){
    ipeer = intersect(ipeer, indexfield(Pi(j)));
  }
  return ipeer;
}
arma::uvec setdiff(const arma::uvec& x, const arma::uvec& y) {

  std::vector<int> a = arma::conv_to< std::vector<int> >::from(arma::sort(x));
  std::vector<int> b = arma::conv_to< std::vector<int> >::from(arma::sort(y));
  std::vector<int> out;

  std::set_difference(a.begin(), a.end(), b.begin(), b.end(),
                      std::inserter(out, out.end()));

  return arma::conv_to<arma::uvec>::from(out);
}


arma::vec kernest(const int& i, const arma::vec& beta, const arma::mat& Xmat,
                  const arma::field<uvec>& indexfield, const double& bw,
                  const int& Ktype){

  arma::vec u, ukern;
  arma::vec Zi = (Xmat.row(i)).t();
  int j;
  uvec Pi = find_finite(Zi);
  double w = as_scalar(Zi(Pi).t()* beta(Pi));
  uvec ipeer = getipeer(Pi, indexfield);
  uvec Pi_peer = find_nonfinite(Zi);
  int nP = Pi_peer.n_elem;
  //printf("na number is: %d \n", nP);
  for(j=0; j < nP; j ++){  // Only impute the missing values instead of all values.
    uvec tmpIV = indexfield(Pi_peer(j));
    uvec ij_peer = intersect(ipeer, tmpIV);
    vec hmat1 = Xmat.col(Pi_peer(j)); //
    vec hmat = hmat1(ij_peer);
    vec Xbeta = Xmat.submat(ij_peer, Pi) * beta(Pi);
    //cout<<"ij_peer: "<<ij_peer.size()<<endl;
    u = (Xbeta - w)/bw;
    ukern = kernel(u, Ktype);//
    Zi(Pi_peer(j)) = as_scalar(hmat.t()*ukern) / sum(ukern);
    if (sum(u<=1 && u>=-1)==0 && Ktype != 1L){
      Rprintf("Warning: the bandwidth is too small to lead an unreliable result, so we suggest using gaussian kernel! \n");
      Zi(Pi_peer(j)) = as_scalar(randn(1));
    }

  }

  return Zi;
}


//[[Rcpp::export]]
Rcpp::List ilse_arma(const arma::vec& Y, const arma::mat& X,const arma::vec& beta_int, const double& bw,
                     const int& Ktype, const int& maxIter=20, const double& eps_be=1e-5, const double& eps_obj = 1e-7,
                     const bool& verbose = true){


  int p = X.n_cols, n = X.n_rows;
  // obtain the observed indice for each variable.
  field<uvec> obsfield = obsindex(X);

  // obtain the observed indices of complete cases.
  uvec var_id = regspace<uvec>(0,  1,  p-1); // 1:p and remember to number from zero
  uvec n_id = regspace<uvec>(0,  1,  n-1); // 1:n

  uvec icom = getipeer(var_id, obsfield);
  uvec NA_ind = setdiff(n_id, icom);
  //Rprintf("good1 \n");

  // evaluate the initial value
  vec beta(beta_int);

  double rss_old = 1e8;
  int k = 1, iter=0;

  mat Xmat0(X);
  Xmat0.elem( find_nonfinite(Xmat0) ).zeros(); // // change non-finite elements to zero

  mat Z2(X), Z1(X);
  vec temp_Z1, beta_new, fn_seq(maxIter);
  fn_seq(0) = 1e20;
  double rss_new, d_fn, d_par;
  int nmis = NA_ind.n_elem;
  for(k = 1; k < maxIter; ++k){

    for(int ii=0; ii<nmis; ++ii){ // here can be in parallel
      temp_Z1 =kernest(NA_ind(ii), beta, X, obsfield, bw, Ktype);
      // Rprintf("Impute \n");
      Z1.row(NA_ind(ii)) = temp_Z1.t();
    }

    //Rprintf("Finish impute \n");

    Z2(find_nonfinite(X)) = Z1(find_nonfinite(X));
    // update beta
    beta_new = (Z2.t()*Z2).i()*Z2.t()*Y;



    fn_seq(k) = as_scalar(norm(Y-Xmat0*beta_new, 2));
    d_fn = abs(fn_seq(k-1) - fn_seq(k)) / fn_seq(k-1);
    d_par = norm(beta_new-beta);

    beta = beta_new;
    if(verbose){
      Rprintf("iter=%d, d_fn=%f, d_par = %f \n", k+1, d_fn, d_par);
    }
    if((d_par < eps_be) || (d_fn < eps_obj)){
      break;
    }
  }
  iter = k+1;
  // Z2.row(0).print();

  List res = List::create(
    Rcpp::Named("beta") = beta,
    Rcpp::Named("hX") = Z2,
    // Rcpp::Named("beta_int") = beta_int,
    Rcpp::Named("Iterations") = iter,
    //Rcpp::Named("fn_seq") = fn_seq,
    Rcpp::Named("d.fn") = d_fn,
    Rcpp::Named("d.par") = d_par);

  return res;
}
