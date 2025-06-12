// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::plugins(cpp11)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

static double const log2pi = std::log(2.0 * M_PI);

// [[Rcpp::export]]
arma::vec dmvnrm_arma_old(arma::mat x,  
                          arma::rowvec mean,  
                          arma::mat sigma, 
                          bool logd = false) { 
  using arma::uword;
  uword const n = x.n_rows, 
    xdim = x.n_cols;
  arma::vec out(n);
  arma::mat rooti = arma::trans(arma::inv(trimatu(arma::chol(sigma))));
  double rootisum = arma::sum(log(rooti.diag()));
  double constants = -(double)xdim/2.0 * log2pi;
  
  for (uword i = 0; i < n; i++) {
    arma::vec z = rooti * arma::trans( x.row(i) - mean) ;    
    out(i)      = constants - 0.5 * arma::sum(z%z) + rootisum;     
  }  
  
  if (logd)
    return out;
  return exp(out);
}

// [[Rcpp::export]]
arma::vec dmvnrm_arma(arma::mat const &x,  
                      arma::rowvec const &mean,  
                      arma::mat const &sigma, 
                      bool const logd = false) { 
  using arma::uword;
  uword const n = x.n_rows, 
    xdim = x.n_cols;
  arma::vec out(n);
  arma::mat const rooti = arma::inv(trimatu(arma::chol(sigma)));
  double const rootisum = arma::sum(log(rooti.diag())), 
    constants = -(double)xdim/2.0 * log2pi, 
    other_terms = rootisum + constants;
  
  arma::rowvec z;
  for (uword i = 0; i < n; i++) {
    z      = (x.row(i) - mean) * rooti;    
    out(i) = other_terms - 0.5 * arma::dot(z, z);     
  }  
  
  if (logd)
    return out;
  return exp(out);
}

/* C++ version of the dtrmv BLAS function */
void inplace_tri_mat_mult(arma::rowvec &x, arma::mat const &trimat){
  arma::uword const n = trimat.n_cols;
  
  for(unsigned j = n; j-- > 0;){
    double tmp(0.);
    for(unsigned i = 0; i <= j; ++i)
      tmp += trimat.at(i, j) * x[i];
    x[j] = tmp;
  }
}

// [[Rcpp::export]]
arma::vec dmvnrm_arma_fast(arma::mat const &x,  
                           arma::rowvec const &mean,  
                           arma::mat const &sigma, 
                           bool const logd = false) { 
  using arma::uword;
  uword const n = x.n_rows, 
    xdim = x.n_cols;
  arma::vec out(n);
  arma::mat const rooti = arma::inv(trimatu(arma::chol(sigma)));
  double const rootisum = arma::sum(log(rooti.diag())), 
    constants = -(double)xdim/2.0 * log2pi, 
    other_terms = rootisum + constants;
  
  arma::rowvec z;
  for (uword i = 0; i < n; i++) {
    z = (x.row(i) - mean);
    inplace_tri_mat_mult(z, rooti);
    out(i) = other_terms - 0.5 * arma::dot(z, z);     
  }  
  
  if (logd)
    return out;
  return exp(out);
}

// [[Rcpp::export]]
float loglik_G_c(const arma::mat y, const arma::vec prob, const arma::mat mu,
                 const arma::cube Sigma, const int C, const int N)
{
  float ll = 0.0;

  for (int i = 0; i < N; i++)
  {
    arma::vec ww = arma::zeros<arma::vec>(mu.n_rows);
    for (arma::uword j = 0; j < mu.n_rows; j++)
    {
      arma::rowvec temp_y = y.row(i);
      arma::rowvec temp_mu = mu.row(j);
      arma::mat temp_sigma = Sigma(arma::span::all, arma::span::all, arma::span(j));

      // ww += dmvnrm_arma_fast(t(y[i,]), mean = mu[kk,], Sigma[,,kk], log = FALSE)
      // ww = c(ww, prob[kk] * dmvnorm(t(y[i,]), mean = mu[kk,], Sigma[,,kk], log = FALSE))
        
      arma::vec temp = dmvnrm_arma_fast(temp_y, temp_mu, temp_sigma);
      ww[j] = prob[j] * temp(0,0);
    }
    
    ll += std::log(arma::accu(ww));
  }

  return(ll);
}

// [[Rcpp::export]]
arma::vec weights_multi_c(const arma::vec x, const arma::vec prob, const arma::mat mu,
                               const arma::cube Sigma, const int C)
{
  arma::vec ww = arma::zeros<arma::vec>(mu.n_rows);
  
  for (arma::uword k = 0; k < mu.n_rows; k++)
  {
    arma::rowvec temp_mu = mu.row(k);
    arma::mat temp_sigma = Sigma(arma::span::all, arma::span::all, arma::span(k));
    
    arma::vec temp = dmvnrm_arma_fast(x.t(), temp_mu, temp_sigma);
    ww[k] = prob[k] * temp(0,0);
  }
  
  for (arma::uword i = 0; i < ww.n_elem; i++)
  {
    ww[i] = ww[i]/arma::accu(ww);
  }
  
  return(ww);
}

// [[Rcpp::export]]
arma::mat weights_multi_matrix_c(const arma::mat y, const arma::vec prob, const arma::mat mu,
                                    const arma::cube Sigma, const int C)
{
    arma::mat z_total(y.n_rows, mu.n_rows, arma::fill::zeros);
    // std::cout << z_total.n_rows << "/" << z_total.n_cols;
    for (arma::uword i = 0; i < y.n_rows; i++)
    {
      arma::rowvec temp = y.row(i);
      arma::vec weights = weights_multi_c(temp.t(), prob, mu, Sigma, mu.n_rows);
      z_total.row(i) = weights.t();
    }
    return(z_total);
}

