#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::List get_gamma_c(arma::vec class_values, const int K)
{
  Rcpp::List gamma(K);
  
  int count = 0;
  for (int i = 0; i < K; i++)
  {
    arma::uvec x = find(class_values == (i + 1));
     
    if (x.n_elem > 0)
    {
      for (arma::uword j = 0; j < x.n_elem; j++)
      {
        x[j] = x[j] + 1;
      }
      gamma[count] = x;
      count += 1;
    }
  }
  
  return (gamma);
}

// [[Rcpp::export]]
Rcpp::List get_mean_values_c(const int K, const Rcpp::List gamma, const arma::mat GG)
{
  arma::vec mean_diag = arma::zeros<arma::vec>(K);
  arma::mat nn(K, K);
  arma::mat mean_off(K, K, arma::fill::zeros);
  
  for (int i = 0; i < K; i++)
  {
    if (gamma.size() <= i) continue;
    arma::uvec gamma_i = gamma[i];
    if (gamma_i.n_elem <= 1) continue;
    // reset indices
    for (arma::uword k = 0; k < gamma_i.n_elem; k++)
    {
      gamma_i[k] = gamma_i[k] - 1;
    }

    // inner loop
    for (int j = i; j < K; j++)
    {
      arma::uvec gamma_j = gamma[j];
      // reset indices
      for (arma::uword k = 0; k < gamma_j.n_elem; k++)
      {
        gamma_j[k] = gamma_j[k] - 1;
      }
      
      if (gamma.size() <= j) continue;
      if (gamma_j.n_elem <= 1) continue;

      // std::cout << gamma_i.n_elem << "/" << gamma_j.n_elem << "--" << i << "/" << j << std::endl;
      // homogenous off diagonals
      if (i == j)
      {
        arma::mat GG_1 = GG.submat(gamma_i, gamma_j);
        nn(i,j) = (GG_1.n_cols*GG_1.n_rows - GG_1.n_rows);
        mean_diag[i] = arma::mean(GG_1.diag());
        mean_off(i, j) = (arma::accu(GG_1) - sum(GG_1.diag()))/nn(i, j);
      }
      // heterogenous off diagonals
      else
      {
        arma::mat GG_2 = GG.submat(gamma_i, gamma_j);
        mean_off(i,j) = mean(mean(GG_2, 0));
      }
    }
  }

  
  return Rcpp::List::create(Rcpp::Named("mean_diag") = mean_diag, 
                             Rcpp::Named("mean_off") = mean_off);
}


// [[Rcpp::export]]
arma::mat get_mu_c(arma::vec labels, const Rcpp::List gamma, const arma::mat GG_new, const int N, const int K,
                          arma::mat mean_off, arma::vec mean_diag)
{
  arma::mat MU(K, N + 1, arma::fill::zeros);

  for (int i = 0; i < K; i++)
  {
    arma::uvec gamma_i = gamma[i];
    for (arma::uword k = 0; k < gamma_i.n_elem; k++)
    {
      gamma_i[k] = gamma_i[k] - 1;
    }
    
    // std::cout << "loop: " << i << "with length for gamma = " << gamma_i.n_elem << std::endl;
    if (gamma_i.n_elem == 1)
    {
      MU.row(i) = GG_new.row(gamma_i[0]);
    }
    else
    {
      int object = gamma_i[0];
      int labels_object = labels[object] - 1;
      for (int j = 0; j < N; j++)
      {
        // correct indices to 0 based
        int labels_j = labels[j] - 1;

        if (labels_j == labels_object)
        {
          arma::colvec diag_values = mean_off.diag();
          MU(i, j) = diag_values[labels_object];
        }
        if (labels_j != labels_object)
        {
          MU(i, j) = mean_off(labels_object, labels_j);
        }
      }

      // add to mean matrix
      MU(i, N) = mean_diag[labels_object];
    }
  }

  return(MU);
}



  





