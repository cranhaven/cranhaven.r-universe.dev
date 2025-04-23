// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace Rcpp;

//' C++ implementation of d_edges
//' 
//' Calculates differential network score for each edge in a network
//' @param nw1 The association scores for network 1
//' @param nw2 The association scores for network 2
//' @param lp The lp value to use.
//' @return A matrix of differential network scores for the edges.
//' @export
// [[Rcpp::export]]
arma::vec d_edgesC(arma::mat nw1, arma::mat nw2, double lp) {
  int p = nw1.n_cols;
  int N = p * (p - 1) / 2;
  arma::vec diff(N);
  
  int k = 0;
  for(int j = 0; j < p - 1; j++) {
    for(int i = j + 1; i < p; i++) {
      diff[k] = pow(std::abs(nw1(j, i) - nw2(j, i)), lp);
      k += 1;
    }
  }
  
  return diff;
}

//' C++ implementation of d_pathway
//' 
//' Calculates differential network score for an entire pathway.
//' @param nw1 The association scores for network 1
//' @param nw2 The association scores for network 2
//' @param lp The lp value to use.
//' @return The differential network score for the pathway.
//' @export
//[[Rcpp::export]]
double d_pathwayC(arma::mat nw1, arma::mat nw2, double lp) {
  int p = nw1.n_cols;
  int N = p * (p - 1) / 2;
  double diff = accu(d_edgesC(nw1, nw2, lp)) / pow((double) N, 1 / 2);
  
  if(lp > 1) {
    diff = pow(diff, 1 / lp);
  }
  
  return diff;
}

//' C++ implementation of d_genes
//' 
//' Calculates differential network score for a set of genes
//' @param nw1 The association scores for network 1
//' @param nw2 The association scores for network 2
//' @param lp The lp value to use.
//' @return A vector of differential network scores for the genes.
//' @export
// [[Rcpp::export]]
arma::vec d_genesC(arma::mat nw1, arma::mat nw2, double lp) {
  int p = nw1.n_cols;
  arma::vec diff(p);
  
  for(int i = 0; i < p; i++) {
    diff[i] = 0;
  }
  
  for(int i = 0; i < p; i++) {
    for(int j = 0; j < p; j++) {
      if(i != j) {
        diff[i] += pow(std::abs(nw1(i, j) - nw2(i, j)), lp);
      }
    }
    
    diff[i] = diff[i] / pow((double) (p - 1), 1 / 2);
    
    if(lp < 1) {
      diff[i] = pow(diff[i], 1 / lp);
    }
  }
  return diff;
}
