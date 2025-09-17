// [[Rcpp::depends(RcppArmadillo)]]
#include <linconGaussR.h>

using namespace Rcpp;
using namespace arma;
using namespace std;
using namespace linconGaussR;


//[[Rcpp::export]]
arma::mat linconGauss_cpp(int n, 
                            arma::mat A, 
                            arma::vec b, 
                            arma::mat Sigma, 
                            arma::vec mu, 
                            arma::vec x_init,
                            bool intersection=true,
                            int nskp=5){
    return linconGaussR::linconGauss_cpp(n,A,b,Sigma,mu,x_init,intersection,nskp);
}
