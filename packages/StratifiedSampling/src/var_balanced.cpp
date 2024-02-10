#include <RcppArmadillo.h>
#include "distUnitk.h"

// [[Rcpp::depends(RcppArmadillo)]]

//' @encoding UTF-8
//' @title Variance Estimation for balanced sample
//' @name vEst
//' @description
//' Estimated variance approximation calculated as the conditional variance with respect to the balancing equations of a particular Poisson design. See Tillé (2020)
//' 
//' @param Xauxs A matrix of size (\eqn{n} x \eqn{p}) of auxiliary variables on which the sample must be balanced.
//' @param piks A vector of inclusion probabilities. The vector has the size of the sample \eqn{s}.
//' @param ys A variable of interest.The vector has the size \eqn{n} of the sample \eqn{s}.
//' 
//' @references 
//' Tillé, Y. (2020), Sampling and Estimation from finite populations, Wiley,
//'
//' @return Estimated variance of the horvitz-thompson estimator.
//' 
//' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
//' 
//' @seealso \code{\link{vDBS}} \code{\link{vApp}} 
//' 
//' @export
//' 
//' @examples
//' 
//' N <- 100 
//' n <- 40
//' x1 <- rgamma(N,4,25)
//' x2 <- rgamma(N,4,25)
//' 
//' pik <- rep(n/N,N)
//' Xaux <- cbind(pik,as.matrix(matrix(c(x1,x2),ncol = 2)))
//' Xspread <- cbind(runif(N),runif(N))
//'   
//' 
//' s <- balseq(pik,Xaux,Xspread)
//'   
//' y <- Xaux%*%c(1,1,3) + rnorm(N,120) # variable of interest
//'   
//' vEst(Xaux[s,],pik[s],y[s])
//' vDBS(Xaux[s,],Xspread[s,],pik[s],y[s])
//' vApp(Xaux,pik,y)
//' 
// [[Rcpp::export]]
arma::mat vEst(arma::mat Xauxs,
               arma::vec piks,
               arma::vec ys) {
  int n = ys.size();
  int p = Xauxs.n_cols;
  
  arma::mat A = arma::diagmat(1.0/piks)*Xauxs;
  
  arma::vec c = (1.0*n/(n-p))*(1.0 - piks);
  arma::mat D = arma::diagmat(c);
  arma::mat XX = arma::pinv(A.t()*D*A);
  
  arma::mat proj = A*XX*A.t()*D;
  
  arma::vec pred = proj*(ys/piks);
  arma::vec diff = ys/piks - pred;
  arma::mat out = diff.t()*D*diff;
  
  return(out);
  
}

/*** R

N <- 100
n <- 40

x1 <- rgamma(N,4,25)
x2 <- rgamma(N,4,25)

pik <- rep(n/N,N)
Xaux <- cbind(pik,as.matrix(matrix(c(x1,x2),ncol = 2)))
Xspread <- cbind(runif(N),runif(N))

s <- balseq(pik,Xaux)
s <- balseq(pik,Xaux,Xspread)

y <- Xaux%*%c(1,1,3) + rnorm(N,120) # variable of interest

vEst(Xaux[s,],pik[s],y[s])
vDBS(Xaux[s,],Xspread[s,],pik[s],y[s])
vApp(Xaux,pik,y)

*/


//' @encoding UTF-8
//' @title Variance Estimation for Doubly Balanced Sample.
//' @name vDBS
//' @description
//' Variance estimator for sample that are at the same time well spread and balanced on auxiliary variables. See Grafstr\"om and Till\'e (2013)
//' 
//' @param Xauxs A matrix of size (\eqn{n} x \eqn{p}) of auxiliary variables on which the sample must be balanced.
//' @param Xspreads Matrix of spatial coordinates.
//' @param piks A vector of inclusion probabilities. The vector has the size \eqn{n} of the sample \eqn{s}.
//' @param ys A variable of interest. The vector has the size \eqn{n} of the sample \eqn{s}.
//' 
//' @references 
//' Grafstr\"om, A. and Till\'e, Y. (2013), Doubly balanced spatial sampling with spreading and restitution of auxiliary totals, Environmetrics, 14(2):120-131
//'
//' @return Estimated variance of the horvitz-thompson estimator.
//' 
//' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
//' 
//' @seealso \code{\link{vDBS}} \code{\link{vApp}} 
//' 
//' @export
//' 
//' @examples
//' 
//' N <- 100 
//' n <- 40
//' x1 <- rgamma(N,4,25)
//' x2 <- rgamma(N,4,25)
//' 
//' pik <- rep(n/N,N)
//' Xaux <- cbind(pik,as.matrix(matrix(c(x1,x2),ncol = 2)))
//' Xspread <- cbind(runif(N),runif(N))
//'   
//' 
//' s <- balseq(pik,Xaux,Xspread)
//'   
//' y <- Xaux%*%c(1,1,3) + rnorm(N,120) # variable of interest
//'   
//' vEst(Xaux[s,],pik[s],y[s])
//' vDBS(Xaux[s,],Xspread[s,],pik[s],y[s])
//' vApp(Xaux,pik,y)
//' 
// [[Rcpp::export]]
double vDBS(arma::mat Xauxs,
               arma::mat Xspreads,
               arma::vec piks,
               arma::vec ys){
  
  double eps = 1e-7;
  int n = ys.size();
  int p = Xauxs.n_cols;
  
  arma::mat A = arma::diagmat(1.0/piks)*Xauxs;
  
  arma::vec c = (1.0 - piks);
  arma::mat D = arma::diagmat(c);
  arma::mat XX = arma::pinv(A.t()*D*A);
  arma::mat proj = XX*A.t()*D;
  arma::vec pred = proj*(ys/piks);
  arma::vec e = ys - Xauxs*pred;
  
  arma::vec e_bar(n);
  arma::vec dd(n);
  arma::uvec idx(n);
  
  for(int i = 0;i< n; i++){
    dd = distUnitk(Xspreads,i+1,false,0.0);
    idx =  arma::sort_index(arma::sort_index(dd));
    arma::uvec ii = arma::find(idx < (1.0*(p + 1 + eps)));
    
    e_bar[i] = arma::sum((1.0 - piks.elem(ii))%e.elem(ii)/piks.elem(ii))/arma::sum((1.0 - piks.elem(ii)));
  }
  
  double v = 0.0;
  for(int k = 0;k < n; k ++){
    double b_k = (1-piks[k]);
    v = v + b_k*pow((e[k]/piks[k] - e_bar[k]),2);
  }
  v = (1.0*n/(n-p)) * ((1.0*p + 1.0)/p*1.0) * v;
  return(v);
  
}


/*** R

N <- 1000
n <- 100
p <- 5

Xaux <- matrix(rnorm(N*p),ncol = p)
Xspread <- as.matrix(cbind(runif(N),runif(N)))
pik <- rep(n/N,N)
Xaux <- cbind(pik,Xaux)
s <- BalancedSampling::cube(pik,Xaux)
y <- Xaux%*%c(1,1,1,1,1,1) + rnorm(N)


varB(Xaux[s,],pik[s],y[s])
varB2(Xaux[s,],pik[s],y[s])
varDBS(Xaux[s,],Xspread[s,],pik[s],y[s])
vDBS(Xaux[s,],Xspread[s,],pik[s],y[s])


*/



//' @encoding UTF-8
//' @title Approximated variance for balanced sample
//' @name vApp
//' @description
//' 
//' Variance approximation calculated as the conditional variance with respect to the balancing equations of a particular Poisson design. See Tillé (2020)
//' 
//' @param Xaux A matrix of size (\eqn{N} x \eqn{p}) of auxiliary variables on which the sample must be balanced.
//' @param pik A vector of inclusion probabilities. The vector has the size \eqn{N} of the population \eqn{U}.
//' @param y A variable of interest.
//' 
//' @references 
//' Tillé, Y. (2020), Sampling and Estimation from finite populations, Wiley,
//' 
//' @return Approximated variance of the Horvitz-Thompson estimator.
//' 
//' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
//' 
//' @seealso \code{\link{vDBS}} \code{\link{vApp}} 
//' 
//' @export
//' 
//' @examples
//' 
//' N <- 100 
//' n <- 40
//' x1 <- rgamma(N,4,25)
//' x2 <- rgamma(N,4,25)
//' 
//' pik <- rep(n/N,N)
//' Xaux <- cbind(pik,as.matrix(matrix(c(x1,x2),ncol = 2)))
//' Xspread <- cbind(runif(N),runif(N))
//'   
//' 
//' s <- balseq(pik,Xaux,Xspread)
//'   
//' y <- Xaux%*%c(1,1,3) + rnorm(N,120) # variable of interest
//'   
//' vEst(Xaux[s,],pik[s],y[s])
//' vDBS(Xaux[s,],Xspread[s,],pik[s],y[s])
//' vApp(Xaux,pik,y)
//' 
// [[Rcpp::export]]
arma::mat vApp(arma::mat Xaux,
               arma::vec pik,
               arma::vec y) {
  int N = Xaux.n_rows;
  int p = Xaux.n_cols;
  
  
  arma::mat A = arma::diagmat(1.0/pik)*Xaux;
  arma::vec b = (1.0*N/(N-p))*(pik%(1.0-pik)) ;
  arma::mat D = arma::diagmat(b);
  arma::mat XX = arma::pinv(A.t()*D*A);
  
  
  arma::mat proj = A*XX*A.t()*D;
  
  arma::vec pred = proj*(y/pik);
  arma::vec diff = y/pik - pred;
  arma::mat out = diff.t()*D*diff;
  
  return(out);
  
}


/*** R

N <- 100
n <- 40

x1 <- rgamma(N,4,25)
x2 <- rgamma(N,4,25)

pik <- rep(n/N,N)
Xaux <- cbind(pik,as.matrix(matrix(c(x1,x2),ncol = 2)))
Xspread <- cbind(runif(N),runif(N))

s <- balseq(pik,Xaux)
s <- balseq(pik,Xaux,Xspread)

y <- Xaux%*%c(1,1,3) + rnorm(N,120) # variable of interest

vEst(Xaux[s,],pik[s],y[s])
vDBS(Xaux[s,],Xspread[s,],pik[s],y[s])
vApp(Xaux,pik,y)


*/

