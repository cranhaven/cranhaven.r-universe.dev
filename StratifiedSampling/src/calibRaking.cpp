#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
//' @title Calibration using raking ratio  
//' @name calibRaking
//' @description This function is inspired by the function \code{\link[sampling:calib]{calib}} of the package sampling. It computes the g-weights of the calibration estimator.
//' 
//' @param Xs A matrix of calibration variables.
//' @param d A vector, the initial weights.
//' @param total A vector that represents the initial weights.
//' @param q A vector of positive value that account for heteroscedasticity.
//' @param max_iter An integer, the maximum number of iterations. Default = 500.
//' @param tol A scalar that represents the tolerance value for the algorithm. Default = 1e-9.
//'
//' @details
//' More details on the different calibration methods can be read in Tillé Y. (2020).
//'
//' @return A vector, the value of the g-weights.
//'
//' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
//'
//'
//' @references Tillé, Y. (2020). \emph{Sampling and estimation from finite populations}. Wiley, New York
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List calibRaking(arma::mat Xs,
                                arma::vec d,
                                arma::vec total,
                                arma::vec q,
                                int max_iter = 500,
                                double tol = 1e-9){
  
  // intiializing
  // int n = Xs.n_rows;
  int p = Xs.n_cols;
  
  arma::vec lambda(p,arma::fill::zeros);
  arma::vec w1 = d % exp(Xs * lambda % q);
  
  // tol and iteration counter
  double crit = 1;
  int iter = 0;
  
  // convergence loop
  do{
    arma::vec phi = Xs.t() * w1 - total;
    arma::mat phiprim = Xs.t() * arma::diagmat(w1) * Xs;
    lambda = lambda - arma::pinv(phiprim) * phi;
    w1 = d % exp(Xs * lambda % q);
    arma::vec tr = Xs.t() * w1;
    crit = max(abs(tr - total)/total);
    iter = iter + 1;
  } while ( (crit > tol) & (iter < max_iter));
  
  if(iter == max_iter){
    Rcpp::Rcout << "Max number of iteration reached : " << iter << std::endl;  
    Rcpp::Rcout << "The relative distance is equal to " << crit << std::endl;  
  }
  
  // return(w1/d);
  
  Rcpp::NumericVector out = Rcpp::wrap(w1/d);
  out.attr("dim") = R_NilValue;
  // return(out);
  
  return Rcpp::List::create(Rcpp::Named("crit") = crit,
                     Rcpp::Named("iter") = iter,
                     Rcpp::Named("g") = out);
  
  
}



/*** R



Xs=cbind(
  c(1,1,1,1,1,0,0,0,0,0),
  c(0,0,0,0,0,1,1,1,1,1),
  c(1,2,3,4,5,6,7,8,9,10)
)
# inclusion probabilities
piks=rep(0.2,times=10)
# vector of population totals
total=c(24,26,290)
d <- 1/piks
q <- rep(1,length(d))
calibRaking(Xs,d= 1/piks,total,q,max_iter = 500)


library(MASS)
N <- 10000
X <- data.frame(x1 = rnorm(N,0,1), x2 = rnorm(N,0,1))

mu <- c(4,9)
Sigma <- matrix(c(1,0.3,0.3,1),ncol = 2)
X <- mvrnorm(N,mu,Sigma)
colnames(X) <- c("x1","x2")
X <- as.data.frame(X)

Y <- data.frame(y1 = 3*X$x1^2 + 4*X$x2^2 + rnorm(N,0,1),y2 = exp(X$x1) + rnorm(N,0,0.1))
Z <- data.frame(z1 = 8*X$x1^2 - 3*X$x2^2 + rnorm(N,0,1),z2 = sqrt(abs(X$x2)) + rnorm(N,0,0.1))


n1=1000
n2=3000

s1=sampling::srswor(n1,N)
s2=sampling::srswor(n2,N)

id1=(1:N)[s1==1]
id2=(1:N)[s2==1]

d1=rep(N/n1,n1)
d2=rep(N/n2,n2)

X1 = X[s1==1,]
X2 = X[s2==1,]
Y1 <- data.frame(Y[s1 == 1,])
Z2 <- data.frame(Z[s2 == 1,])


# number of units in each sample
n1 <- nrow(X1)
n2 <- nrow(X2)

# add constant vector to ensure same sum
XX1=cbind(rep(1,n1),X1)
XX2=cbind(rep(1,n2),X2)

# we can specify the desired total (for example if we know the totals of the population)
n12=length(intersect(id1,id2))
a=(n1-n12)/(n1+n2-2*n12)  
totals=a*colSums(d1*XX1)+(1-a)*colSums(d2*XX2)

q <- rep(1,length(d1))
# calibration with sampling package
# w1=d1*calib(XX1,d1,totals,method=method)


sampling::calib(XX1,d1,totals,method = "raking")
calibRaking(as.matrix(XX1),d1,totals,q)





*/




// [[Rcpp::depends(RcppArmadillo)]]
//' @title Generalized calibration using raking ratio  
//' @name gencalibRaking
//' @description This function is inspired by the function \code{\link[sampling:calib]{calib}} of the package sampling. It computes the g-weights of the calibration estimator.
//' 
//' @param Xs A matrix of calibration variables.
//' @param Zs A matrix of instrumental variables with same dimension as Xs. 
//' @param d A vector, the initial weights.
//' @param total A vector that represents the initial weights.
//' @param q A vector of positive value that account for heteroscedasticity.
//' @param max_iter An integer, the maximum number of iterations. Default = 500.
//' @param tol A scalar that represents the tolerance value for the algorithm. Default = 1e-9.
//'
//' @details
//' More details on the different calibration methods can be read in Tillé Y. (2020).
//'
//' @return A vector, the value of the g-weights.
//'
//' @author Raphaël Jauslin \email{raphael.jauslin@@unine.ch}
//'
//'
//' @references Tillé, Y. (2020). \emph{Sampling and estimation from finite populations}. Wiley, New York
//' 
//' @export
// [[Rcpp::export]]
Rcpp::List gencalibRaking(arma::mat Xs,
                       arma::mat Zs,
                       arma::vec d,
                       arma::vec total,
                       arma::vec q,
                       int max_iter = 500,
                       double tol = 1e-9){
  
  // intiializing
  // int n = Xs.n_rows;
  int p = Xs.n_cols;
  
  arma::vec lambda(p,arma::fill::zeros);
  arma::vec w1 = d % exp(Zs * lambda % q);
  
  // tol and iteration counter
  double crit = 1;
  int iter = 0;
  
  // convergence loop
  do{
    arma::vec phi = Xs.t() * w1 - total;
    arma::mat phiprim = Xs.t() * arma::diagmat(w1) * Zs;
    lambda = lambda - arma::pinv(phiprim) * phi;
    w1 = d % exp(Zs * lambda % q);
    arma::vec tr = Xs.t() * w1;
    crit = max(abs(tr - total)/total);
    iter = iter + 1;
  } while ( (crit > tol) & (iter < max_iter));
  
  if(iter == max_iter){
    Rcpp::Rcout << "Max number of iteration reached : " << iter << std::endl;  
    Rcpp::Rcout << "The relative distance is equal to " << crit << std::endl;  
  }
  
  // return(w1/d);
  
  Rcpp::NumericVector out = Rcpp::wrap(w1/d);
  out.attr("dim") = R_NilValue;
  // return(out);
  
  return Rcpp::List::create(Rcpp::Named("crit") = crit,
                            Rcpp::Named("iter") = iter,
                            Rcpp::Named("g") = out);
  
  
}


/*** R


############
## Example 1
############
# matrix of sample calibration variables 
Xs=cbind(
  c(1,1,1,1,1,0,0,0,0,0),
  c(0,0,0,0,0,1,1,1,1,1),
  c(1,2,3,4,5,6,7,8,9,10))
# inclusion probabilities
piks=rep(0.2,times=10)
# vector of population totals
total=c(24,26,290)
# matrix of instrumental variables
Zs=Xs+matrix(runif(nrow(Xs)*ncol(Xs)),nrow(Xs),ncol(Xs))
# the g-weights using the truncated method
q=rep(1,length(d))
g=gencalib(Xs,Zs,d=1/piks,total,q,method="raking")
g1=gencalibRaking(Xs,Zs,d=1/piks,total,q)
# the calibration estimator of X is equal to the 'total' vector
t(g/piks)%*%Xs
# the g-weights are between lower and upper bounds
summary(g)



############
## Example 2
############
# Example of generalized g-weights (linear, raking, truncated, logit),
# with the data of Belgian municipalities as population.
# Firstly, a sample is selected by means of Poisson sampling.
# Secondly, the g-weights are calculated.
data(belgianmunicipalities)
attach(belgianmunicipalities)
# matrix of calibration variables for the population
X=cbind(Totaltaxation/mean(Totaltaxation),medianincome/mean(medianincome))
# selection of a sample with expected size equal to 200
# by means of Poisson sampling
# the inclusion probabilities are proportional to the average income 
pik=inclusionprobabilities(averageincome,200)
N=length(pik)               # population size
s=UPpoisson(pik)            # sample
Xs=X[s==1,]                 # sample calibration variable matrix 
piks=pik[s==1]              # sample inclusion probabilities
n=length(piks)              # sample size
# vector of population totals of the calibration variables
total=c(t(rep(1,times=N))%*%X)  
# the population total
total
Z=cbind(TaxableIncome/mean(TaxableIncome),averageincome/mean(averageincome))
# defines the instrumental variables
Zs=Z[s==1,]
# computation of the generalized g-weights
# by means of different generalized calibration methods
g1=gencalib(Xs,Zs,d=1/piks,total,method="linear")
g2=gencalib(Xs,Zs,d=1/piks,total,method="raking")
q = rep(1,length(d))
g22 = gencalibRaking(Xs,Zs,d=1/piks,total,q)


g3=gencalib(Xs,Zs,d=1/piks,total,method="truncated",bounds=c(0.5,8))
g4=gencalib(Xs,Zs,d=1/piks,total,method="logit",bounds=c(0.5,1.5))
# In some cases, the calibration does not exist
# particularly when bounds are used.
# if the calibration is possible, the calibration estimator of X total is printed
if(checkcalibration(Xs,d=1/piks,total,g1)$result) print(c((g1/piks)%*% Xs)) else print("error")
if(!is.null(g2))
  if(checkcalibration(Xs,d=1/piks,total,g2)$result) print(c((g2/piks)%*% Xs)) else print("error")
if(!is.null(g3))
  if(checkcalibration(Xs,d=1/piks,total,g3)$result) print(c((g3/piks)%*% Xs)) else print("error")
if(!is.null(g4))
  if(checkcalibration(Xs,d=1/piks,total,g4)$result) print(c((g4/piks)%*% Xs)) else print("error")


*/
