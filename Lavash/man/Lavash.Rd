\name{Lavash}
\alias{Lavash}
\title{Lava Estimation for the Sum of Sparse and Dense Signals}
\description{
The lava estimation of linear regression models, which are suitable for estimating coefficients that can be represented by the sum of sparse and dense signals. The post-lava method corrects the shrinkage bias of lava. The model is Y=X*b+error, where b can be decomposed into b=dense+sparse. The method of lava solves the following problem: min_[b2,b1] 1/n*|Y-X*(b1+b2)|_2^2+lamda2*|b2|_2^2+lambda1*|b1|_1. The estimator is b2+b1. Both tuning parameters are chosen using the K-fold cross-validation.
}
\usage{
Lavash(X, Y, K, Lambda1, Lambda2, method = c("profile", "iteration"), Maxiter = 50)
}
\arguments{
  \item{X}{
n by p data matrix, where n and p respectively denot the sample size and the number of regressors.
}
  \item{Y}{
n by 1 matrix of outcome.
}
  \item{K}{
the K fold cross validation.
}
  \item{Lambda1}{
a vector of candidate values to be evaluated for lambda1, in the cross validation.
}
  \item{Lambda2}{
a vector of candidate values to be evaluated for lambda2, in the cross validation.
}
  \item{method}{
choose between 'profile' and 'iteration'. 'profile' computes using the profiled lasso method. 'iteration' computes using iterating lasso and ridge.
}
  \item{Maxiter}{
the maximum number of iterations. the default value is 50. Only used when 'iteration' is selected.
}
}
\details{
We recommend using a relatively long vector of Lambda1 (e.g., 50 or 100 values), but a short vector of Lambda2 (e.g., within 10). Higher dimensions of Lambda2 substantially increase the computational time because a 'for' loop is called for lambda2.
Two algorithms are used: (i) the profiled lasso method, building on the singular value decomposition of the design matrix, and runs fast. (whose computational cost is nearly the same as that of lasso) (ii) the iteration between lasso and ridge.
}
\value{
\item{lava_dense }{parameter estimate of the dense component using lava}
\item{lava_sparse }{parameter estimate of the sparse component using lava}
\item{lava_estimate }{equals lava_dense+lave_estimate: parameter estimate using lava}
\item{postlava_dense }{parameter estimate of the dense component using post-lava}
\item{postlava_sparse }{parameter estimate of the sparse component using post-lava}
\item{post_lava }{equals postlava_dense+postlava_sparse: parameter estimate using post-lava}
\item{LAMBDA }{[lambda1lava,lambda2lava, lambda1post, lambda2post]: These are the CV-chosen lambda1 and lambda2 tunings for lava and post-lava.}
}
\references{
Chernozhukov, V., Hansen, C., and Liao, Y. (2017) "A lava attack on the recovery of sums of dense and sparse signals", Annals of Statistics, 45, 39-76
}
\author{
Victor Chernozhukov, Christian Hansen, Yuan Liao, Jaeheon Jung
}
\examples{
n <- 10; p <- 5
b <- matrix(0,p,1); b[1,1] <- 3; b[2:p,1] <- 0.1
X <- randn(n,p) 
Y <- X\%*\%b+randn(n,1)
K<-5
iter<-50
Lambda2<-c(0.01, 0.07, 0.2, 0.7, 3,10,60,1000,2000)
Lambda1<-seq(0.01,6,6/50)

result<-Lavash(X,Y,K,Lambda1,Lambda2,method="profile", Maxiter = iter)

result$lava_dense
result$lava_sparse
result$lava_estimate
result$postlava_dense
result$postlava_sparse
result$post_lava
result$LAMBDA
}
\keyword{lava}% use one of  RShowDoc("KEYWORDS")