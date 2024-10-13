\name{lassoshooting}
\alias{lassoshooting}
\title{Lasso Shooting}
\encoding{utf-8}
\description{
Efficient estimates of sparse regression coefficients with a lasso (L1) penalty
}
\usage{
lassoshooting(X=NULL, y=NULL, lambda, XtX=NULL, Xty=NULL, thr=1.0e-6,
              maxit=1e4, nopenalize=NULL, penaltyweight=NULL, trace=0, ...)
}
\arguments{
\item{X}{Design matrix: N by p matrix of p explanatory variables}
\item{y}{vector of 1 response variable for N observations}
\item{XtX}{X'X, could be given together with X'y instead of X and y}
\item{Xty}{X'y, could be given together with X'X instead of X and y}
\item{lambda}{(Non-negative) regularization parameter for lasso. lambda=0 means no regularization.}
\item{thr}{Threshold for convergence. Default value is 1e-4.  Iterations stop when max absolute parameter change is less than thr}
\item{maxit}{Maximum number of iterations of outer loop. Default 10,000}
\item{nopenalize}{List of coefficients not to penalize starting at 0}
\item{penaltyweight}{p weights, one per variable, will be multiplied by overall lambda penalty}
\item{trace}{Level of detail for printing out information as iterations proceed.
Default 0 -- no information}
\item{...}{Reserved for experimental options}
}

\details{
Estimates a sparse regression coefficient vector using a lasso (L1) penalty
using the approach of cyclic coordinate descent. See references for details.

The solver does NOT include an intercept, add a column of ones to \code{x} if your data is not centered.
}
\value{
A list with components
\item{coefficients}{Estimated regression coefficient vector}
\item{iterations}{Number of iterations of outer loop used by algorithm}
\item{delta}{Change in parameter value at convergence}
\item{infnorm}{\eqn{||X'y||_\infty}}
}
\author{
  Tobias Abenius
}
 \references{
   Rebecka Jörnsten, Tobias Abenius, Teresia Kling, Linnéa Schmidt, Erik Johansson, Torbjörn Nordling, Bodil Nordlander, Chris Sander, Peter Gennemark, Keiko Funa, Björn Nilsson, Linda Lindahl, Sven Nelander. (2011)  Network modeling of the transcriptional effects of copy number aberrations in glioblastoma.   Molecular Systems Biology 7 (to appear)

   Friedman J, Hastie T, et al. (2007) Pathwise coordinate optimization. Ann Appl Stat 1: 302--332

   Fu WJ (1998) Penalized regressions: the bridge versus the lasso. J Comput Graph Statist 7: 397--416
}
\examples{
  \dontrun{
set.seed(42)

b <- seq(3,3,length=10)
n<-100;
p<-10; 
X <- matrix(rnorm(n*p),n,p)
noise <- as.matrix(rnorm(n,sd=0.1))
y <- X %*% b + noise

require(lassoshooting)
# FIXME: write proper example using R built in dataset
#add intercept column to the design matrix
Xdesign <- cbind(1,X)
lambda <- 20
#don't penalize the intercept
bhat <- lassoshooting(X=Xdesign,y=y,lambda=lambda,nopenalize=0) 

#above equals below 
bhat1 <- lassoshooting(X=Xdesign,y=y,lambda=2*lambda,penaltyweight=c(0,seq(0.5,0.5,length=p-1)))

T1 <- all(abs(bhat1-bhat) < 1e-20)

c <- 10
bhat2 <- lassoshooting(X=Xdesign,y=y, lambda=lambda, penaltyweight=c(0,1,1,1,1,1,c,c,c,c,c))

T2 <- all(bhat2[2:6] > bhat2[7:11])
T1 && T2
  }
}
\keyword{selection}
\keyword{multivariate}
\keyword{models}
\keyword{graphs}

