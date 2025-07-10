\name{CovComb}
\alias{CovComb}
\title{
Programs to combine partially observed (high dimensional) covariance matrices. Combining datasets this way, using relationships, is an alternative to imputation.
}
\description{
Use for combining partially observed covariance matrices.
This function can be used for combining data from independent experiments by combining the estimated covariance or relationship matrices learned from each of the experiments. 
}
\usage{
CovComb(Klist = NULL, Kinvlist = NULL,
lambda = 1, w = 1, nu = 1000,
maxiter = 500, miniter = 100, Kinit = NULL, 
tolparconv = 1e-04,
loglik=FALSE, plotll=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Klist}{
A list of covariance / relationship matrices with row and column names to be combined.
}
  \item{Kinvlist}{
A list of inverse covariance / relationship matrices with row and column names to be combined, default NULL.
}
  \item{lambda}{
A scalar learning rate parameter, between 0 and 1. 1 is the default value. 
}
  \item{w}{
Weight parameter, a vector of the same length as Klist, elements corresponding to weights assigned to each of the covariance matrices. Default is 1. 
}
  \item{nu}{
Degrees of freedom parameter. It is either a scalar (same degrees of freeom to each of the covariance component) or a vector of the same length as Klist elements of which correspond to each of the covariance matrices. Currently, only scalar nu is accepted. Default is 1000. the value of nu needs to be larger than the variables in the covariance matrix.
}
  \item{maxiter}{
Maximum number of iterations before stop. Default value is 500.
}
  \item{miniter}{
Minimum number of iterations before the convergence criterion is checked. Default value is 100.
}

  \item{Kinit}{
Initial estimate of the combined covariance matrix. Default value is an identity matrix. 
}
  \item{tolparconv}{
The minimum change in convergence criteria before stopping the algorithm unless the maxiter is reached. This is not evaluated in the first miniter iterations. Default value is 1e-4.
}
\item{loglik}{
Logical with default FALSE. Return the path of the log-likelihood or not.
}
\item{plotll}{
Logical with default FALSE. Plot the path of the log-likelihood or not.
}
}
\details{Let \eqn{A=\left\{a_1, a_2, \ldots, a_m \right\}} be the set of not necessarily disjoint subsets of genotypes covering a set of \eqn{K} (i.e., \eqn{K= \cup_{i=1}^m a_i}) with total \eqn{n} genotypes. Let \eqn{G_{a_1}, G_{a_2},\ldots, G_{a_m}} be the corresponding sample of covariance matrices.

Starting from an initial estimate \eqn{\Sigma^{(0)}=\nu\Psi^{(0)},} the Wishart EM-Algorithm repeats updating the estimate of the covariance matrix until convergence: 
\deqn{  \Psi^{(t+1)} =\frac{1}{\nu m}\sum_{a\in A}P_a\left[ \begin{array}{cc}
          G_{aa} & G_{aa}(B^{(t)}_{b|a})'  \\
          B^{(t)}_{b|a}G_{aa} & \nu \Psi^{(t)}_{bb|a}+ B^{(t)}_{b|a}G_{aa}(B^{(t)}_{b|a})'
        \end{array}\right]P'_a}
where \eqn{B^{(t)}_{b|a}=\Psi^{(t)}_{ab}(\Psi^{(t)}_{aa})^{-1},} \eqn{\Psi^{(t)}_{bb|a}=\Psi^{(t)}_{bb}-\Psi^{(t)}_{ab}(\Psi^{(t)}_{aa})^{-1}\Psi^{(t)}_{ba},} \eqn{a}  is the set of genotypes in the given partial covariance matrix and \eqn{b} is the set difference of \eqn{K} and \eqn{a.} The matrices \eqn{P_a} are permutation matrices that put each matrix in the sum in the same order. The initial value, \eqn{\Sigma^{(0)}} is usually assumed to be an identity matrix of dimesion \eqn{n.}  The estimate \eqn{\Psi^{(T)}} at the last iteration converts to the estimated covariance with \eqn{\Sigma^{(T)}=\nu\Psi^{(T)}.}

A weighted version of this algorithm can be obtained replacing \eqn{G_{aa}} in above equations with \eqn{G^{(w_a)}_{aa}=w_aG_{aa}+(1-w_a)\nu\Psi^{(T)}} for a vector of weights \eqn{(w_1,w_2,\ldots, w_m)'.}
}
\value{
Combined covariance matrix estimate. if loglik is TRUE, the this is a list with first element equal to the covariance estimate, second element in the list is the path of the log-likelihood.
}
\references{
- Adventures in Multi-Omics I: Combining heterogeneous data sets via relationships matrices. Deniz Akdemir, Julio Isidro Sanchez. bioRxiv, November 28, 2019
}
\author{Deniz Akdemir // Maintainer: Deniz Akdemir
\email{deniz.akdemir.work@gmail.com}
}


\examples{
####Using Iris data for a simple example
data(iris)
colnames(iris)<-c("S.L","S.W","P.L","P.W","Species")
iris$Species
##Setting seed for reproducability.
set.seed(1234)

###The input of the CovComb is a list of partial 
#covariance matrices for the species 'virginica'.
CovList<-vector(mode="list", length=3)
CovList[[1]]<-cov(iris[sample(101:150,20),c(1,2)])
CovList[[2]]<-cov(iris[sample(101:150,25),c(1,3)])
CovList[[3]]<-cov(iris[sample(101:150,30),c(2,4)])
###Note that the covariances between the variables 
##1 and 2, 2 and 3, and 3 and 4 are not observed in 
##the above. We will use these covariance matrices 
##to obtain a 4 by 4 covariance matrix that estimates 
##these unobserved covariances.

library(CovCombR)
outCovComb<-CovComb(CovList, nu=40)
###
#####Compare the results with what we would get
#if we observed all data. 
outCovComb
cov(iris[101:150,1:4])

####Compare the same based on correlations.
cov2cor(outCovComb)
cov2cor(cov(iris[101:150,1:4]))

####Here is a simple plot for visual comparison.

image(cov2cor(outCovComb),xlab="", ylab="", axes = FALSE, main="Combined")
axis(1, at = seq(0, 1, length=4),labels=rownames(outCovComb), las=2)
axis(2, at = seq(0, 1, length=4),labels=rownames(outCovComb), las=2)
image(cov2cor(cov(iris[101:150,1:4])),xlab="", ylab="", axes = FALSE,
main="All Data")
axis(1, at = seq(0, 1, length=4),labels=colnames(iris[,1:4]), las=2)
axis(2, at = seq(0, 1, length=4),labels=colnames(iris[,1:4]), las=2)

\donttest{

#### Using Weights
outCovCombhtedwgt<-CovComb(CovList, nu=75,w=c(20/75,25/75,30/75))
cov2cor(outCovCombhtedwgt)



####Refit and plot log-likelihood path
outCovCombhtedwgt<-CovComb(CovList, nu=75,w=c(20/75,25/75,30/75),
loglik=TRUE, plotll=TRUE)



#### For small problems (when the sample size
## moderate and the number of variables is small),
## we can try using optimization to estimate the degrees of freedom 
## parameter nu. Nevetheless, this is not always satisfactory. 
## The value of nu does not change the 
## estimate of the covariance, but it is 
## important for evaluating estimation errors. 
negativellfornu<-function(nu){
outCovComb<-CovComb(CovList, nu=ceiling(nu), loglik=TRUE, plotll=FALSE)
return(-max(outCovComb[[2]]))
}


optout<-optimize(negativellfornu,interval=c(20,100),tol=1e-3)
est.df<-ceiling(optout$minimum)
est.df
#> est.df= 39


####### Estimated nu can be used as an input
## to other statistical procedures
## such as hypothesis testing about 
## the covariance parameters, graphical modeling, 
## sparse covariance estimation, etc,....
}
}

\keyword{multivariate}
\keyword{models}
\keyword{regression}
