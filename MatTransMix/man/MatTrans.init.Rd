\name{MatTrans.init}
\alias{MatTrans.init}
% Also NEED an '\alias' for EACH other topic documented here.
\title{ Initialization for the EM algorithm for matrix clustering}
\description{
Runs the initialization for the EM algorithm for matrix clustering}
\usage{
MatTrans.init(Y, K, n.start = 10, scale = 1)

}
% maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y }{ dataset of random matrices (p x T x n), n random matrices of dimensionality (p x T)}
  \item{K }{ number of clusters}
  \item{n.start }{ initial random starts}
  \item{scale }{ scaling parameter}
}
\details{
Random starts are used to obtain different starting values. The number of clusters, the skewness parameters, and number of random starts need to be specified. In the case when transformation parameters are not provided, the function runs the EM algorithm without any transformations, i.e., it is equivalent to the EM algorithm for a matrix Gaussian mixture.
Notation: n - sample size, p x T - dimensionality of the random matrices, K - number of mixture components. 
}
\value{
  \item{scale }{ scale parameter set by the user}
  \item{result }{ parsimonious models}
  \item{model }{ model types}
  \item{loglik }{ log likelihood values}
  \item{bic }{ bic values}
  \item{best.result }{ best parsimonious model}
  \item{best.model }{ best model type}
  \item{best.loglik }{ best logliklihood}
  \item{best.bic }{ best bic}
  \item{trans }{ transformation type}
}
\examples{
set.seed(123)
data(crime)
Y <- crime$Y[c(2,7),,] / 1000
p <- dim(Y)[1]
T <- dim(Y)[2]
n <- dim(Y)[3]
K <- 2
init <- MatTrans.init(Y, K = K, n.start = 2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Initialization }% __ONLY ONE__ keyword per line
