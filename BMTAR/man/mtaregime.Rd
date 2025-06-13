\name{mtaregime}
\alias{mtaregime}

\title{Object class \dQuote{\code{regime}} creation}
\description{
Create an object of class \dQuote{\code{regime}} given nonstructural and structural parameters for each regime.
}
\usage{
mtaregime(orders = list(p = 1,q = 0,d = 0), cs = NULL, Phi,
Beta = NULL, Delta = NULL, Sigma)
}
\arguments{
  \item{orders}{list type object with names (p,q,d), number of lags of Yt, Xt and Zt, respectively. Default p = 1, q = 0, d = 0}
  \item{cs}{matrix type object, the constant term of the regime specification. Default NULL}
  \item{Phi}{list type object with names (phi1, ..., phip), each one a matrix \eqn{(kxk)} type object autoregressive specification. Not NULL}
  \item{Beta}{list type object with names (beta1, ..., betaq), each one a matrix \eqn{(kx\nu)} type object covariate parameters specification Default NULL}
  \item{Delta}{list type object with names (delta1, ..., deltad), each one a matrix \eqn{(kx1)} type object parameter specification of Threshold process. Default NULL}
  \item{Sigma}{a positive-definite symmetric matrix \eqn{(kxk)}, specification of errors covariate matrix. Not NULL}
}
\details{
  Causes creation of the object class \dQuote{\code{regime}}. Sigma matrix corresponds to \eqn{\Sigma} (root of the covariance matrix). When cs is not specified or only matrices are delivered for some lags, the function assumes unspecified 0 (matrix). Rows number in the Phi, Beta and Delta matrix should be the same (k dimension of variables in Yt).
}
\value{
  Return list type object of class \dQuote{\code{regime}} with the values of the arguments
}
\references{
Calderon, S. and Nieto, F. (2017) \emph{Bayesian analysis of multivariate threshold autoregress models with missing data.} Communications in Statistics - Theory and Methods 46 (1):296–318. doi:10.1080/03610926.2014.990758.
}
\author{Valeria Bejarano \email{vbejaranos@unal.edu.co} & Andrey Rincon \email{adrincont@unal.edu.co}}

\seealso{\code{\link[BMTAR]{mtarsim}}}

\examples{
# Creation of parameters for regimen with orders = c(2,1,1), nu = 1 y k = 2.
## previous objects
orders = list(p = 2,q = 1,d = 1)
Phi = list(phi2 = matrix(c(0.1,0.6,-0.4,0.5),2,2, byrow = TRUE))
Beta = list(beta1 = matrix(c(0.3,-0.4),2, 1))
Delta = list(delta1 = matrix(c(0.6,1),2,1))
Sigma = matrix(c(1,0.6,0.6,1.5),2,2,byrow = TRUE)
cs = matrix(c(1,-1),nrow = 2)
## creacion de la clase regime
Ri = mtaregime(orders = orders,Phi = Phi,Beta = Beta,Delta = Delta,
Sigma = Sigma,cs = cs)
class(Ri)
}
\keyword{Threshold process}
\keyword{Multivariate threshold autoregressive model}
\keyword{Regime}

