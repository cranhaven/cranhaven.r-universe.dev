\name{sim.data.ts.single}
\alias{sim.data.ts.single}
\title{ Generate single-level simulation data
}
\description{ This function generates a single-level dataset with given parameters.
}
\usage{
sim.data.ts.single(n, Z, A, B, C, Sigma, W, Delta = NULL, p = NULL, nburn = 100)
}
\arguments{
  \item{n}{ an integer indicating the length of the time series.
}
  \item{Z}{ a vector of treatment/exposure assignment at each time point.
}
  \item{A}{ a numeric value of model coefficient.
}
  \item{B}{ a numeric value of model coefficient.
}
  \item{C}{ a numeric value of model coefficient.
}
  \item{Sigma}{ a 2 by 2 matrix, is the covariance matrix of the two Gaussian white noise processes.
}
  \item{W}{ a \eqn{2p} by 2 matrix, is the transition matrix.
}
  \item{Delta}{ a 2 by 2 matrix, is the covariance matrix of the initial condition. Default is \code{NULL}, will be the same as \code{Sigma}.
}
  \item{p}{ a numeric value indicating the order of the vector autoregressive (VAR) model. Default is \code{NULL}, will be calculated based on \code{W}.
}
  \item{nburn}{ a integer indicating the number of burning sample. Default is 100.
}
}
\details{ The single level GMA model is
  \deqn{M_{t}=Z_{t}A+E_{1t},}
  \deqn{R_{t}=Z_{t}C+M_{t}B+E_{2t},}
  and for stochastic processes \eqn{(E_{1t},E_{2t})},
  \deqn{E_{1t}=\sum_{j=1}^{p}\omega_{11_{j}}E_{1,t-j}+\sum_{j=1}^{p}\omega_{21_{j}}E_{2,t-j}+\epsilon_{1t},}
  \deqn{E_{2t}=\sum_{j=1}^{p}\omega_{12_{j}}E_{1,t-j}+\sum_{j=1}^{p}\omega_{22_{j}}E_{2,t-j}+\epsilon_{2t}.}
  \code{Sigma} is the covariance matrix of the Gaussian white noise \eqn{(\epsilon_{1t},\epsilon_{2t})}, and \code{Delta} is the covariance matrix of \eqn{(\epsilon_{10},\epsilon_{20})}. \code{W} is the transition matrix with element \eqn{\omega}'s.
}
\value{ The function returns a list with two data frames. One is the data with variables \code{Z}, \code{M} and \code{R}; one is the data frame of \eqn{(E_{1t},E_{2t})}.
}
\references{Zhao, Y., & Luo, X. (2017). \emph{Granger Mediation Analysis of Multiple Time Series with an Application to fMRI.} arXiv preprint arXiv:1709.05328.
}
\author{ Yi Zhao, Brown University, \email{zhaoyi1026@gmail.com};
  Xi Luo, Brown University, \email{xi.rossi.luo@gmail.com}
}
\examples{
###################################################
# Generate a single-level dataset

# covariance matrix of errors
delta<-0.5
Sigma<-matrix(c(1,2*delta,2*delta,4),2,2)

# model coefficients
A0<-0.5
B0<--1
C0<-0.5

# number of time points
n<-500

# generate a treatment assignment vector
set.seed(1000)
Z<-matrix(rbinom(n,size=1,prob=0.5),n,1)

# VAR(1) model
p<-1

# Delta and W matrices
Delta<-matrix(c(2,delta*sqrt(2*8),delta*sqrt(2*8),8),2,2)
W<-matrix(c(-0.809,0.154,-0.618,-0.5),2,2)

# number of burning samples
nburn<-1000

set.seed(1000)
data.single<-sim.data.ts.single(n,Z,A0,B0,C0,Sigma,W,Delta,p=p,nburn=nburn)
###################################################
}
\keyword{ datagen }
