\name{sim.data.ts.two}
\alias{sim.data.ts.two}
\title{ Generate two-level simulation data
}
\description{ This function generates a two-level dataset with given parameters.
}
\usage{
sim.data.ts.two(Z.list, N, theta, Sigma, W, Delta = NULL, p = NULL, 
  Lambda = diag(rep(1, 3)), nburn = 100)
}
\arguments{
  \item{Z.list}{ a list of data. Each list is a vector containing the treatment/exposure assignment at each time point for the subject.
}
  \item{N}{ an integer, indicates the number of subjects.
}
  \item{theta}{ a vector of length 3, contains the population level causal effect coefficients.
}
  \item{Sigma}{ a 2 by 2 matrix, is the covariance matrix of the two Gaussian white noise processes in the single level model.
}
  \item{W}{ a \eqn{2p} by 2 matrix, is the transition matrix in the single level model.
}
  \item{Delta}{ a 2 by 2 matrix, is the covariance matrix of the initial condition in the single level model. Default is \code{NULL}, will be the same as \code{Sigma}.
}
  \item{p}{ an integer, indicates the order of the vector autoregressive (VAR) model in the single level model. Default is \code{NULL}, will be calculated based on \code{W}.
}
  \item{Lambda}{ the covariance matrix of the model errors in the linear model of the model coefficients. Default is a 3 by 3 identity matrix.
}
  \item{nburn}{ a integer indicating the number of burning sample in the single level model. Default is 100.
}
}
\details{ For the time series of length \eqn{n_{i}} of subject \eqn{i}, the single level GMA model is
  \deqn{M_{i_{t}}=Z_{i_{t}}A_{i}+E_{i_{1t}},}
  \deqn{R_{i_{t}}=Z_{i_{t}}C_{i}+M_{i_{t}}B_{i}+E_{i_{2t}},}
  and
  \deqn{E_{i_{1t}}=\sum_{j=1}^{p}\omega_{i_{11_{j}}}E_{i_{1,t-j}}+\sum_{j=1}^{p}\omega_{i_{21_{j}}}E_{i_{2,t-j}}+\epsilon_{i_{1t}},}
  \deqn{E_{i_{2t}}=\sum_{j=1}^{p}\omega_{i_{12_{j}}}E_{i_{1,t-j}}+\sum_{j=1}^{p}\omega_{i_{22_{j}}}E_{i_{2,t-j}}+\epsilon_{i_{2t}},}
  where \code{Sigma} is the covariance matrix of \eqn{(\epsilon_{i_{1t}},\epsilon_{i_{2t}})} (for simplicity, \code{Sigma} is the same across subjects). For coefficients \eqn{A_{i}}, \eqn{B_{i}} and \eqn{C_{i}}, we assume a multivariate regression model. The model errors are from a trivariate normal distribution with mean zero and covariance \code{Lambda}.
}
\value{
  \item{data}{a list of data. Each list is a data frame of \eqn{(Z_{t},M_{t},R_{t})}.}
  \item{error}{a list of data. Each list is a data frame of \eqn{(E_{1t},E_{2t})}.}
  \item{A}{a vector of length \code{N}, the value of \eqn{A}s.}
  \item{B}{a vector of length \code{N}, the value of \eqn{B}s.}
  \item{C}{a vector of length \code{N}, the value of \eqn{C}s.}
  \item{type}{a character indicates the type of the dataset.}
}
\references{Zhao, Y., & Luo, X. (2017). \emph{Granger Mediation Analysis of Multiple Time Series with an Application to fMRI.} arXiv preprint arXiv:1709.05328.
}
\author{ Yi Zhao, Brown University, \email{zhaoyi1026@gmail.com};
  Xi Luo, Brown University, \email{xi.rossi.luo@gmail.com}
}
\examples{
###################################################
# Generate a two-level dataset

# covariance matrix of errors
delta<-0.5
Sigma<-matrix(c(1,2*delta,2*delta,4),2,2)

# model coefficients
A0<-0.5
B0<--1
C0<-0.5

theta<-c(A0,B0,C0)

# number of time points
N<-50
set.seed(2000)
n<-matrix(rpois(N,100),N,1)

# treatment assignment list
set.seed(1000)
Z.list<-list()
for(i in 1:N)
{
  Z.list[[i]]<-matrix(rbinom(n[i,1],size=1,prob=0.5),n[i,1],1)
}

# Lambda
Lambda<-diag(0.5,3)

# VAR(1) model
p<-1

# Delta and W matrices
Delta<-matrix(c(2,delta*sqrt(2*8),delta*sqrt(2*8),8),2,2)
W<-matrix(c(-0.809,0.154,-0.618,-0.5),2,2)

# number of burning samples
nburn<-1000

# set.seed(2000)
# data2<-sim.data.ts.two(Z.list,N,theta=theta,Sigma,W,Delta,p,Lambda,nburn)
###################################################
}
\keyword{ datagen }
