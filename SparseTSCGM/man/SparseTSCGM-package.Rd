\name{SparseTSCGM-package}
\alias{SparseTSCGM-package}
\alias{SparseTSCGM}
\docType{package}
\title{
Sparse Time Series Chain Graphical Models.
}
\description{
Computes sparse autoregressive coefficient and precision matrices for time
 series chain graphical models(TSCGM). These models provide an effeicient way of 
 simultaneously dealing with Gaussian graphical models (undirected graphs for 
 instantaneous interactions) and Bayesian networks (directed graphs for dynamic
 interactions) for reconstructing instantaneous and dynamic networks from 
 repeated multivariate time series data. 
 }
\details{
\tabular{ll}{
Package: \tab SparseTSCGM\cr
Type: \tab Package\cr
Version: \tab 4.0\cr
Date: \tab 2021-01-12\cr
License: \tab GPL (>=3) \cr
LazyLoad: \tab yes\cr
}
}

\author{
Fentaw Abegaz and Ernst Wit

Maintainer: Fentaw Abegaz <f.abegaz.yazew@rug.nl>
}
\references{
\cite{Fentaw Abegaz and Ernst Wit (2013). Sparse time series chain graphical
  models for reconstructing genetic networks. \emph{Biostatistics}.
  \bold{14}, 3: 586-599.}
  
\cite{Rothman, A.J., Levina, E., and Zhu, J. (2010). Sparse multivariate regression 
  with covariance estimation. \emph{Journal of Computational and Graphical 
  Statistics}. \bold{19}: 947--962.}
}
\keyword{package}
\examples{

seed = 321
datas <- sim.data(model="ar1", time=10,n.obs=10, n.var=5,seed=seed,prob0=0.35,
         network="random")
data.fit <-  datas$data1
prec_true <- datas$theta
autoR_true <- datas$gamma
    
   
res.tscgm <- sparse.tscgm(data=data.fit, lam1=NULL, lam2=NULL, nlambda=NULL, 
 model="ar1", penalty="scad", optimality="bic_mod",
 control=list(maxit.out = 10, maxit.in = 100))
   
#Estimated sparse precision and autoregression matrices
prec <- res.tscgm$theta
autoR <- res.tscgm$gamma

#Graphical visualization
oldpar <- par(mfrow=c(2,2))
plot.tscgm(datas, mat="precision",main="True precision matrix")         
plot.tscgm(res.tscgm, mat="precision",main="Estimated precision matrix")     
plot.tscgm(datas, mat="autoregression",main="True autoregression coef. matrix")    
plot.tscgm(res.tscgm, mat="autoregression",
           main="Estimated autoregression coef. matrix") 
par(oldpar)
}
