\name{full.model.mtx}
\alias{full.model.mtx}
\docType{package}
\title{
  Generate full model matrix for MEs and CMEs
}
\description{
  \code{full.model.mtx} returns the full model matrix for main effects (MEs) and conditional main effects (CMEs).
}
\usage{
  full.model.mtx(xme)
}
\arguments{
  \item{xme}{An \eqn{n} x \eqn{p} binary model matrix (\eqn{n} observations, \eqn{p} binary MEs).}
}
\value{
  \item{model.mtx}{An \eqn{n} x (\eqn{p}+4*choose(\eqn{p},2)) full model matrix for MEs and CMEs.}
  \item{cme.mtx}{An \eqn{n} x (4*choose(\eqn{p},2)) model matrix for only CMEs.}
}

\examples{
library(MASS)
n <- 50 #number of observations
p <- 50 #number of main effects

## Simulate model matrix for MEs and CMEs
set.seed(1)
rho <- 0 #correlation
ones <- matrix(1,p,p)
covmtx <- rho*ones+(1-rho)*diag(p)
latmtx <- mvrnorm(n,p,mu=rep(0,p),Sigma=covmtx) #equicorrelated cov. matrix
memtx <- (latmtx>=0)-(latmtx<0) #simulate model matrix for MEs
model.mtx <- full.model.mtx(memtx)$model.mtx #generate model matrix for MEs and CMEs

}
