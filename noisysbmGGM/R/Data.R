#' @title
#' NoisySBM for test
#' @description
#' Example of NoisySBM data
#'
#' @format
#' \describe{
#'   \item{\code{dataMatrix}}{A square matrix containing the observation of the graph}
#'   \item{\code{theta}}{True NSBM parameters}
#'   \item{\code{latentZ}}{True latent clustering}
#'   \item{\code{latentAdj}}{True latent adjacency matrix}
#'   }
#'
#' @examples
#' main_noisySBM(NSBMtest$dataMatrix,NIG=TRUE,Qup=10,nbOfZ=1,nbCores=1)
#'
#' #Note : These data were created using the following instructions
#' p=50
#' Q=6
#' pi=c(1/6,1/6,1/6,1/6,1/6,1/6)
#' w=c(0.811,0.001,0.001,0.001,0.001,0.001,0.811,0.011,0.001,0.001,0.001,
#' 0.811,0.001,0.001,0.001,0.811,0.001,0.001,0.811,0.011,0.811)
#' theta=list(pi=pi,w=w,nu0=c(0,1))
#' theta$nu <- array(0, dim = c(Q*(Q+1)/2, 2))
#' theta$nu[,1] <- rep(2,21)
#' theta$nu[,2] <- rep(2,21)
#' NSBMtest=rnsbm(p,theta)
#'
"NSBMtest"

#' @title
#' GGM for test
#' @description
#' Example of a GGM
#'
#' @format
#' \describe{
#'   \item{\code{dataMatrix}}{A n-sample of a p Gaussian Vector associated to a GGM G}
#'   \item{\code{Z.true}}{True latent clustering}
#'   \item{\code{A.true}}{True latent adjacency matrix of the graph G}
#'   }
#'
#' @examples
#' main_noisySBM_GGM(GGMtest$dataMatrix,Meth="Ren",NIG=TRUE,Qup=10,nbOfZ=1,nbCores=1)
#'
#' #Note : These data were created using the following instructions
#' n=30
#' p=10
#' u=0.1
#' v=0.3
#' theta=list(pi=c(1/3,2/3),w=0.25*cbind(c(1/6,1/120),c(1/120,1/6)))
#' Q=2
#' Z <- sample(1:Q, p, replace=TRUE, prob=theta$pi)
#' A <- matrix(0, p, p)
#' for (i in 1:(p-1)){
#'     A[i,(i+1):p] <- stats::rbinom(p-i, 1, theta$w[Z[i],Z[(i+1):p]])}
#'  A.true <- A + t(A)
#' Omega <- A.true*v
#' diag(Omega) = abs(min(eigen(Omega)$values)) + 0.1 + u
#' Sigma = stats::cov2cor(solve(Omega))
#' X = MASS::mvrnorm(n, rep(0, p), Sigma)
#' GGMtest=list(dataMatrix=X,Z.true=Z,A.true=A.true)
"GGMtest"

