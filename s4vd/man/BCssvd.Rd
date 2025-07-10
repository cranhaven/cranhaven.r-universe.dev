\name{BCssvd}
\alias{BCssvd}
\alias{BCssvd-class}
\alias{ssvd}
\alias{\S4method{biclust}{matrix,BCssvd}}

\title{
Biclustering via sparse singular value decomposition 
}

\description{
The function performs a biclustering of the data matrix by sparse singular value decomposition.
}

\usage{
\S4method{biclust}{matrix,BCssvd}(x,method=BCssvd(),
	 K=10,
	 threu = 1,
	 threv = 1,
	 gamu = 0,
	 gamv =0,
	 u0 = svd(X)$u[,1],
	 v0 = svd(X)$v[,1],
	 merr = 10^(-4),
	 niter = 100)
}

\arguments{
  \item{x}{
the matrix to be clustered
}
  \item{method}{
calls the BCssvd() method
}
  \item{K}{
number of SSVD-layers 
}
  \item{threu}{
type of penalty (thresholding rule) for the left singular vector,
				 1 = (Adaptive) LASSO (default)
                 2 = hard thresholding
}
  \item{threv}{
type of penalty (thresholding rule) for the right singular vector,
                 1 = (Adaptive) LASSO (default)
                 2 = hard thresholding	
}
  \item{gamu}{
weight parameter in Adaptive LASSO for the left singular vector, nonnegative constant (default = 0, LASSO)
}
  \item{gamv}{
weight parameter in Adaptive LASSO for the right singular vector, nonnegative constant (default = 0, LASSO)
}
  \item{u0}{
initial left singular vector                
}
  \item{v0}{
initial right singular vector   
}
  \item{merr}{
threshold to decide convergence
}
  \item{niter}{
maximum number of iterations
}
}

\value{
Returns an Biclust object.
}
\references{
Mihee Lee, Haipeng Shen, Jianhua Z. Huang and J. S. Marron1
"Biclustering via Sparse Singular Value Decomposition", Biometrics, 2010 
}
\author{
Adaptation of original code from Mihee Lee by Martin Sill \
\email{m.sill@dkfz.de}
}

\seealso{ \code{\link{biclust}}, \code{\link{Biclust}}}

\examples{
\donttest{
# example data set according to the simulation study in Lee et al. 2010
# generate artifical data set and a correspondig biclust object
u <- c(10,9,8,7,6,5,4,3,rep(2,17),rep(0,75))
v <- c(10,-10,8,-8,5,-5,rep(3,5),rep(-3,5),rep(0,34))
u <- u/sqrt(sum(u^2)) 
v <- v/sqrt(sum(v^2))
d <- 50
set.seed(1)
X <- (d*u\%*\%t(v)) + matrix(rnorm(100*50),100,50)
params <- info <- list()
RowxNumber <- matrix(rep(FALSE,100),ncol=1)
NumberxCol <- matrix(rep(FALSE,50),nrow=1)
RowxNumber[u!=0,1] <- TRUE 
NumberxCol[1,v!=0] <- TRUE
Number <- 1
ressim <- BiclustResult(params,RowxNumber,NumberxCol,Number,info)

#perform s4vd biclustering 
ress4vd <- biclust(X,method=BCs4vd,pcerv=0.5,pceru=0.5,pointwise=FALSE,nbiclust=1,savepath=TRUE)
#perform s4vd biclustering with fast pointwise stability selection
ress4vdpw <- biclust(X,method=BCs4vd,pcerv=0.5,pceru=0.5,pointwise=TRUE,nbiclust=1)
#perform ssvd biclustering
resssvd <- biclust(X,BCssvd,K=1)
#agreement of the results with the simulated bicluster
jaccardind(ressim,ress4vd)
jaccardind(ressim,ress4vdpw)
jaccardind(ressim,resssvd)
}
}

\keyword{cluster}
\keyword{classif}
\keyword{svd}
