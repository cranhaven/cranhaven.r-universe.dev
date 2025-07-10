\name{BCs4vd}
\alias{BCs4vd}
\alias{s4vd}
\alias{BCs4vd-class}
\alias{\S4method{biclust}{matrix,BCs4vd}}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Robust biclustering by sparse singular value decomposition incorporating stability selection
}
\description{
The function performs biclustering of the data matrix by sparse singular value decomposition with nested stability selection.
}
\usage{
\S4method{biclust}{matrix,BCs4vd}(x, method=BCs4vd(),
		steps = 100,
		pcerv = 0.05,
		pceru = 0.05,
		ss.thr = c(0.6,0.65),
		size = 0.632,
		gamm = 0,
		iter = 100,
		nbiclust = 10,
		merr = 10^(-4),
		cols.nc=FALSE,
		rows.nc=TRUE,
		row.overlap=TRUE,
		col.overlap=TRUE,
		row.min=4,
		col.min=4,
		pointwise=TRUE,
		start.iter=0,
		savepath=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The matrix to be clustered.
}
  \item{method}{
calls the BCs4vd() method
}
  \item{steps}{
Number of subsamples used to perform the stability selection.
}
  \item{pcerv}{
Per comparsion wise error rate to control the number of falsely selected right singular vector coefficients (columns/samples).
}
  \item{pceru}{
Per comparsion wise error rate to control the number of falsely selected left singular vector coefficients (rows/genes).
}
  \item{ss.thr}{
Range of the cutoff threshold (relative selection frequency) for the stability selection.
}
  \item{size}{
Size of the subsamples used to perform the stability selection.  
}
  \item{gamm}{
Weight parameter for the adaptive LASSO, nonnegative constant (default = 0, LASSO).
}
  \item{iter}{
Maximal number of iterations to fit a single bicluster.
}
  \item{nbiclust}{
Maximal number of biclusters. 
}
  \item{merr}{
Threshold to decide convergence. 
}
  \item{cols.nc}{
Allow for negative correlation of columns (samples) over rows (genes).
}
  \item{rows.nc}{
Allow for negative correlation of rows (genes) over columns (samples).
}
  \item{row.overlap}{
Allow rows to overlap between biclusters. 
}
  \item{col.overlap}{
Allow columns to overlap between biclusters. 
}
  \item{row.min}{
Minimal number of rows.
}
  \item{col.min}{
Minimal number of columns.
}
  \item{pointwise}{
If TRUE performs a fast pointwise stability selection instead of calculating the complete stability path.  
}
  \item{start.iter}{
Number of starting iterations in which the algorithm is not allowed to converge. 
}
  \item{savepath}{
Saves the stability path in order plot the path with the stabpathplot function.
Note that pointwise needs to be TRUE to save the path. For extreme high dimensional data sets (e.g. the lung cancer example) the resulting
biclust object may exceed the available memory.
}
}
\value{
Returns an object of class \code{Biclust}.
}
\author{
Martin Sill \
\email{m.sill@dkfz.de}
}
\references{
Martin Sill, Sebastian Kaiser, Axel Benner and Annette Kopp-Schneider
"Robust biclustering by sparse singular value decomposition incorporating stability selection", Bioinformatics, 2011 
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

\keyword{biclustering}
\keyword{svd}
\keyword{stability selection}
