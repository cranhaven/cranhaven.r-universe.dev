\name{exploreBic}
\alias{exploreBic}
\title{The exploreBic function}

\description{Provides exploratory plots for biclustered and clustered data.}
\usage{
exploreBic(dset, bres, gby ="genes", pfor ="mean", mname ="biclust", bnum =1, 
fabia.thresZ=0.5,fabia.thresL=NULL)
}
\arguments{
  \item{dset}{data matrix.}
  \item{bres}{bicluster result.}
  \item{gby}{dimension to plot; 'genes' or 'conditions'.}
  \item{pfor}{plot for 'mean', 'median', 'variance', 'mad', 'all', or 'quant' (quantile).}
  \item{mname}{method name; 'biclust', 'isa2', 'fabia' or 'bicare'}
  \item{bnum}{existing biclusters; '1','2'...}
  \item{fabia.thresZ}{Bicluster threshold for \code{mname="fabia"}. Threshold for sample belonging to bicluster; default 0.5.}
  \item{fabia.thresL}{Bicluster threshold for \code{mname="fabia"}. Threshold for loading belonging to bicluster (if not given it is estimated).}
  
}
\details{The exploreBic function is mainly used for exploratory data analysis. It provides summary plots for mean, median, variance, MAD and quantile plot.

The \code{\link{exploreBic}} function checks if the parameters are appropriately submitted and then identifies the biclusters submatrix and calculates its summary statistics. Finally, the results are displayed on the required plot.

Note that the \code{"biclust"} option for \code{mname} will also accept results from the packages \pkg{iBBiG} and \pkg{rqubic}.
}
\value{
Summary plot will display according to the user specification.
}

\author{Mengsteab Aregay \email{mycs.zab@gmail.com}}
\references{
  Van't Veer, L.J., Dai, H., van de Vijver, M.J., He, Y.D., Hart, A.A. et al. (2002). Gene expression profiling predicts clinical outcome of breast cancer,\emph{Nature}, 415, 530-536.

  Hochreiter, S., Bodenhofer, U., Heusel, M.\emph{et al.} (2010).FABIA: factor analysis for bicluster acquisition. \emph{Bioinformatices}, 26, 1520-1527.
  
}

\seealso{\code{\link{exploreOnlybic}}}
\examples{
data(breastc)
# find bicluster using biclust package
library(biclust)
bic <- biclust(breastc,method=BCPlaid())
# Plot the mean of biclusterd and clustered genes parallely.
exploreBic(dset=breastc,bres=bic,gby="conditions",pfor="mean",mname="biclust")
}
\keyword{exploreBic}
