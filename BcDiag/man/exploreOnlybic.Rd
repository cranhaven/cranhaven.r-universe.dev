\name{exploreOnlybic}
\alias{exploreOnlybic}
\title{The exploreOnlybic function}

\description{Provides exploratory plots only for biclustering results.}
\usage{
exploreOnlybic(dset, bres, pfor= "all", gby= "genes", mname="biclust",bnum=1, 
fabia.thresZ=0.5,fabia.thresL=NULL)
}
\arguments{
  \item{dset}{data matrix.}
  \item{bres}{biclustering result.}
  \item{gby}{group bicluster; 'genes' or 'conditions'.}
  \item{pfor}{fit a plot for 'mean', 'median', 'variance', 'mad', 'all', or 'quant' (quantile).}
  \item{mname}{method name; 'biclust', 'isa2', 'fabia' or 'bicare'.}
  \item{bnum}{existing biclusters; '1','2'...}
  \item{fabia.thresZ}{Bicluster threshold for \code{mname="fabia"}. Threshold for sample belonging to bicluster; default 0.5.}
  \item{fabia.thresL}{Bicluster threshold for \code{mname="fabia"}. Threshold for loading belonging to bicluster (if not given it is estimated).}
}
\details{The exploreOnlybic function has similar function with \code{\link{exploreBic}}. The only difference is that it provides exploratory plots only for biclustered data.}
\value{
Summary plot will display only for biclustered data.

Note that the \code{"biclust"} option for \code{mname} will also accept results from the packages \pkg{iBBiG} and \pkg{rqubic}.
}

\author{Mengsteab Aregay \email{mycs.zab@gmail.com}}
\references{
  Van't Veer, L.J., Dai, H., van de Vijver, M.J., He, Y.D., Hart, A.A. et al. (2002). Gene expression profiling predicts clinical outcome of breast cancer,\emph{Nature}, 415, 530-536.

  Hochreiter, S., Bodenhofer, U., Heusel, M.\emph{et al.} (2010).FABIA: factor analysis for bicluster acquisition. \emph{Bioinformatices}, 26, 1520-1527.
  
}

\seealso{
\code{\link{exploreBic}}}
\examples{
data(breastc)
# find bicluster using biclust algorithm
library(biclust)
bic <- biclust(breastc,method=BCPlaid())
# Plot the median of biclusterd data.
exploreOnlybic(dset=breastc, bres=bic, pfor="all", gby="genes", mname="biclust", bnum=1)
}
\keyword{exploreOnlybic}
