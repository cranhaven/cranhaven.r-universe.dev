\name{memrobmatrix-class}
\docType{class}
\alias{memrobmatrix-class}
\alias{memrobmatrix}

\title{Class "memrobmatrix"}
\description{
Objects of class \code{'memrobmatrix'} hold the full membership robustness matrix generated from analysis of a consensus matrix. This
includes the calculations of membership robustness for all features (e.g. genes) for each cluster. This can be useful as it allows you to
see what conritbution a particular feature (e.g. gene) is making to other clusters. This could resonably be thought of as a measure similar
to 'fuzziness' i.e. partial cluster membership. If the value of the membership robustness for a feature is similar in many clusters then that
is additional evidence that the feature is not easily placed in any cluster.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("memrobmatrix", ...)}, although they are usually generated internally by the \code{\link{memrob}} function. 
}
\section{Slots}{
  \describe{
    \item{\code{mrm}:}{Object of class \code{"matrix"} - this is the full membership robustness matrix itself and therefore has the same dimensions as the
    original data object used in the clustering}
  }
}

\references{
Merged consensus clustering to assess and improve class discovery with microarray data. Simpson TI, Armstrong JD and Jarman AP. BMC Bioinformatics 2010, 11:590.
}

\author{
Dr. T. Ian Simpson
\email{ian.simpson@ed.ac.uk}
}

\seealso{
Also see the \code{\link{memrob}} function.
}
\examples{
showClass("memrobmatrix")

#load cmr
data(testcmr);

#calculate membership robustness
mr <- memrob(testcmr$e1_kmeans_k3)

#get the full membership robustness matrix (matrix itself held in slot 'mrm')
mrm <- mr$resultmatrix@mrm;

}
\keyword{classes}