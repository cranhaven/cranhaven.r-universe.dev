\name{memroblist-class}
\docType{class}
\alias{memroblist-class}
\alias{memroblist}

\title{Class "memroblist"}
\description{
Objects of class \code{'memroblist'} are created to hold the membership robustness scores for the features (e.g. genes) of a cluster.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("memroblist", ...)}, although these objects are normally created internally by the \code{\link{memrob}} function. 
}
\section{Slots}{
  \describe{
    \item{\code{mrl}:}{Object of class \code{"data.frame"} - the membership robustness list itself}
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
Also see the \code{\link{memrob}} function/
}
\examples{
showClass("memroblist")

#load a cmr
data(testcmr);

#calculate the membership robustness for agnes, k=4
mr <- memrob(testcmr$e2_agnes_k4);

#get a membership robustness list
mrl <- mr$cluster1;

}
\keyword{classes}
