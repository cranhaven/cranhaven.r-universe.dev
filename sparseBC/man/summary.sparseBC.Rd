\name{summary.sparseBC}
\alias{summary.sparseBC}
\alias{summary.matrixBC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot an object of class \code{sparseBC} or \code{matrixBC}
}
\description{
This function provides some information for an object \code{\link{sparseBC}} or \code{\link{matrixBC}}
}
\usage{
\method{summary}{sparseBC}(object, \dots) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object of class \code{\link{sparseBC}} or \code{link{matrixBC}}
}

  \item{\dots}{
any other arguments passed to \code{print}.
}

}
\details{
Some information for an object sparseBC.  (1) Cluster labels for the rows/observations.  (2) Cluster labels for the columns/features.  (3) The estimated bicluster mean for each cluster.}  

\references{
#
}
\author{
Kean Ming Tan and Daniela Witten
}


\seealso{
\code{\link{sparseBC}}
\code{\link{matrixBC}}
\code{\link{image.sparseBC}}
\code{\link{sparseBC.BIC}}
\code{\link{sparseBC.choosekr}}
}
\examples{
# See example in sparseBC
}

