\name{summary.hglasso}
\alias{summary.hglasso}
\alias{summary.hcov}
\alias{summary.hbn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot an object of class \code{hglasso}, \code{hcov}, or \code{\link{hbn}}
}
\description{
This function provides some information for an object \code{\link{hglasso}}, \code{\link{hcov}}, or \code{\link{hbn}}.
}
\usage{
\method{summary}{hglasso}(object, \dots) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
an object of class \code{\link{hglasso}}, \code{\link{hcov}}, or \code{\link{hbn}}.
}

  \item{\dots}{
any other arguments passed to \code{print}.
}

}
\details{
Some information for an object hglasso, hcov, or hbn: (1) The number of observations n and the number of features p.  (2) The number of edges in Theta, V, and Z. (3) The indices for hub nodes, and also the number of edges within each hub node.}

\references{
Tan et al. (2014). Learning graphical models with hubs. Journal of Machine Learning Research 15(Oct):3297-3331.
}
\author{
Kean Ming Tan 
}


\seealso{
\code{\link{image.hglasso}}
\code{\link{plot.hglasso}}
\code{\link{hglasso}}
\code{\link{hcov}}
\code{\link{hbn}}
}
\examples{
# See example in hglasso, hcov, or hbn.
}

