\name{BCCC}
\title{The CC Bicluster algorithm}
\alias{BCCC}
\alias{CC}
\alias{BCCC-class}
\alias{biclust,matrix,BCCC-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\description{Performs CC Biclustering based on the framework by Cheng and Church (2000). Searches for submatrices with a score lower than a specific treshold in a standardized data matrix. }
\usage{
\S4method{biclust}{matrix,BCCC}(x, method=BCCC(), delta = 1.0, alpha=1.5, number=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Data matrix.}
  \item{method}{Here BCCC, to perform CC algorithm}
  \item{delta}{Maximum of accepted score.}
  \item{alpha}{Scaling factor.}
  \item{number}{Number of bicluster to be found.}
}

\value{
  Returns an object of class \code{Biclust}.
}
\author{Sebastian Kaiser 
\email{sebastian.kaiser@stat.uni-muenchen.de}
}

\references{
Cheng, Y. & Church, G.M. 
Biclustering of Expression Data 
Proceedings of the Eighth International Conference on Intelligent Systems for Molecular Biology, 
2000, 1, 93-103


}

\seealso{\code{\link{biclust}}, \code{\link{Biclust}}}
\examples{
test <- matrix(rbinom(400, 50, 0.4), 20, 20)
res <- biclust(test, method=BCCC(), delta=1.5,  alpha=1, number=10)
res
}

\keyword{cluster}
\keyword{classif}
