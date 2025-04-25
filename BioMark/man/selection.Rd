\name{selection}
\alias{selection}
\title{Accessor function to the selected variables of a BioMark object}
\description{Convenience function to get the indices of the selection in
  a BioMark object.}
\usage{selection(object, ...)}
\arguments{
  \item{object}{An object of class \code{BioMark}.}
  \item{\dots}{Further arguments, currently ignored.}
}
\value{A vector containing the indices of the selected variables.}
\seealso{\code{\link{get.biom}}}
\examples{
## stability-based selection
set.seed(17)
data(spikedApples)
apple.stab <- get.biom(X = spikedApples$dataMatrix,
                       Y = factor(rep(1:2, each = 10)),
                       ncomp = 2:3, type = "stab")
selected.variables <- selection(apple.stab)
}
\keyword{models}
\keyword{multivariate}
