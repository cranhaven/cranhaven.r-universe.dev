\name{plapply}
\alias{plapply}
\title{Parallell list apply}
\encoding{utf-8}
\description{Parallell list apply}
\usage{
plapply(X1, X2, FUN, \dots)
}
\arguments{
  \item{X1}{a vector (atomic or list). Other objects (including classed objects) will be coerced by \code{\link{as.list}}.}
  \item{X2}{See \code{X1}.}
  \item{FUN}{the function to be applied to each pair of \code{X1} and \code{X2} at the corresponding positions.}
\item{\dots}{optional arguments to \code{FUN}.}
}

\details{
  \code{FUN} is found by a call to \code{\link{match.fun}} and typically is specified as a function or a symbol (e.g. a backquoted name) or a character string specifyign a function to be searched for from the environment of the call to \code{plapply}.

  Function \code{FUN} must be able to accept as input any of the element pairs of \code{X1} and \code{X2}. If any of these are atomic vectors, \code{FUN} will always be passed a length-one vector of the same type as \code{X1}, \code{X2} respectively.

  Users of S4 classes should pass a list to \code{plapply}: the internal coercion is done by the system \code{\link{as.list}} in the base namespace and not one defined by a user (e.g. by setting S4 methods on the system function).
}
\value{
  A list.
}
\seealso{
  \code{\link{lapply}}
}
\examples{
X1 <- array(1:4,dim=c(2,2))
X2 <- array(5:8,dim=c(2,2))
X3 <- array(9:12,dim=c(2,2))
X4 <- array(13:16,dim=c(2,2))
l <- plapply(list(X1,X2),list(X3,X4), function(E1,E2) E2 - E1)
stopifnot(all(sapply(l,sum)/4 == 4*2))
}
\keyword{iteration}
\keyword{list}

