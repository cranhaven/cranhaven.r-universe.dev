\name{distShell}
\alias{distShell}

\title{
Distance Matrix Computation
}
\description{
Produces a 'dist' object using a user-defined distance measure.
}
\usage{
distShell(DATA, FUNC, diag = FALSE, upper = FALSE, ...)
}
\arguments{
  \item{DATA}{
a matrix-like object with variables in COLUMNS, cases in ROWS.
}
  \item{FUNC}{
the distance function; takes two vector arguments and returns a single scalar distance measure. See Details.
}
  \item{diag}{
logical value indicating whether the diagonal of the distance matrix should be printed by print.dist.
}
  \item{upper}{
logical value indicating whether the upper triangle of the distance matrix should be printed by print.dist.
}
  \item{\dots}{
further arguments, passed to FUNC.
}
}
\details{
FUNC must be a function of the form foo(x1, x2, ...). The first two arguments must be vectors of equal length, specifying the two cases to be compared. It must return a single scalar distance measure. Similarity measures will work, but for consistency stick to distance measures.

A number of example functions are provided in the package; see \code{\link{Distance Measures}}.
}
\value{
\code{distShell} returns an object of class \code{"dist"}, including the attribute \code{call}. 

See \code{\link{dist}} for details of this class.
}
\author{
Mike Meredith, 10 Dec 2006, updated 1 Sept 2012.
}
\seealso{
\code{\link{dist}} in package stats. Also \code{vegan::vegdist} and \code{labdsv::dsvdis}. See \code{\link{Distance Measures}} for details of plug-in functions.
}

\examples{
# Use the artificial data set, see ?distTestData
data(distTestData)

# Using distance measure functions in this package:
distShell(distTestData, distSorensen)
distShell(distTestData, distMorisitaHorn)

# Write a customised distance function:
K <- function(a1, a2)  {
  shared <- sum(a1 > 0 & a2 > 0)
  notshared <- sum(xor(a1 > 0, a2 > 0))
  shared / notshared
}
distShell(distTestData, K)
# This returns Inf if the number of species not shared is zero. May not be a good measure!
}

