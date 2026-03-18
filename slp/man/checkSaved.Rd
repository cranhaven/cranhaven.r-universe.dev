\name{checkSaved}
\alias{checkSaved}
\title{Determine if combination of N, W and K is available on disk, or must be computed}
\description{
    Given values for \code{N}, \code{W} and \code{K}, determine if the combination is 
    available on disk as a \code{data()} load. If so, significant time savings can be
    obtained, especially for large \code{N}. 
}
\usage{
checkSaved(N, W, K)
}
\arguments{
  \item{N}{the length of the time index array, in units of \code{deltat}.}
  \item{W}{the time bandwidth, \bold{in units of} \code{df/}year. This is \bold{not}
      the same units as a \code{slp(...)} call, and \code{W} should be an integer, 
      e.g., 6 \code{df/}year.}
  \item{K}{the number of basis vectors requested. }
}
\details{
    Does a lookup against \code{data(slpSavedObjects)} to determine whether the
    combination of \code{N}, \code{W} and \code{K} is saved on disk as part of the
    package, and can be loaded. It is possible to create your own basis objects for
    particular choices of \code{N}, \code{W} and \code{K} and save them as part 
    of the library directory, updating \code{slpSavedObjects} as you do so. 
}
\value{
    Logical (\code{TRUE} or \code{FALSE}), indicating availability or lack thereof. 
}
\examples{
    # Examples using pkg:gam
    library("slp")
    checkSaved(N = 730, W = 6, K = 24)
    checkSaved(N = 365, W = 6, K = 13)
}
\keyword{models}
\keyword{smooth}
\keyword{regression}
\keyword{Slepian}
\keyword{gam}
