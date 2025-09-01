\name{find.tol}
\alias{find.tol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ An internal function that is not supposed to be directly accessed by the user. Find m/z tolerance level. }
\description{
  The function finds the tolerance level in m/z from a given vector of observed m/z values.
}
\usage{
find.tol(a, uppermost=1e-4, aver.bin.size=4000, min.bins=50, max.bins=200)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{ The vector of observed m/z values. }
  \item{uppermost}{ Consider only m/z diffs smaller than this value. }
  \item{aver.bin.size}{The average bin size to determine the number of equally spaced points in the kernel density estimation.}
  \item{min.bins}{the minimum number of bins to use in the kernel density estimation. It overrides aver.bin.size when too few observations are present.}
  \item{max.bins}{the maximum number of bins to use in the kernel density estimation. It overrides aver.bin.size when too many observations are present.}
}
\details{
  The method assumes a mixture model: an unknown distribution of m/z variations in the same peak, and an exponential distribution of between-peak diffs. The parameter of the exponential distribution is estimated by the upper 75% of the sorted data, and the cutoff is selected where the density of the empirical distribution is >1.5 times the density of the exponential distribution. 
}
\value{
  The tolerance level is returned.
}
\author{Tianwei Yu <tyu8@emory.edu>}
\examples{
data(prof)
find.tol(prof[[1]][,1])
}
\keyword{ models }