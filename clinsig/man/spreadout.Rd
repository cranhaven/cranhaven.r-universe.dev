\name{spreadout}
\alias{spreadout}
\title{Spread numeric values out to a minimum spacing.}
\description{
 Spreads a vector of numeric values out by increasing any intervals smaller than
 \samp{mindist}.
}
\usage{
 spreadout(x,mindist)
}
\arguments{
  \item{x}{A numeric vector that may contain NAs.}
  \item{mindist}{The minimum interval allowed in the output.}
}
\details{
 \samp{spreadout} orders the vector \samp{x} and begins at the middle, increasing
 any intervals between values to \samp{mindist}. The function is designed to avoid
 crowding of labels on a plot, but may have other uses.
}
\value{New values with a minimum interval of \samp{mindist}. NAs are preserved.}
\author{Jim Lemon}
\keyword{misc}
