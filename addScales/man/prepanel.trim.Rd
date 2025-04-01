\name{prepanel.trim}
\alias{prepanel.trim}

\title{
Lattice Prepanel Function to Trim Panel Limits
}
\description{
Trims numeric x and or y limits to specified quantiles. May be useful when unusual extreme values distort the scales and obscure informative features of the data. Scales for factors are not affected.
}
\usage{
prepanel.trim(x, y, trim.x = 0, trim.y = 0.05, min.trim = 20, ...)
}
\arguments{
  \item{x, y}{
      x and y values, numeric or factor.
   }
  \item{trim.x, trim.y}{
      Numeric trimming proportions, p, with 0 <= p < .5 .\cr
      \code{trim.x} and \code{trim.y} can be different.
   }
  \item{min.trim}{
      The minimum number of data values needed before trimming
      \emph{after removing} \samp{NAs} and \samp{Infs}. Otherwise the
      range of the data is returned (min and max of all the remaining finite
      values).
   }
  \item{\dots}{
      Other arguments, usually ignored
   }
}

\details{
If the trimming proportion is p, the limits returned are essentially
\code{quantile(p, 1-p, type = 8)}. See \code{\link{quantile}} for details.
So, for example if p = .1, roughly 10\% of the lowest and 10\% of the highest values are removed, and the range of the middle 80\% of the data are returned. More precisely (quoting from \code{\link{xyplot}}), \dQuote{\ldots{} the actual limits of the panels are guaranteed to include the limits returned by the prepanel function} -- i.e., these quantiles.
}
\value{
For numeric data, a numeric vector of length 2, as would be returned by
\code{range}. For a factor, a list with components \code{yat} and \code{ylim},
as described in the \code{prepanel} section of \code{\link{xyplot}}
}
\author{
Bert Gunter \email{bgunter.4567@gmail.com}
}
\note{
No banking calculations are done.
}

\seealso{
 \code{\link{xyplot}}, \code{\link{prepanel.default.xyplot}},
 \code{\link{quantile}}
}

\keyword{ dplot }
\keyword{ hplot }
