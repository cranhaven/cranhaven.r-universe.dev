\name{softthresh}
\alias{softthresh}
\title{Soft Threshold}
\encoding{utf-8}
\description{
  Soft threshold
}
\usage{
  softthresh(x, t)
}
\arguments{
  \item{x}{value}
  \item{t}{threshold}
}
\details{
  Pass \code{x} through a soft threshold with parameter \code{t}.
}
\value{A numeric scalar}
\examples{
  \dontrun{
    t <- 0.4
    plot(sapply(seq(-2,2,by=0.1),function (x) softthresh(x, t)),type='l')
  }
}
\keyword{noise}
\keyword{threshold}
