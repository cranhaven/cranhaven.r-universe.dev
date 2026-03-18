\name{fechner-internal}
\alias{fechner-internal}
\alias{geodesic.chains.loops}
\concept{geodesic chains}
\concept{geodesic loops}
\alias{shortest.paths.information}
\concept{shortest paths}
\title{Internal fechner Functions}
\description{
  The two internal functions of the \pkg{fechner} package used for
  computing intermediate graph-theoretic information.
}
\usage{
geodesic.chains.loops(M)
shortest.paths.information(M)
}
\details{
  These functions are not intended to be called by the user.  They are
  for computing intermediate graph-theoretic information, processed to
  give the final Fechnerian scaling results.
}
\note{
  There are also the internal functions \code{\link{plot.fechner}},
  \code{\link{print.fechner}}, and \code{\link{summary.fechner}}, the
  \code{plot}, \code{print}, and \code{summary} methods for objects of
  the class \code{fechner}, and the internal function
  \code{\link{print.summary.fechner}}, the \code{print} method for
  objects of the class \code{summary.fechner}.  These are documented,
  respectively, at the afore mentioned links.
}
\keyword{internal}
