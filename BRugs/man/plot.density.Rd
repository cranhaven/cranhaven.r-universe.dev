\name{plotDensity}
\alias{plotDensity}
\title{Plot density estimate or histogram of a scalar variable}
\description{This function plots a smoothed kernel density estimate for a scalar variable if it is continuous
    or a histogram if it is discrete.}
\usage{
plotDensity(node, plot=TRUE, main = NULL, xlab = "" , ylab = "", col = "red",
             ...)
}
\arguments{
  \item{node}{Character, name of a scalar variable in the model.}
  \item{plot}{Logical, whether to plot the trace or only return density estimates.
    If \code{TRUE}, density estimates are returned invisibly.}
  \item{main, xlab, ylab, col}{graphical parameters, see \code{\link{plot.default}}}
  \item{...}{Further graphical parameters as in \code{\link{par}}.}
}
\details{Acts on a scalar variable. See the wrapper function \code{\link{samplesDensity}} for more details.}
\seealso{\code{\link{samplesDensity}}, \code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\keyword{hplot}

