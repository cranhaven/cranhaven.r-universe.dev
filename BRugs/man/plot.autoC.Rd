\name{plotAutoC}
\alias{plotAutoC}
\title{Plot autocorrelation function for a scalar variable}
\description{This function plots the autocorrelation function of a scalar variable.}
\usage{
plotAutoC(node, plot = TRUE, 
    colour = c("red", "blue", "green", "yellow", "black"), 
    lwd = 5, main = NULL, ...)
}
\arguments{
  \item{node}{Character, name of a scalar variable in the model.}
  \item{plot}{Logical, whether to plot the ACF or only return the values. If \code{TRUE}, values are returned invisibly.}
  \item{colour}{Colours used to represent different chains.}
  \item{lwd, main}{graphical parameters, see \code{\link{plot.default}}}  
  \item{...}{Further graphical parameters as in \code{\link{par}}.}  
}
\value{
An \code{\link{acf}} object. See \code{\link{acf}} for details.
}
\details{Acts on a scalar variable. See the wrapper function \code{\link{samplesAutoC}} for more details.}
\seealso{\code{\link{samplesAutoC}}, \code{\link{acf}}, \code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\keyword{hplot}

