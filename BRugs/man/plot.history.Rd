\name{plotHistory}
\alias{plotHistory}
\title{Trace of a scalar variable}
\description{This function returns and plots a complete trace for a scalar variable.}
\usage{
plotHistory(node, plot = TRUE, 
    colour = c("red", "blue", "green", "yellow", "black"), 
    main = NULL, xlab = "iteration", ylab = "", ...)
}
\arguments{
  \item{node}{Character, name of a scalar variable in the model.}
  \item{plot}{Logical, whether to plot the trace or only return the values. 
    If \code{TRUE}, values are returned invisibly.}
  \item{colour}{Colours used to represent different chains.}
  \item{main, xlab, ylab}{graphical parameters, see \code{\link{plot.default}}}  
  \item{...}{Further graphical parameters as in \code{\link{par}}.}  
}
\value{
A matrix containing samples of \code{node}, each row corresponds to one chain.
}
\details{Acts on a scalar variable. See the wrapper function \code{\link{samplesHistory}} for more details.}
\seealso{\code{\link{samplesHistory}}, \code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\keyword{hplot}

