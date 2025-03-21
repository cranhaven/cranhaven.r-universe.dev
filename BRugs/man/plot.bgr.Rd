\name{plotBgr}
\alias{plotBgr}
\title{Plot the Gelman-Rubin convergence statistic for a scalar variable}
\description{This function calculates and plots the Gelman-Rubin convergence statistic for a scalar variable, 
    as modified by Brooks and Gelman (1998).
}
\usage{
plotBgr(node, plot = TRUE, main = NULL, xlab = "iteration", 
    ylab = "bgr", col = c("red", "blue", "green"), bins = 50, 
    ...)
}
\arguments{
  \item{node}{Character, name of a scalar variable in the model.}
  \item{plot}{Logical, whether to plot the BGR statistics or only return the values. 
    If \code{TRUE}, values are returned invisibly.}
  \item{main, xlab, ylab}{annotation, see \code{\link{plot.default}}}
  \item{col}{Colours, see Details Section in \code{\link{samplesBgr}}.}
  \item{bins}{Number of blocks}
  \item{...}{Further graphical parameters as in \code{\link{par}}.}
}
\value{
Data frame with elements 
    \item{Iteration}{end iteration of corresponding bin}
    \item{pooledChain80pct)}{80pct interval (normalized) of pooled chains}
    \item{withinChain80pct}{80pct interval (normalized) of mean within chain}
    \item{bgrRatio}{BGR ratio}
}
\details{Acts on a scalar variable. See the wrapper function \code{\link{samplesBgr}} for more details.}
\seealso{\code{\link{samplesBgr}}, \code{\link{BRugs}}, \code{\link{help.WinBUGS}}}
\keyword{interface}
\keyword{hplot}

