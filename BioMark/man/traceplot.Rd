\name{traceplot}
\alias{traceplot}
\title{Plot the coefficient or stability trace for the lasso/elastic net
  biomarker selection.}
\description{
The function plots the coefficient or stability traces for the lasso
element of a BioMark object.
}
\usage{
traceplot(object, ...)
}
\arguments{
  \item{object}{An object of class \code{BioMark}.}
  \item{\dots}{Further plotting arguments.}
}
\references{
N. Meinshausen and P. Buhlmann: Stability
Selection. J. R. Statist. Soc. B 72, 417-473 (2010)}
\author{Ron Wehrens}

\seealso{\code{\link{get.biom}}}
\examples{
library(BioMark)
data(spikedApples)

mycols <- rep("gray", ncol(spikedApples$dataMatrix))
mycols[spikedApples$biom] <- "red"
myltys <- rep(2, ncol(spikedApples$dataMatrix))
myltys[spikedApples$biom] <- 1

par(mfrow = c(1,2))
apple.coef <- get.biom(X = spikedApples$dataMatrix,
                       Y = factor(rep(0:1, each = 10)),
                       ncomp = 2:3, type = "coef")
traceplot(apple.coef, col = mycols, lty = myltys)

apple.stab <- get.biom(X = spikedApples$dataMatrix,
                       Y = factor(rep(0:1, each = 10)),
                       fmethod = c("vip","lasso"), type = "stab")
traceplot(apple.stab, col = mycols, lty = myltys)
}
\keyword{multivariate}
\keyword{models}
