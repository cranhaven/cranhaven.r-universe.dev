\name{plot.pocre}
\alias{plot.pocre}
\title{
Visualization of a \code{pocre} Object
}
\description{
Plot the regression coefficients, and the loadings of all components for a fitted model by POCRE.
}
\usage{
\method{plot}{pocre}(x, x.id = NA, which=1:2, cex=.5, ...)
}
\arguments{
  \item{x}{
a \link{pocre} object, i.e., the result from \link{pocre}.
}
  \item{x.id}{
a vector indicating the indices or positions of the covariates in the original data.
}
  \item{which}{
  1 for plotting the regression coefficients, 2 for plotting the loadings of all components.
}
  \item{cex}{
A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default, see \link[graphics]{par}.
}
\item{...}{
additional arguments accepted by \link[ggplot2]{ggplot}.
}
}
\references{
Zhang D (2018). R package POCRE: Exploring high-dimensional data via supervised dimension reduction. Manuscript.

Zhang D, Lin Y, and Zhang M (2009). Penalized orthogonal-components regression for large p small n data. \emph{Electronic Journal of Statistics}, 3: 781-796.
}
\author{Dabao Zhang, Zhongli Jiang, Zeyu Zhang, Department of Statistics, Purdue University}
\seealso{
\code{\link{pocre}}, \code{\link{plot.pocrepath}}, \code{\link{pocrepath}}.
}
\examples{
data(simdata)
xx <- scale(as.matrix(simdata[,-1]))
yy <- scale(as.matrix(simdata[,1]))

##Fit with pocre()
pres <- pocre(yy, xx, lambda=0.9)

# plot(pres,which=1)
plot(pres)
}
