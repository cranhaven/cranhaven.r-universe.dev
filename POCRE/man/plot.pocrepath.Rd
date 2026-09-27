\name{plot.pocrepath}
\alias{plot.pocrepath}
\title{
Visulaization of a POCRE Path
}
\description{
For a series models built by POCRE for different tuning paramter values, it provides three types of plots to help select an appropriate tuning parameter value.
}
\usage{
\method{plot}{pocrepath}(x, which=1:3, cex=.5, lwd=1, ...)
}
\arguments{
  \item{x}{
a \link{pocrepath} object, i.e., the result from \link{pocrepath}.
}
  \item{which}{
  1 for plotting the tuning parameter vs. (beta, #[beta!=0]), 2 for plotting the tuning parameter vs. (beta, R^2), 3 for plotting the tuning parameter vs. (R^2, #[beta!=0]).
}
 \item{cex}{
A numerical value giving the amount by which plotting text and symbols should be magnified relative to the default, see \link[graphics]{par}.
}
 \item{lwd}{
line width, see \link[graphics]{par}.
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
\code{\link{pocrepath}}, \code{\link{plot.pocre}}, \code{\link{pocre}}.
}
\examples{
data(simdata)
xx <- scale(as.matrix(simdata[,-1]))
yy <- scale(as.matrix(simdata[,1]))

# ppres <- pocrepath(yy, xx, delta=0.01)
ppres <- pocrepath(yy, xx)

# plot(ppres)
plot(ppres,which=3)
}
