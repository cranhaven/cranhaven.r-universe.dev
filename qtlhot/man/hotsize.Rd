\name{hotsize}
\alias{hotsize}
\alias{hotsize.scanone}
\alias{hotsize.highlod}
\alias{print.hotsize}
\alias{summary.hotsize}
\alias{plot.hotsize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hotspot size routines.
}
\description{
Determine hotspot sizes and display. Use individual threshold and
quantile thresholds as provided.
}
\usage{
hotsize(hotobject, \dots)
\method{hotsize}{scanone}(hotobject, lod.thr = NULL, drop.lod = 1.5, \dots)
\method{hotsize}{highlod}(hotobject, lod.thr = NULL, window = NULL,
  quant.level = NULL, \dots)
\method{print}{hotsize}(x, \dots)
\method{summary}{hotsize}(object, \dots)
\method{plot}{hotsize}(x, ylab = "counts", quant.axis = pretty(x$max.N),
  col = c("black", "red", "blue"), by.chr = FALSE, maps = NULL,
  title = "",\dots)
}
\arguments{
  \item{hotobject}{object of class \code{\link[qtl]{scanone}} or \code{\link{highlod}}}
  \item{lod.thr}{LOD threshold}
  \item{drop.lod}{LOD drop from max to keep for support intervals}
  \item{window}{window width in cM for smoothing hotspot size; not used
    if \code{0} or \code{NULL}}
  \item{quant.level}{vector of LOD levels for 1 up to
    \code{length(quant.level)} size hotspots}
  \item{x,object}{object of class \code{hotsize}}
  \item{ylab}{label for vertical plot axis}
  \item{quant.axis}{hotspot sizes for quantile axis (vertical on right
    side of plot)}
  \item{col}{col of hotspot size, smoothed hotspot size, and sliding
    hotspot size}
  \item{by.chr}{separate plot by chromosome if \code{TRUE}}
  \item{maps}{if not \code{NULL}, list of objects of class \code{map}
    to use for rugs on top and bottom of plot}
  \item{title}{title for plot}
  \item{\dots}{arguments passed along to scanone methods}
}
\seealso{\code{\link{highlod}}, \code{\link{hotperm}}}
\value{
  \code{hotsize} methods return an object of class \code{hotsize}, which
  is essentially an object of class \code{\link[qtl]{summary.scanone}}
  with additional attributes for \code{lod.thr}, \code{window}, and
  \code{quant.level}. 
}
\author{
Brian S Yandell and Elias Chaibub Neto
}
\examples{
example(highlod)
hots1 <- hotsize(high1)
summary(hots1)
plot(hots1)
}
\keyword{utilities}
