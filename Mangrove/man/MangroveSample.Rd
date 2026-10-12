\name{MangroveSample}
\alias{MangroveSample}
\alias{print.MangroveSample}
\alias{summary.MangroveSample}
\alias{plot.MangroveSample}
\title{
The MangroveSample class
}
\description{
Functions for manipulating MangroveSample objects
}
\usage{
\method{print}{MangroveSample}(x,...)
\method{summary}{MangroveSample}(object, ...)
\method{plot}{MangroveSample}(x,...)
}
\arguments{
  \item{x}{
	A \code{MangroveSample} object.
}
  \item{object}{
	A \code{MangroveSample} object.
}
  \item{\dots}{
	Further arguments to be passed on to downstream methods.
}
}
\details{
MangroveSample objects are produced from a \code{\link[Mangrove]{MangroveTree}} object via a call to \code{tree$getPrevs}. They contain samples from the posterior distribution of number of affecteds in a family conditional on that family's genotypes.

The \code{print} method writes all the samples to screen. You can plot the distribution of number of affecteds using the \code{plot} method (which calls \code{\link[Mangrove]{plotSampledPrev}}), and perform a significance test for whether the observed number of affecteds is greater than would be expected using the \code{summary} method. 

You can see a detailed example of how these methods are applied by calling \code{vignette("Mangrove")}
}
\value{
None of the methods return anything.
}
\seealso{
\code{\link[Mangrove]{MangroveTree}} to generate \code{MangroveSample} objects. \code{\link[Mangrove]{plotSampledPrev}} for more on plotting expected distributions.
}
\examples{
data(famped)
data(exampleORs)
tree <- initialiseTree()
tree$addPed(famped,exampleORs)
sam <- tree$getPrevs(exampleORs,0.02)

summary(sam)
plot(sam)

}
\keyword{classes}
\keyword{methods}
