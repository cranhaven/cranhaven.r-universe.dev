\name{plot.mixreg}
\alias{plot.mixreg}
\title{
   Plot a fitted mixture of regressions.
}
\description{
   Plots the fitted regression lines for a mixture of regression
   models as fitted by \code{mixreg()} (or possibly \code{visualFit()}).
}
\usage{
   \method{plot}{mixreg}(x, y, cMeth=c("none","distance","prob"),
           legPos = "topleft", ...)
}
\arguments{
  \item{x}{
  An object of class \code{"mixreg"} as returned by \code{\link{mixreg}()}
  or possibly by \code{\link{visualFit}()}.
}
  \item{y}{
  Not used.
}
  \item{cMeth}{
  Text string specifying the \dQuote{classification method}.
  If \code{cMeth} is \code{"none"} no classification is done. If
  \code{cMeth} is \code{"distance"} then points are classified
  as \dQuote{belonging} to a component of the mixture according
  to which of the regression lines they are nearest to, in terms
  of Euclidean distance.  If \code{cMeth} is \code{"prob"} then
  points are classified as \dQuote{belonging} to a component
  of the mixture according to which of the probabilities
  \eqn{\gamma_{ij}}{gamma[i,j]} of their being associated with
  that component is largest.  If the points are classified then
  they are plotted in colours corresponding to that classification.
}
  \item{legPos}{
  A list with entries \code{x} and \code{y}, or a text string,
  specifying the placement of the legend.  See \code{\link{legend}()}
  for details.  The plotting of a legend may be suppressed
  by setting \code{legPos=NULL}.
}
  \item{\dots}{
  Optional extra arguments for \code{plot()}, \code{points()}
  and \code{lines()}.  These may include \code{xlim}, \code{ylim},
  \code{xlab}, \code{ylab}, \code{lty}, \code{col}, \code{pch}, and
  \code{main}.  If \code{col} is supplied, then when \code{cMeth}
  is \code{"none"} its first entry determines the colour in which
  the points are plotted and the remaining entries of \code{col}
  determine the colours in which the fitted lines (corresponding to
  the different components) are plotted.  If the points actually get
  classified then the entries of \code{col} determine the colours
  for both points and lines.  Note that \code{lty} is replicated to
  have length \code{K} where \code{K} is the number of components
  in the model.  If \code{cMeth} is \code{"none"} then \code{col}
  is replicated to have length \code{K+1}, otherwise it is replicated
  to have length \code{K}.
}
}
\value{
  If \code{cMeth} is \code{"none"} then no value is returned.
  Otherwise the value is the \code{data} component of the
  \code{x} argument of this function, augmented by an extra
  column \code{groups}.  This column is a factor that specifies
  the component to which each point has been assigned.  This data
  frame also has an attribute \code{"cMeth"}, the value of the
  \code{cMeth} argument.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
\code{\link{plot.cband}()}, \code{\link{plot.mixresid}()},
\code{\link{qqMix}()},
}
\examples{
   thStrt <- list(
                   list(beta=c(26.07,48808),sigsq=1.1573,lambda=0.33333333),
                   list(beta=c(23.48,32387),sigsq=1.8730,lambda=0.33333333),
                   list(beta=c(-0.0597,20760),sigsq=0.2478,lambda=0.33333333)
                 )
   kfit  <- mixreg(y ~ x,ncomp=3,data=kilnAoneOut,thetaStart=thStrt)
   plot(kfit,pch=8,col=c("red","green","blue","black"))
   plot(kfit,pch=8,col=c("green","blue","black"),cMeth="d")
   plot(kfit,pch=8,col=c("green","blue","black"),cMeth="p")
   afit <- mixreg(plntsInf ~ aphRel,data=aphids,ncomp=2)
   plot(afit,cMeth="p",col=c("blue","red"),pch=20)
   # Separates the points into two groups incredibly clearly!
   npfit <- mixreg(plntsInf ~ 1, data=aphids,ncomp=2)
   plot(npfit,cMeth="p",col=c("blue","red"),pch=20)
}
\keyword{ hplot }
