\name{plot.cband}
\alias{plot.cband}
\title{
Plot confidence bands for a mixture of regressions.
}
\description{
Plots the fitted lines and confidence and prediction bands as
calculated by cband, for a mixture of regressions on one variable.
}
\usage{
\method{plot}{cband}(x, cbands=TRUE, pbands=TRUE,
                     type=c("both","upper","lower"),
                     legPos="topleft", sepFac=0.8, \dots)
}
\arguments{
\item{x}{
An object of class \code{"cband"} specifying the fitted lines and
confidence and prediction bands to be plotted, as produced by cband.
}
\item{cbands}{
Logical scalar; should the confidence bands be plotted?
}
\item{pbands}{
Logical scalar; should the prediction bands be plotted?
}
\item{type}{
Character string specifying whether the bands plotted should be
two-sided (\code{"both"}) or if only the upper (\code{"upper"}
or lower (\code{"lower"}) envelopes should be plotted.  May be
abbreviated (e.g. to \code{"b"}, \code{"u"} or \code{"l"}).
}
\item{legPos}{
  A list with entries \code{x} and \code{y}, or a text string,
  specifying the placement of the legend.  See \code{\link{legend}()}
  for details.  The plotting of a legend may be suppressed
  \code{legPos=NULL}.  The legend consists of two parts; the upper
  part, which specifies line types, will have an entry for confidence
  bands only if \code{cbands} is \code{TRUE} and likewise it will
  have an entry type for prediction bands only if \code{pbands} is
  \code{TRUE}.  The lower part specifies the colours of the fitted
  lines corresponding to the different components.  It will be
  present only if there is more than one colour for the components.

}
\item{sepFac}{
  \dQuote{Separation factor}.
  A numeric scalar determining the amount of separation between
  the two parts of the legend (see above).  Has an effect only
  if both parts of the legend are present.  Making \code{sepFac}
  larger increases the amount of separation between the two parts;
  making it smaller decreases the amount.  Making it much smaller
  than the default value will cause over-printing.
}
\item{\dots}{
  Optional extra arguments for \code{plot()}, \code{points()}
  and \code{lines()}.  These may include \code{xlim}, \code{ylim},
  \code{xlab}, \code{ylab}, \code{lty}, \code{col}, \code{pch},
  and \code{main}.  If \code{col} is supplied, its first entry
  determines the colour in which the points are plotted, and the
  remaining entries of \code{col} determine the colours in which
  the fitted lines and envelopes (corresponding to the different
  components) are plotted.  Note that \code{col} is replicated to
  have length \code{K+1} where \code{K} is the number of components
  in the model.  The argument \code{lty} determines the line types
  for the fitted lines, the confidence envelopes and the prediction
  envelopes.  These line types remain constant across components.
  Note that if \code{main} is not supplied then a \dQuote{sensible}
  default main title is created.  If no main title is desired,
  specify \code{main=""}.
}
}
\value{
None.  This function is called for its side effect of producing a plot.
}
\details{
This function is a method for plot.  Note that a simple plot
of the fit may be produced by calling \code{plot(object)} where
\code{object} is an object of class \code{"mixreg"} and the \code{x}
argument of this function was produced by applying \code{cband()}
to this object.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\seealso{
\code{\link{cband}()}, \code{\link{plot.cint}()},
\code{\link{plot.mixreg}()}, \code{\link{plot.mixresid}()},
\code{\link{qqMix}()}, \code{\link{residuals.mixreg}()}
}
\examples{
    thStrt <- list(list(beta=c(3.0,0.1),sigsq=16,lambda=0.5),
                   list(beta=c(0.0,0.0),sigsq=16,lambda=0.5))
    fit    <- mixreg(plntsInf~aphRel,ncomp=2,thetaStart=thStrt,
                     covMat=TRUE,data=aphids)
    cbds   <- cband(fit)
    plot(cbds)
    plot(cbds,pbands=FALSE)
    plot(cbds,pbands=FALSE,type="u")
}
\keyword{models}
\keyword{regression}
