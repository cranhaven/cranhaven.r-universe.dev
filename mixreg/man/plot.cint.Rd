\name{plot.cint}
\alias{plot.cint}
\title{
Plot confidence intervals for a Gaussian scalar mixture model.
}
\description{
Plots confidence and prediction intervals, corresponding to each
component of a Gaussian scalar mixture model.
}
\usage{
\method{plot}{cint}(x, cints=TRUE, pints=TRUE,rugged=TRUE,
                     type=c("both","upper","lower"),\dots)
}
\arguments{
\item{x}{
An object of class \code{"cint"}, specifying the confidence
and prediction intervals to be plotted, as produced by the
undocumented function \code{cint()}.  Such an object would usually
be produced (indirectly) from a call to \code{\link{cband}()}.
If the class \code{"mixreg"} object, to which \code{cband()}
is applied, specifies a fit to a model involving no predictors,
then confidence and prediction \emph{bands} make no sense, and
the object is handed over to \code{cint()} to produce confidence
and prediction \emph{intervals}.
}
\item{cints}{
Logical scalar; should the confidence intervals be plotted?
}
\item{pints}{
Logical scalar; should the prediction intervals be plotted?
}
\item{rugged}{
Logical scalar.  Should a \dQuote{rug} be added to the plot,
displaying the values of the variable to which the model was
fitted?  See \code{\link{rug}()}.  If \code{rugged} is
\code{TRUE} then a rug is added at the right hand margin of the
plot.
}
\item{type}{
Character string specifying whether the intervals plotted should be
two-sided (\code{"both"}) or if only the upper (\code{"upper"}
or lower (\code{"lower"}) bounds should be plotted.  May be
abbreviated (e.g. to \code{"b"}, \code{"u"} or \code{"l"}).
}
\item{\dots}{
  Optional extra arguments for \code{plot()} and \code{title()} These
  may include \code{xlim}, \code{ylim}, \code{xlab}, \code{ylab},
  \code{col}, \code{pch}, and \code{main}.  Note that \code{col}
  is replicated to have length \code{K} where \code{K} is the number
  of components in the model.  If \code{main} is not supplied then
  a \dQuote{sensible} default main title is created.  If no main
  title is desired, specify \code{main=""}.
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
\code{\link{cband}()}, \code{\link{plot.cband}()},
\code{\link{plot.mixreg}()}, \code{\link{plot.mixresid}()},
\code{\link{qqMix}()}, \code{\link{residuals.mixreg}()}
}
\examples{
    fit  <- mixreg(plntsInf~1,ncomp=2,seed=42,covMat=TRUE,data=aphids)
    cis  <- cband(fit) # *Could* call cint(fit) directly 
    plot(cis)
    plot(cis,pintss=FALSE)
    plot(cis,pintss=FALSE,type="u")
}
\keyword{models}
\concept{mixtures}
