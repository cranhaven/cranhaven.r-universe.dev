\name{visualFit}
\alias{visualFit}
\title{
  Fit a mixture of regressions model by \dQuote{visual} means.
}
\description{
  Displays a plot of the data and invites the user to \dQuote{click}
  on points judged to lie on the various components.
}
\usage{
visualFit(fmla, data=NULL, ncomp, eqVar=FALSE, chsnPts=NULL,
      keepPlotVisible = FALSE)
}
\arguments{
  \item{fmla}{
  A formula specifying the regression model to be fitted.
}
  \item{data}{
  A list or data frame in which the variables specified by
  \code{fmla} may be searched for.  Variables not found in
  \code{data} are searched for in the global environment.
}
  \item{ncomp}{
  Positive integer scalar.  The number of components in the mixture
  which is to be fitted.
}
  \item{eqVar}{
  Logical scalar; should the error variance be the same for
  all components?  (The alternative is that each component should be
  allowed to have a different error variance.)
}
  \item{chsnPts}{
  A list with \code{ncomp} components each of which is a list
  of length two with components \code{x} and \code{y}.  Each
  of \code{x} and \code{y} is a vector of length two, constituting
  the \eqn{x} and \eqn{y} coordinates respectively of two points
  on a line that presumably underlies the corresponding component.
} 
  \item{keepPlotVisible}{
  Logical scalar.  Should the plot of the data, produced by this
  function, be kept visible after the model has been \dQuote{fitted}?
  (Rather than being dismissed by \code{dev.off()}.)
}
}
\details{
  If the model involves more than one predictor, or if the specified
  predictor is a matrix with more than one column, then an error is
  thrown.  This function is intended for use only with one-variable
  regression.

  If there is an intercept in the model, then for each component
  (numbered 1 to \code{ncomp}) the user is invited to click
  on \emph{two} points judged to lie on a line underlying that
  component.  If there is no intercept, then the user is invited
  to click on a single point for each component, with the origin
  \code{(0,0)} taken (silently) to be the second point needed to
  determine the line.

  The fit that this function returns is calculated by assigning
  a component to each point in the data set, based on which of the
  visually determined lines that point is closest to.

  If \code{eqVar} is \code{TRUE} then the model is constructed using
  a factor, whose entries are these assigned components, as a predictor
  (along with the \dQuote{\code{x}} variable in \code{fmla}) in a
  call to \code{lm()}.  If code \code{eqVar} is \code{FALSE} then a
  model is fitted separately to each component.  (See the code for
  details.) \dQuote{Obviously} the linear coefficient estimates will
  be the same in either case.  Only the error variance estimates
  will differ.

  If \code{eq.var} is \code{TRUE} then the number of parameters in
  the model, as used in the calculation of \code{aic}, is \code{M =
  2*K + (K-1) + 1 = 3*K} when there is an intercept term and \code{M
  = K + (K-1) + 1 = 2*K} when there is no intercept term.

  If \code{eq.var} is \code{FALSE} then the number of parameters
  is \code{M = 2*K + (K-1) + K = 4*K - 1} if there is an intercept,
  and \code{M = K + (K-1) + K = 3*K - 1} if there is no intercept.

  The argument \code{chsnPts} allows one to use this function
  in a non-interactive session by creating and saving, \emph{a
  priori}, an object to be supplied as the value of \code{chsnPts}.
  If \code{chsnPts} is supplied then the method employed isn't
  really \dQuote{visual}, but presumably the object supplied
  would have been created in a visual manner.  Be that as it may,
  this function is mainly intended to be used visually, that is
  \emph{without} supplying \code{chsnPts}.

  If \code{chsnPts} is not supplied then (\dQuote{obviously}!!!) this
  function can be used \emph{only} in an interactive session.
}
\value{
  An object of class \code{"mixreg"}.  (See \code{\link{mixreg}()}.)
  Components \code{nsteps} and \code{converged} are set to \code{NA}.
  Component \code{data} has an extra column \code{groups} appended
  to it.  This column is a factor specifying the components assigned
  to the points on the basis of distances from the lines determined
  by the chosen points.

  The value also has an attribute \code{"chsnPts"}.  This is the
  list of points judged by the user to lie on the component lines
  (or the value of the \code{chsnPts} argument if this was supplied).
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}

\seealso{
   \code{\link{mixreg}()}
}
\examples{
\dontrun{
    vfita <- visualFit(plntsInf ~ aphRel,data=aphids,ncomp=2)
    plot(vfita)
    vfitk1 <- visualFit(y ~ x, data=kilnAoneOut, ncomp=3)
    cp     <- attr(vfitk1,"chsnPts")
    vfitk2 <- visualFit(y ~ x, data=kilnAoneOut, eqVar=TRUE, chsnPts=cp)
    vfitk1$parmat
    vfitk2$parmat # Same as above except for the "sigsq" column.
}
}
\keyword{models}
\keyword{regression}
\keyword{iplot}
