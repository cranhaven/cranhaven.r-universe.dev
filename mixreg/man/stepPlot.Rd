\name{stepPlot}
\alias{stepPlot}
\title{
    Plot the steps of the fit of a mixture of regressions model.
}
\description{
    Fit a mixture of regressions model, one EM step at a time,
    plotting the result after each step.
}
\usage{
stepPlot(fmla, ncomp = NULL, eqVar = FALSE, thetaStart = NULL,
         nsteps = 100, eps = 1e-06, digits = 7, maxTry = 5,
         seed = NULL, data)
}
\arguments{
  \item{fmla}{
  The formula specifying the regression in the mixture model that
  is to be fitted.
}
  \item{ncomp}{
  Integer scalar; the number of components in the model.  Defaults
  to 2.
}
  \item{eqVar}{
  See \code{\link{mixreg}()}.
}
  \item{thetaStart}{
  See \code{\link{mixreg}()}.
}
  \item{nsteps}{
  Integer scalar; the maximum number of EM steps to be undertaken.
}
  \item{eps}{
  See \code{\link{mixreg}()}.
}
  \item{digits}{
  The number of digits to which the details about the EM steps are printed out.
}
  \item{maxTry}{
  See \code{\link{mixreg}()}.
}
  \item{seed}{
  See \code{\link{mixreg}()}.
}
  \item{data}{
  A data frame containing the variables to which the model is
  to be fitted.
}
}
\details{
  Can only be used in an interactive session.  \emph{May} give
  some insight into convergence problems or into the reasons
  for unexpected results in the fit.
}
\value{
  An object of class \code{"mixreg"}.  See \code{\link{mixreg}()}.
  The model being fitted may not have converged.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\references{
   See \code{\link{mixreg}()}.
}

\seealso{
    \code{\link{mixreg}()}.
}
\examples{
\dontrun{ # Interactive session is required.
    vfit <- visualFit(y~x,data=kilnAfull,ncomp=3)
    stepPlot(y ~ x,ncomp=3,data=kilnAfull,thetaStart=vfit$theta)
}
# The result is clearly slightly wrong in respect of the second ("middle")
# component, which has a slope that is too low.  The outlier in
# kilnAfull may be "pulling up" the left hand end of the fitted line.
}

\keyword{models}
\keyword{regression}
\keyword{iplot}
