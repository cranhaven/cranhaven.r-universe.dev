\name{tfplot.rootsEstEval}
\alias{tfplot.rootsEstEval}
\alias{plot.rootsEstEval}

\title{Specific tfplot methods for rootsEstEval (EstEval) objects}
\description{See the generic function description.}
\usage{
    \method{tfplot}{rootsEstEval}(x, ...)
    \method{plot}{rootsEstEval}(x, complex.plane=TRUE, cumulate=TRUE, norm=FALSE,
       bounds=TRUE, transform=NULL, invert=FALSE, Sort=TRUE, ...)
}
\arguments{
    \item{x}{an object for which a tfplot is to be produced.}
    \item{complex.plane}{logical indicating if the plot should be on the complex
       plane.}
    \item{cumulate}{logical indicating if the cumulative average of roots should be
    plotted}
    \item{invert}{logical indicating if the inverse of roots should be
    plotted}
    \item{Sort}{logical indicating if the roots should be sorted.}
    \item{...}{arguments passed to other methods.}
    \item{norm}{logical indicating if the euclidean norm of roots should be
       plotted (square root of the sum of squared roots).}
    \item{bounds}{logical indicating if estimated one standard error bounds
       should be plotted around the lines for the true roots.}
    \item{transform}{an optional string indicating the name of a function which
       should be applied to the roots before plotting.}
}

\details{
If complex.plane is TRUE then all results are plotted on a complex plane and 
   the arguements cumulate and Sort do not apply. If complex.plane is FALSE 
   then a sequential plot of the real and imaginary parts is produced.
 If cumulate is true the cumulative average is plotted.
 If mod is true the modulus is used, otherwise real and imaginary are separated.
 if invert is true the reciprical is used (before cumulating).
 if Sort is true then sort is applied (before cumulate but after mod) by the Re part of the root.
   Some grouping is usually necessary since roots are not in an obvious order
   but sorting by the real part of the roots could be improved upon.
}
\seealso{
    \code{\link[tfplot]{tfplot}}
    \code{\link{EstEval}}
}

\concept{DSE}
\keyword{ts}

