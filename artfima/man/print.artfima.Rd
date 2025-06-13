\name{print.artfima}
\alias{print.artfima}

\title{Print Method for "arfima" Object}

\description{
Displays the fitted model.
The exact log-likelihood, AIC and BIC are shown.
The signal-to-noise ratio (snr) is defined the (sample variance minus the 
estimated innovation variance) divided by the innovation variance. 
Similar to the coefficient of determination in regression, 
it indicates how much of the randomness is captured by the model.
}

\usage{\method{print}{artfima}(x, ...)}

\arguments{
  \item{x}{ object of class "artfima" }
  \item{...}{ optional arguments }
}

\value{A terse summary is displayed}

\references{TBA}

\author{A. I. McLeod, aimcleod@uwo.ca}

\seealso{\code{\link{artfima}}}

\examples{
artfima(rnorm(100))
}

\keyword{ts}