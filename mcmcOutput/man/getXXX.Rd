\name{Get diagnostics}
\alias{getMCE}
\alias{getNeff}
\alias{getRhat}
\encoding{UTF-8}
\title{
Get diagnostic statistics
}
\description{
These functions calculated diagnostic statistics for objects of class \code{mcmcOutput}. Optionally, only values that are worse than a predefined threshold will be returned, and values can be sorted so that the worst are at the start of the output vector.
}
\usage{
getMCE(x, pc=TRUE, bad=5, sort=TRUE)
getNeff(x, bad=10000, sort=TRUE)
getRhat(x, bad=1.1, sort=TRUE)

}
\arguments{
  \item{x}{
an object of any class with MCMC output that can be coerced to class \code{mcmcOutput}.
}
  \item{pc}{
if TRUE, the value of the MC error as a percentage of the posterior SD will be returned.
}
  \item{bad}{
threshold for "bad" values: only values above this (for \code{getMCE} or \code{getRhat}) or below this (for \code{getNeff}) will be returned. If \code{bad = NA}, all values will be returned, including NAs.
}
  \item{sort}{
if TRUE, the values will be sorted, with the worst at the top.
}
}
\details{

\code{getRhat} returns the Brooks-Gelman-Rubin (BGR) convergence diagnostic (Brooks & Gelman 1998), a non-parametric 'interval' estimator of the 'potential scale reduction factor' for MCMC output. Similar to the function \code{coda::gelman.diag}, but faster when thousands of parameters are involved and will not cause R to crash.

\code{getMCE} returns the Monte Carlo standard error calculated using the batch method of Lunn et al (2013, p77); see also Roberts (1996).

\code{getNeff} returns the effective number of draws taking account of autocorrelation within each chain. It is a wrapper for \code{coda::effectiveSize}.
}

\value{
A named vector with the values of the diagnostic. Values of NA will be excluded unless \code{bad = NA}. It may have length 0 if no values are bad.
}
\author{
Mike Meredith
}
\references{
Brooks, S.P. & Gelman, A. (1998) General methods for monitoring convergence of iterative simulations. \emph{Journal of Computational and Graphical Statistics}, 7, 434-455.

Lunn, D., Jackson, C., Best, N., Thomas, A., & Spiegelhalter, D. (2013) \emph{The BUGS book: a practical introduction to Bayesian analysis}, Chapman and Hall.

Roberts, G.O. (1996). Markov chain concepts related to sampling algorithms. In \emph{Markov Chain Monte Carlo in practice} (eds W.R. Gilks, D.J. Spiegelhalter & S. Richardson). Chapman & Hall, London.
}

\examples{
data(mcmcListExample)
mco <- mcmcOutput(mcmcListExample)

getMCE(mco, bad=2)
getMCE(mco, bad=0)  # returns all except NAs
getMCE(mco, bad=NA)  # returns all including NAs
getMCE(mco, bad=NA, sort=FALSE)  # returns all, in original order

getNeff(mco, bad=2800)
getNeff(mco, bad=Inf)  # returns all except NAs
getNeff(mco, bad=NA)   # returns all including NAs

getRhat(mco)
getRhat(mco, bad=0)
getRhat(mco, bad=NA, sort=FALSE)

# Extract the values with 'bad' MCE and do plots:
( badNodes <- names(getMCE(mco, bad=2)) )
( badMco <- mco[badNodes] )
plot(badMco)
}


