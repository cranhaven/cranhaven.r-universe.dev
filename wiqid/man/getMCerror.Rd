\name{getMCerror}
\alias{getMCerror}

\title{
MCMC error using the batch method (deprecated)
}
\description{
This is now a wrapper for \code{\link[mcmcOutput]{getMCE}}
}

\usage{
getMCerror(object, n.chains, SDpc=FALSE)
}

\arguments{
  \item{object}{
an object of any class with MCMC output that can be coerced to class mcmcOutput.
}
  \item{n.chains}{
ignored
}
  \item{SDpc}{
if TRUE, the value of the MC error as a percentage of the posterior SD will be returned.
}
}

\value{
If SDpc is FALSE (the default), a named vector with the estimates of MC error. If TRUE, the MC error as a percentage of the standard deviation of the posterior chain. A value <5\% of SD is adequate for most purposes, but <1.5\% is needed to properly estimate tail probabilities (Lunn et al 2013, p78-79).
}

\references{
Lunn, D., Jackson, C., Best, N., Thomas, A., & Spiegelhalter, D. (2013) \emph{The BUGS book: a practical introduction to Bayesian analysis}, Chapman and Hall.

Roberts, G.O. (1996). Markov chain concepts related to sampling algorithms. In \emph{Markov Chain Monte Carlo in practice} (eds W.R. Gilks, D.J. Spiegelhalter & S. Richardson). Chapman & Hall, London.
}

\author{
Mike Meredith
}


\examples{
# Get some output to use
data(salamanders)
y <- rowSums(salamanders)
( out <- BoccSS0(y, 5) )

getMCerror(out)
getMCerror(out, SDpc=TRUE)
}
