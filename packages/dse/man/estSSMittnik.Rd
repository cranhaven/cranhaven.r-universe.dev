\name{estSSMittnik}
\alias{estSSMittnik}
\title{Estimate a State Space Model}
\description{Estimate a state space model using Mittnik's markov parameter estimation.
}
\usage{
    estSSMittnik(data, max.lag=6, n=NULL, subtract.means=FALSE, normalize=FALSE)
}
\arguments{
    \item{data}{A TSdata object.}
    \item{max.lag}{The number of markov parameters to estimate.}
    \item{n}{The state dimension.}
    \item{subtract.means}{If TRUE subtract the means from the data before estimation.}
    \item{normalize}{If TRUE normalize the data before estimation.}
}
\value{A state space model in an object of class \code{TSestModel}.}
\details{
Estimate a nested-balanced state space model by svd from least squares
estimate of markov parameters a la \cite{Mittnik (1989, p1195)}.
The quality of the estimate seems to be quite sensitive to \code{max.lag}, 
and this is not properly resolved yet.
If \code{n} is not supplied the svd criteria will be printed and \code{n} prompted for.
If \code{subtract.means=T} then the sample mean is subtracted. 
If \code{normalize} is \code{T} the lsfit estimation is done with outputs normalize to cov=I
(There still seems to be something wrong here!!).
The model is then re-transformed to the original scale.

See \code{MittnikReduction} and references cited there. If the state
dimension is not specified then the singular values of the Hankel matrix are
printed and the user is prompted for the state dimension.
}
\references{
    See references for \code{\link{MittnikReduction}}. 
}
\seealso{
    \code{\link{MittnikReduction}}
    \code{\link{estVARXls}}
    \code{\link{bft}}
}
\examples{
    data("egJofF.1dec93.data", package="dse")
    # this prints information about singular values and prompts with
    #Enter the number of singular values to use for balanced model:
    \donttest{model <- estSSMittnik(egJofF.1dec93.data)}
    # the choice is difficult in this example. 
    model <- estSSMittnik(egJofF.1dec93.data, n=3)
}
\concept{DSE}
\keyword{ts}

