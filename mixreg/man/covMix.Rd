\name{covMix}
\alias{covMix}
\alias{covMixMC}
\title{
Calculate the covariance matrix of the parameter estimates for a
mixture of regressions.
}
\description{
Produces an estimate of the covariance matrix of the parameter
estimates for a fitted mixture of linear regressions, by inverting
the observed Fisher information matrix.
}
\usage{
covMix(object,useMC=c("ifNec","no"),...)
covMixMC(object,nsim=100,progRep=TRUE,seed=NULL,...)
}
\arguments{
\item{object}{
Object describing the fitted mixture of regressions, as returned
by \code{\link{mixreg}()}.
}
\item{useMC}{
Text string specifying whether to call upon a Monte Carlo procedure
if there are problems with the \dQuote{analytic} procedure (i.e.
if the calculated observed Fisher information is singular) or to
simply give up and throw an error.
}
\item{nsim}{
Positive integer scalar specifying how many simulated samples to
generate for the purpose of calculating the Monte Carlo estimate
of the covariance matrix.
}
\item{progRep}{
    Logical scalar; should nominal \dQuote{progress reports} be
    issued during the simulation?
}
\item{seed}{
    Integer scalar.  The seed for the random number generator used to
    produce random samples from which to calculate the Monte Carlo
    based estimate of the covariance matrix.  If this argument is
    not supplied, then it is randomly sampled from \code{1:1e5}.
}
\item{...}{
Optional arguments \code{semiPar} and \code{conditional} to be
passed on to \code{\link{rmixreg}()} by \code{covMixMC()}.
}
}
\value{
The estimated covariance matrix.  If the Monte Carlo method
was applied then this matrix has an attribute \code{"seed"}.
This attribute will be the value of the \code{seed} argument if
this was supplied, otherwise it is the randomly generated
replacement for this argument.
}
\details{
If different variances are allowed amongst the components (i.e. if
\code{object$eqVar} is \code{FALSE}) then the parameters are taken
in the order beta.1, sigsq.1, lambda.1, \dots, beta.K, sigsq.K for
a K component model --- lambda.K is redundant and hence omitted.
If equal variances are assumed, the parameters are taken in the
order beta.1, lambda.1, \dots, beta.K, sigsq.

In the foregoing beta refers to the linear coefficients, sigsq to
the variance or variances, and lambda to the mixing probabilities.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\references{
T. Rolf Turner (2000). Estimating the rate of spread of a
viral infection of potato plants via mixtures of regressions.
\emph{Applied Statistics} \bold{49} Part 3, pp. 371 -- 384.

T. A. Louis (1982).  Finding the observed information matrix when
using the EM algorithm. \emph{Journal of the Royal Statistical
Society}, series B \bold{44} pp. 226 -- 233.
}
\seealso{
\code{\link{ncMcTest}()}, \code{\link{cband}()}
\code{\link{mixreg}()}, \code{\link{plot.cband}()},
\code{\link{plot.mixresid}()}, \code{\link{qqMix}()},
\code{\link{residuals.mixreg}()}
}
\examples{
# Aphids.
    fita   <- mixreg(plntsInf~aphRel,ncomp=2,seed=42,data=aphids)
    cMafi  <- covMix(fita)
    \dontrun{
        cMaMC  <- covMixMC(fita)
    }
# Kilns.
   thStrt <- list(
                   list(beta=c(26.07,48808),sigsq=1.1573,lambda=0.33333333),
                   list(beta=c(23.48,32387),sigsq=1.8730,lambda=0.33333333),
                   list(beta=c(-0.0597,20760),sigsq=0.2478,lambda=0.33333333)
                 )
    fitk   <- mixreg(y ~ x,ncomp=3,data=kilnAoneOut,thetaStart=thStrt)
    \dontrun{
        cMkfi  <- covMix(fitk)
        cMkMC  <- covMixMC(fitk)
        cMkMCs <- covMixMC(fitk,semiPar=TRUE)
    }
}
\keyword{models}
\keyword{regression}
