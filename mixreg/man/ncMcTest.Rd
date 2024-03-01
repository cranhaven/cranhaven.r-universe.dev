\name{ncMcTest}
\alias{ncMcTest}
\title{
Perform a Monte Carlo test for the number of components in a mixture
of regressions.
}
\description{
Produces \code{nsim} simulated realizations of the likelihood
ratio statistic, either parametrically or semi-parametrically,
and calculates the corresponding \eqn{p}-value of the test.
}
\usage{
ncMcTest(x, y, data=NULL, ncomp=2, ncincr=1, intercept=TRUE, nsim=99,
         seed=NULL, ts1=NULL, ts2=NULL, semiPar=FALSE,
         conditional=semiPar, verb=FALSE, progRep=TRUE, \dots)
}
\arguments{
\item{x}{
Either a formula specifying the regression model to be fitted
or a predictor or matrix of predictors for the regression model.
If \code{x} is a matrix it should \emph{NOT} include an initial
column of \code{1}s.  If an intercept is desired then the
\code{intercept} argument should be left equal to \code{TRUE}.
}
\item{y}{
The vector of responses for the regression models. Ignored if
\code{x} is a formula.
}
\item{data}{
A list or data frame containing the variables in the regression
model.  Such variables will be looked for first in \code{data}
and then in the global environment.
}
\item{ncomp}{
The null-hypothesized number of components in the mixture.
}
\item{ncincr}{
The increment from the null-hypothesized number of components in the mixture
to the number under the alternative hypothesis; i.e. the number
of components under the alternative hypothesis is \code{ncomp + ncincr}.
}
\item{intercept}{
Logical argument indicating whether the regression models in the mixture
should have intercept terms.  Ignored if \code{x} is a formula.
}
\item{nsim}{
The number of simulated replicates of the log likelihood ratio statistic
to be produced.
}
\item{seed}{
Positive integer scalar.  The seed for random number generation.
If left \code{NULL} it is obtained by sampling from \code{1:1e5}.
}
\item{ts1}{
Starting values for fitting the \code{ncomp} component model.  If ts1 is
null, random starting values are used.  (This is not recommended.)
}
\item{ts2}{
Starting values for fitting the \code{ncomp+nincr} component model.  If ts2 is
null, random starting values are used.  (This is not recommended.)
}
\item{semiPar}{
Logical scalar; should semi-parametric bootstrapping should be used?
}
\item{conditional}{
Logical scalar; should the component-selection probabilities be
determined conditionally upon the observations?
}
\item{verb}{
Logical argument indicating whether the fitting processes should
be verbose (i.e. whether details should be printed out at each
step of the EM algorithm).  If TRUE a huge amount of
screen output is produced.
}
\item{progRep}{
Logical argument indicating whether the index, of the simulated
statistic just constructed, should be printed out, to give an idea
of how the process is progressing.
}
\item{...}{
Further arguments to be passed to \code{mixreg()} to control the
fitting procedure.
}}
\value{
A list with components:
\item{lrs}{the likelihood ratio statistic for the test}
\item{pval}{the (Monte Carlo) \eqn{p}-value of the test}
\item{simStats}{a vector of the values of the likelihood
ratio statistics of the simulated data sets}
\item{aic.ncomp}{a vector of the aic values for the
\code{ncomp} models fitted to the simulated data sets}
\item{aic.ncomp+nincr}{a vector of the aic values for the
\code{ncomp+nincr} models fitted to the simulated data sets.
(Note that in any given instance \code{ncomp} and \code{nincr} are
replaced by the actual numeric values that are used in that instance.)}
\item{df}{the degrees of freedom that would be appropriate if the
test statistic actually had a chi-squared distribution.
Explicitly \code{df} is equal to the number of parameters in
the alternative model minus the number of parameters in the
null model}
\item{screwUps}{a data frame with columns \code{seed},
\code{i} and \code{type}, containing respectively the random number
generator seed, the index and the type of each screw-up.  Note that
if there were in fact no screw-ups then the \code{screwUps} entry
of the returned value will not be present.  Note also that it is possible
for values in the \code{i} column of \code{screwUps} to be repeated.
The entries of the \code{type} column take values in the set
\code{{1,2,3,4,5}}.  These values have the interpretation:
\itemize{
    \item 1: singularity in the likelihood surface for the
             \code{ncomp} model
    \item 2: the algorithm failed to converge when fitting the
    \code{ncomp} component model
    \item 3: singularity in the likelihood surface for the
             \code{ncomp+nincr} model
    \item 4: the algorithm converged for the
    \code{ncomp} component model but failed to converge for the
    \code{ncomp + nincr} model
    \item 5: the likelihood ratio statistic for the \code{ncomp}
    model was \emph{greater} than that for the \code{ncomp+nincr}
    model (which is \emph{theoretically} impossible)

    Note that if a screw-up does occur, the replicate is redone
    completely so that the returned value contains results for a
    full \code{nsim} simulations.
}}

The returned value has an attribute \code{"seed"} which is the
(initial) value of the random number generation seed that was used.
This is either the value of the argument \code{seed}, or, if this
was \code{NULL}, a randomly generated value.
}
\details{
In this context the \dQuote{parametric} procedure is to
simulate data sets by generating data from the fitted
\code{ncomp} model parameters, using Gaussian errors.  In contrast,
under the
semiparametric bootstrapping procedure, the errors are generated by
resampling from the residuals.  Since at each predictor value there
are \code{ncomp} residuals, one for each component of the model,
the errors are selected at random from these \code{ncomp} possibilities.
If the argument \code{conditional} is \code{TRUE} then the selection
probabilities at this step are the conditional probabilities, of
the observation being generated by each component of the model,
given that observation.  If \code{conditional} is \code{FALSE} then
these probabilities are the corresponding entries of \code{lambda}
(see \bold{Value}.  The residuals are sampled independently in
either case.  The procedure is termed \emph{semi}-parametric since
the sampling probabilities depend on the parameters of the model.
Note that it makes no sense to specify \code{conditional=TRUE}
if \code{semiPar} is \code{FALSE}.  Doing so will generate an error.

It is important to be aware that the test conducted by this function
is a \emph{Monte Carlo} test and that the \eqn{p}-value produced is
a Monte Carlo \eqn{p}-value.  It is consequently an \emph{exact}
\eqn{p}-value in a sense which must be carefully understood.
See for example Baddeley et al. 2015 (section 10.6) and Turner and
Jeffs 2017 for explanation of the interpretation of Monte Carlo
\eqn{p}-values and for some general discussion of Monte Carlo tests
and of their advantages.  Such tests effect substantial savings on
computational costs with only marginal diminishment of power.
}
\author{Rolf Turner
  \email{r.turner@auckland.ac.nz}
}
\references{
T. Rolf Turner (2000). Estimating the rate of spread of a
viral infection of potato plants via mixtures of regressions.
\emph{Applied Statistics} \bold{49} Part 3, pp. 371 -- 384.

Adrian Baddeley, Ege Rubak and Rolf Turner (2015). Spatial Point
Patterns: Methodology and Applications with R. London:
Chapman and Hall/CRC Press.

Rolf Turner and Celeste Jeffs (2017).  A note on exact Monte Carlo
hypothesis tests.  \emph{Communications in Statistics:  Simulation
and Computation} \bold{46}, pp. 6545 -- 6558.
}

\seealso{
\code{\link{cband}()}, \code{\link{covMix}()},
\code{\link{mixreg}()}, \code{\link{plot.cband}()},
\code{\link{plot.mixresid}()}, \code{\link{qqMix}()},
\code{\link{residuals.mixreg}()}
}
\examples{
    \dontrun{
       tst12 <- ncMcTest(plntsInf ~ aphRel,ncomp=1,data=aphids,seed=42)
    } # Monte Carlo p-value is 0.01; mixture model is called for.
    TS1 <- list(list(beta=c(3.0,0.1),sigsq=16,lambda=0.5),
                list(beta=c(0.0,0.0),sigsq=16,lambda=0.5))
    TS2 <- list(list(beta=c(3.0,0.1),sigsq=9,lambda=1/3),
                list(beta=c(1.5,0.05),sigsq=9,lambda=1/3),
                list(beta=c(0.0,0.0),sigsq=9,lambda=1/3))
    x <- aphids$aphRel
    y <- aphids$plntsInf
    \dontrun{
      nsim <- 999
    }
    \testonly{
      nsim <- 9
    }
    tst23 <- ncMcTest(x,y,nsim=nsim,ts1=TS1,ts2=TS2)
}
\keyword{htest}
