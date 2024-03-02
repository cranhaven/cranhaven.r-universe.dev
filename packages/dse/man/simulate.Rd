\name{simulate}
\alias{simulate}
\alias{simulate.ARMA}
\alias{simulate.SS}
\alias{simulate.TSestModel}

\title{Simulate a TSmodel}
\description{Simulate a model to produce artificial data.}
\usage{
    simulate(model, ...)
    \method{simulate}{ARMA}(model, y0=NULL, input=NULL, input0=NULL,
        start=NULL, freq=NULL, sampleT=100, noise=NULL, sd=1, Cov=NULL,
        rng=NULL, noise.model=NULL, compiled=.DSEflags()$COMPILED, ...)
    \method{simulate}{SS}(model, input=NULL, 
        start=NULL, freq=NULL,sampleT=100, noise=NULL, sd=1, Cov=NULL,
	rng=NULL, compiled=.DSEflags()$COMPILED, ...)
    \method{simulate}{TSestModel}(model, input=inputData(model),
			sd=NULL, Cov=NULL, ...)
}
\arguments{
      \item{model}{An object of class TSmodel or TSestModel.}
    \item{input}{Data for the exogenous variable if specified in the model.}
    \item{sampleT}{The length of the sample to simulate.}
    \item{start}{start date for resulting data.}
    \item{freq}{freq for resulting data.}
    \item{y0, input0}{
Lagged values prior to t=1 for y and u, in reverse order so y0[1,] and
input0[1,]correspond to t=0. These arguments  are not implemented for state 
space models. If not specified initial values are set to zero. }
    \item{noise}{
Noise can be supplied. Otherwise it will be generated.
If supplied it should be a list as described below in details.}
    \item{Cov}{
The covariance of the noise process. If this is specified then sd is ignored.
A vector or scalar is treated as a diagonal matrix. For an object of class
TSestModel, if neither Cov nor sd are specified, then Cov is set to
the estimated covariance (\code{model$estimates$cov}).}
    \item{sd}{The standard deviation of the noise. This can be a vector.}
    \item{noise.model}{
A TSmodel to be used for generating noise (not yet supported by SS methods).}
    \item{rng}{The random number generator information needed to
regenerate a simulation.}
    \item{compiled}{
    Specifies the compiled version of the code should be used (instead of
    the S code version).}
    \item{...}{arguments passed to other methods.}
}

\value{
The value returned is an object of class TSdata which can be supplied as an
argument to estimation routines. (See TSdata). In addition to the usual
elements (see the description of a TSdata object)
there are some additional elements:
   model- the generating model,
   rng - the initial RNG and seed,
   version - the version of S used (random number generators may vary)
   Cov as specified
   sd    as specified
   noise - the noise details as provided in the argument or as generated.
   state - the state variable for state space models.
}
\details{
A state space or ARMA model (see TSmodel, \link{ARMA}, and \link{SS} for more details)
is simulated with pseudo
random noise (The default noise is a normally distributed processes. 
An object of class TSdata is
returned. This can be used as input to estimation algorithms. If start and 
freq are specified, or if input or \code{noise$w} (in that order)
have time series properties, these are given to the output. 

If \code{noise} is not supplied then random values will be generated
using other supplied information or defaults. The rng will be set first if 
it is specified.

The default noise generation will be N(0,I) 
If \code{Q} is not square in a non innovations state space model (i.e. the system
noise has a dimension less than the state dimension), then it is
padded with zeros, so generated noise of higher dimension has no
effect.  If sd is supplied, then w as describe below will be N(0,sqr(sd)).
sd can be a vector of \code{p} elements corresponding to each of the \code{p}
outputs.

If noise is supplied it should
be a list of the necessary noise processes. For non-innovation form state
space models the list must have elements \code{w}, \code{e}, and \code{w0}. 
(\code{w0} is \code{w} for \code{t=0} in
state space model and prior lags in ARMA models.) For innovation form state
space models and ARMA models with MA components the list should have elements
\code{w} and \code{w0}, but if \code{w0} is not specified it is set to zero. 
For ARMA models with no  MA components (i.e. VAR models) the list needs 
only \code{w}.
In this case, and in the innovations form state space model with \code{w0=0}, 
a matrix may be supplied in place of a list. \code{w} should be 
a \code{sampleT} by \code{p} matrix giving the noise for \code{t=1} 
to \code{sampleT}. 
If noise is specified \code{sampleT} will be set to the number of periods 
in \code{w}.
 

If \code{noise$w0} is a matrix (rather than a vector) for a state space model 
simulation (as it is for ARMA simulations) then it is set to a
vector of zeros. This provides compatability with VAR models (ARMA
models with no lags in B).

Input must be specified for ARMA models with \code{model$C} not NULL and
state space models with \code{model$G} not NULL..

In general ARMA and SS simulations will
not produce exactly the same results because it is impossible to
determine necessary transformation of initial conditions and \code{w0}.
}

\seealso{
  \code{\link{makeTSnoise}},
  \code{\link{TSmodel}},
  \code{\link{TSdata}},
  \code{\link{ARMA}},
  \code{\link{SS}}
  }
\examples{
    mod1 <- ARMA(A=array(c(1,-.25,-.05), c(3,1,1)), B=array(1,c(1,1,1)))
    AR   <- array(c(1, .5, .3, 0, .2, .1, 0, .2, .05, 1, .5, .3) ,c(3,2,2))
    VAR  <- ARMA(A=AR, B=diag(1,2))
    print(VAR)
    simData <- simulate(VAR)

    C    <- array(c(0.5,0,0,0.2),c(1,2,2))
    VARX <- ARMA(A=AR, B=diag(1,2), C=C) 
    simData <- simulate(VARX, sampleT=150, input=matrix(rnorm(300),150,2))

    MA   <- array(c(1, .2, 0, .1, 0, 0, 1, .3), c(2,2,2)) 
    ARMA  <- ARMA(A=AR, B=MA, C=NULL) 
    simData <- simulate(ARMA, sampleT=200)

    ARMAX <- ARMA(A=AR, B=MA, C=C) 
    simData <- simulate(ARMAX, sampleT=150, input=matrix(rnorm(300),150,2))

    data("eg1.DSE.data.diff", package="dse")
    model <- estVARXls(eg1.DSE.data.diff)
    simData <- simulate(model)

    ss <- SS(F=array(c(.5, .3, .2, .4), c(2,2)), 
             H=array(c(1, 0, 0, 1), c(2,2)),
	     K=array(c(.5, .3, .2, .4), c(2,2)))

    print(ss)
    simData <- simulate(ss)

    testEqual(simData, simulate(ss))
    testEqual(simData, simulate(ss, rng=setRNG::getRNG(simData)))

    simData2 <- simulate(ss,
        noise=list(w=matrix(runif(300), 150,2), w0=runif(2)))

    simData3 <- simulate(ss, noise=matrix(runif(400), 200,2))
    }
\concept{DSE}
\keyword{ts}

