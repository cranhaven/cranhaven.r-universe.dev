\name{EstEval}
\alias{EstEval}
\alias{is.EstEval}

\title{Evaluate an estimation method}
\description{Evaluate an estimation method.}
\usage{
    EstEval(model, replications=100, rng=NULL, quiet=FALSE, 
                       simulation.args=NULL,
                       estimation=NULL, estimation.args=NULL, 
                       criterion ="coef", criterion.args =NULL) 
%%		       Spawn=if (exists(".SPAWN")) .SPAWN else FALSE)
    is.EstEval(obj)
}
\arguments{
    \item{model}{A TSmodel.} 
    \item{replications}{The number of simulations.}
    \item{rng}{The RNG and starting seed.}
    \item{quiet}{If TRUE then no information is printed during estimation.}
    \item{simulation.args}{A list of any arguments to pass to simulate.}
    \item{estimation}{A character string indicating the estimation routine to use.}
    \item{estimation.args}{A list of any arguments to pass to the estimation routine.}
    \item{criterion}{
    A function to apply to the results of estimation to extract 
    the information which is to be retained.}
    \item{criterion.args}{
    A list of any arguments to be passed to the criterion function.}
%%    \item{Spawn}{a logical indication if Splus For loops should be used.}
    \item{obj}{an object.}
}
\value{
A list with element \code{result} of length replications, each element containing the
results of criterion(estimation(simulate(model))). Other elements of
the list contain information from the supplied arguments.
}
\details{
estimation.args and criterion.args should be NULL if no args are needed.
If model is an object of class 'EstEval' or 'simulation'
then the model and the seed!!! are extracted so the evaluation will be
based on the same generated sample.
criterion can be \code{ 'coef', 'roots', 'TSmodel', or 'TSestModel'}.
With the default (\code{coef}) or with \code{TSmodel} the other criteria 
can be reconstructed
(when the estimation method finds a known form for the model - which
is not always the case, for example with \code{estBlackBox} methods).
If \code{criterion = 'roots'} then 
\code{criterion.args= list(verbose=FALSE)} is advised.
}
\seealso{
\code{\link[dse]{simulate}}
\code{\link{MonteCarloSimulations}}
\code{\link{distribution}}
\code{\link[dse]{forecastCovWRTtrue}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <- estVARXls(TSdata(output=outputData(eg1.DSE.data.diff)))
    z <-  EstEval(model, 
       estimation="estVARXls", estimation.args=list(max.lag=2))
    tfplot(z)
    zz <-  EstEval(model, 
       estimation="estVARXls", estimation.args=list(max.lag=2),
       simulation.args=list(sampleT=50, sd=1.5))
    is.EstEval(z)
}

\concept{DSE}
\keyword{ts}

