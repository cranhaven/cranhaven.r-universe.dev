\name{forecastCovReductionsWRTtrue}
\alias{forecastCovReductionsWRTtrue}

\title{Forecast covariance for different models}
\description{Calculate the forecast covariance for different models.}
\usage{
   forecastCovReductionsWRTtrue(true.model, rng=NULL,
                       simulation.args=NULL,
                       est.replications=2, pred.replications=2,
                       discard.before=10, horizons=1:12,quiet=FALSE,
                       estimation.methods=NULL,
                       criteria=NULL, compiled=.DSEflags()$COMPILED) 
%%                       Spawn=if (exists(".SPAWN")) .SPAWN else FALSE)
}
\arguments{
    \item{true.model}{An object of class TSmodel or TSestModel.}
    \item{discard.before}{
       An integer indicating the number of points in the
       beginning of forecasts to discard for calculating covariances.}
    \item{est.replications}{an interger indicating the number of times
       simulation and estimation are repeated.}
    \item{pred.replications}{an argument passed to \code{forecastCovWRTtrue}.}
    \item{simulation.args}{
       A list of any arguments which should be passed to simulate
       in order to simulate the true model.}
    \item{horizons}{Horizons for which forecast covariance should be calculated.}
    \item{rng}{If specified then it is used to set RNG.}
%%    \item{Spawn}{If TRUE then Splus For loops are used.}
    \item{quiet}{If TRUE then some messages are not printed.}
    \item{estimation.methods}{a list as used by estimateModels.}
    \item{criteria}{a ...}
    \item{compiled}{a logical indicating if compiled code should be
      used. (Usually true except for debugging.)}
}
\value{
    A list ...
}
\details{
 Calculate the forecasts cov of reduced models estimated from simulations of
  true.model with an estimation method indicated by estimation.methods. 
   (estimation.methods is as in estimation.models BUT ONLY THE FIRST IS USED.)
  discard.before is an integer indicating 1+the number of points in the
  beginning of forecasts to discard for calculating forecast covariances.
  criteria can  be a vector of criteria as in informationTests,
   (eg c("taic", "tbic") in which case the "best" model for each criteria
   is accounted separately. (ie. it is added to the beginning of the list of
  estimated models)
}
\concept{DSE}
\keyword{ts}

