\name{metrics}
\alias{metrics}

\title{
Metrics to Evaluate the Prognostic Capacities
}

\description{
Compute several metrics to evaluate the prognostic capacities with time-to-event data.
}

\usage{
metrics(times, failures, data, prediction.matrix, prediction.times, metric,
pro.time=NULL, ROC.precision=seq(.01, .99, by=.01))
}


\arguments{
  \item{times}{The name of the variable related the numeric vector with the follow-up times.}
  \item{failures}{The name of the variable related the numeric vector with the event indicators (0=right censored, 1=event).}
  \item{data}{A data frame for in which to look for the variables related to the status of the follow-up time (\code{times}) and the event (\code{failures}).}
  \item{prediction.matrix}{A matrix with the predictions of survivals of each subject (lines) for each prognostic times (columns).}
  \item{prediction.times}{A vector of numeric values with the times of the \code{predictions} (same length than the number of columns of \code{prediction.matrix}).}
  \item{metric}{The metric to compute. See details.}
  \item{pro.time}{This optional value of prognostic time represents the maximum delay for which the capacity of the variable is evaluated. The same unit than the one used in the argument times. Not used for the following metrics: "loglik", "ibs", "bll", and "ibll". Default value is the time at which half of the subjects are still at risk.}
   \item{ROC.precision}{An optional argument with the percentiles (between 0 and 1) of the prognostic variable used for computing each point of the time dependent ROC curve. Only used when \code{metric="auc"}. 0 (min) and 1 (max) are not allowed. By default, the precision is \code{seq(.01,.99,.01)}.}
 }

\details{
The following metrics can be used: "bs" for the Brier score at the prognostic time \code{pro.time}, "ci" for the concordance index at the prognostic time \code{pro.time}, "loglik" for the log-likelihood, "ibs" for the integrated Brier score up to the last observed time of event, "ibll" for the integrated binomial log-likelihood up to the last observed time of event, "bll" for the binomial log-likelihood, "ribs" for the restricted integrated Brier score up to the prognostic time \code{pro.time}, "ribll" for the restricted integrated binomial log-likelihood up to the last observed time of event, "bll" for the binomial log-likelihood, "auc" for the area under the time-dependent ROC curve up to the prognostic time \code{pro.time}.
}

\value{
A numeric value with the metric estimation.
}

\examples{

data(dataDIVAT2)

# The estimation of the model
model <- LIB_COXridge(times="times", failures="failures", data=dataDIVAT2,
  cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"), lambda=1)

# The apparent AUC at 10-year post-transplantation
metrics(times="times", failures="failures", data=dataDIVAT2,
  prediction.matrix=model$predictions, prediction.times=model$times,
  metric="auc", pro.time=10)

# The integrated Brier score up to 10 years post-transplanation
metrics(times="times", failures="failures", data=dataDIVAT2,
  prediction.matrix=model$predictions, prediction.times=model$times,
  metric="ribs", pro.time=10)
}

\keyword{Metrics}
