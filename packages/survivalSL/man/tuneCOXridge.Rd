\name{tuneCOXridge}
\alias{tuneCOXridge}

\title{
Tune Ridge Cox Regression
}

\description{
This function finds the optimal lambda parameter for a ridge Cox regression.
}

\usage{
tuneCOXridge(times, failures, group=NULL, cov.quanti=NULL,
cov.quali=NULL, data, cv=10, parallel=FALSE, lambda)
}

\arguments{
  \item{times}{The name of the variable related the numeric vector with the follow-up times.}
  \item{failures}{The name of the variable related the numeric vector with the event indicators (0=right censored, 1=event).}
  \item{group}{The name of the variable related to the exposure/treatment. This variable shall have only two modalities encoded 0 for the untreated/unexposed patients and 1 for the treated/exposed ones. The default value is \code{NULL}: no specific exposure/treatment is considered. When a specific exposure/treatment is considered, it will be forced in the algorithm or related interactions will be tested when possible.}
  \item{cov.quanti}{The name(s) of the variable(s) related to the possible quantitative covariates. These variables must be numeric.}
  \item{cov.quali}{The name(s) of the variable(s) related to the possible qualitative covariates. These variables must be numeric with two levels: 0 and 1. A complete disjunctive form must be used for covariates with more levels.}
  \item{data}{A data frame for training the model in which to look for the variables related to the status of the follow-up time (\code{times}), the event (\code{failures}), the optional treatment/exposure (\code{group}) and the covariables included in the previous model (\code{cov.quanti} and \code{cov.quali}).}
  \item{cv}{The value of the number of folds. The default value is 10.}
  \item{parallel}{If \code{TRUE}, use parallel \code{foreach} to fit each fold. The default is \code{FALSE}.}
  \item{lambda}{The values of the regularization parameter lambda optimized over.}
 }

\details{
The function runs the \code{cv.glmnet} function of the \code{glmnet} package.
}

\value{
\item{optimal}{The value of lambda that gives the minimum mean cross-validated error.}
\item{results}{The data frame with the mean cross-validated errors for each lambda values.}
}

\references{
Simon, N., Friedman, J., Hastie, T. and Tibshirani, R. (2011) Regularization Paths for Cox's Proportional Hazards Model via Coordinate Descent, Journal of Statistical Software, Vol. 39(5), 1-13, https://www.jstatsoft.org/v39/i05/
}

\examples{
data(dataDIVAT2)

tune.model <- tuneCOXridge(times="times", failures="failures", data=dataDIVAT2,
  cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"),
  cv=5, lambda=seq(0, 10, by=.1))

tune.model$optimal$lambda # the estimated lambda value

# The estimation of the training modelwith the corresponding lambda value
model <- LIB_COXridge(times="times", failures="failures", data=dataDIVAT2,
  cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"),
  lambda=tune.model$optimal$lambda)

# The resulted predicted survival of the first subject of the training sample
plot(y=model$predictions[1,], x=model$times, xlab="Time (years)",
ylab="Predicted survival", col=1, type="l", lty=1, lwd=2, ylim=c(0,1))
}

\keyword{Tunning}
