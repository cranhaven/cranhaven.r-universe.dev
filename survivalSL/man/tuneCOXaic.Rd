\name{tuneCOXaic}
\alias{tuneCOXaic}

\title{
Tune a Cox Model with a Forward Selection Based on the AIC
}

\description{
This function finds the model which minimize the AIC of a Cox PH model.
}

\usage{
tuneCOXaic(times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL,
data, model.min=NULL, model.max=NULL)
}

\arguments{
  \item{times}{The name of the variable related the numeric vector with the follow-up times.}
  \item{failures}{The name of the variable related the numeric vector with the event indicators (0=right censored, 1=event).}
  \item{group}{The name of the variable related to the exposure/treatment. This variable shall have only two modalities encoded 0 for the untreated/unexposed patients and 1 for the treated/exposed ones. The default value is \code{NULL}: no specific exposure/treatment is considered. When a specific exposure/treatment is considered, it will be forced in the algorithm or related interactions will be tested when possible.}
  \item{cov.quanti}{The name(s) of the variable(s) related to the possible quantitative covariates. These variables must be numeric.}
  \item{cov.quali}{The name(s) of the variable(s) related to the possible qualitative covariates. These variables must be numeric with two levels: 0 and 1. A complete disjunctive form must be used for covariates with more levels.}
  \item{data}{A data frame for training the model in which to look for the variables related to the status of the follow-up time (\code{times}), the event (\code{failures}), the optional treatment/exposure (\code{group}) and the covariables included in the previous model (\code{cov.quanti} and \code{cov.quali}).}
  \item{model.min}{An optional argument with the minimal set of covariates.}
  \item{model.max}{An optional argument with the maximal set of covariates.}
 }

\details{
The function runs the \code{stepAIC} function of the \code{MASS} package for covariates' selection.
}

\value{
\item{optimal}{The names of covariate to adjuste the fit.}
\item{results}{The result of the stepAIC process.}
}

\references{
Venables, W. N. and Ripley, B. D. (2002) Modern Applied Statistics with S. Fourth edition. Springer.
}

\examples{
data(dataDIVAT2)

tune.model <- tuneCOXaic(times="times", failures="failures", data=dataDIVAT2,
  cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"))

tune.model$optimal$final.model # the covariate in the model with the best AIC

# The estimation of the training model with the corresponding lambda value
model <- LIB_COXaic(times="times", failures="failures", data=dataDIVAT2,
  cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"),
  final.model=tune.model$optimal$final.model)

# The resulted predicted survival of the first subject of the training sample
plot(y=model$predictions[1,], x=model$times, xlab="Time (years)",
ylab="Predicted survival", col=1, type="l", lty=1, lwd=2, ylim=c(0,1))
}

\keyword{Tunning}
