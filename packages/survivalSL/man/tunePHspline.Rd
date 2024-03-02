\name{tunePHspline}
\alias{tunePHspline}

\title{
Tune a Survival Regression using the Royston/Parmar Spline Model
}

\description{
This function finds the optimal number of knots of the spline function.
}

\usage{
tunePHspline(times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL,
data, cv=10, k)
}

\arguments{
  \item{times}{The name of the variable related the numeric vector with the follow-up times.}
  \item{failures}{The name of the variable related the numeric vector with the event indicators (0=right censored, 1=event).}
  \item{group}{The name of the variable related to the exposure/treatment. This variable shall have only two modalities encoded 0 for the untreated/unexposed patients and 1 for the treated/exposed ones. The default value is \code{NULL}: no specific exposure/treatment is considered. When a specific exposure/treatment is considered, it will be forced in the algorithm or related interactions will be tested when possible.}
  \item{cov.quanti}{The name(s) of the variable(s) related to the possible quantitative covariates. These variables must be numeric.}
  \item{cov.quali}{The name(s) of the variable(s) related to the possible qualitative covariates. These variables must be numeric with two levels: 0 and 1. A complete disjunctive form must be used for covariates with more levels.}
  \item{data}{A data frame for training the model in which to look for the variables related to the status of the follow-up time (\code{times}), the event (\code{failures}), the optional treatment/exposure (\code{group}) and the covariables included in the previous model (\code{cov.quanti} and \code{cov.quali}).}
    \item{cv}{The value of the number of folds. The default value is 10.}
  \item{k}{The number of knots optimized over.}
 }

\details{
The function runs the \code{flexsurvspline} function of the \code{flexsurv} package. The metric used in the cross-validation is the C-index.
}

\value{
\item{optimal}{The value of \code{k} that gives the maximum mean cross-validated C-index.}
\item{results}{The data frame with the mean cross-validated C-index according to \code{k}.}
}

\references{
Royston, P. and Parmar, M. (2002). Flexible parametric proportional-hazards and proportional odds models for censored survival data, with application to prognostic modelling and estimation of treatment effects. Statistics in Medicine 21(1):2175-2197. doi: 10.1002/sim.1203
}

\examples{
data(dataDIVAT2)

# The estimation of the hyperparameters

tune.model <- tunePHspline(times="times", failures="failures", data=dataDIVAT2[1:200,],
    cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"),
    cv=3, k=1:2)

# the estimated nodesize value

 tune.model$optimal
 tune.model$results

# The estimation of the training model with the corresponding lambda value

 model <- LIB_PHspline(times="times", failures="failures", data=dataDIVAT2[1:200,],
   cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"),
   k=tune.model$optimal$k)

# The resulted predicted survival of the first subject of the training sample

plot(y=model$predictions[1,], x=model$times, xlab="Time (years)",
 ylab="Predicted survival", col=1, type="l", lty=1, lwd=2, ylim=c(0,1))
}

\keyword{Tunning}
