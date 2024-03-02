\name{LIB_PHspline}
\alias{LIB_PHspline}

\title{
Library of the Super Learner for an Survival Regression using the Royston/Parmar Spline Model
}

\description{
Fit an PH model with a survival function is modelled as a natural cubic spline function.
}

\usage{
LIB_PHspline(times, failures, group=NULL, cov.quanti=NULL, cov.quali=NULL,
data, k)
}


\arguments{
  \item{times}{The name of the variable related the numeric vector with the follow-up times.}
  \item{failures}{The name of the variable related the numeric vector with the event indicators (0=right censored, 1=event).}
  \item{group}{The name of the variable related to the exposure/treatment. This variable shall have only two modalities encoded 0 for the untreated/unexposed patients and 1 for the treated/exposed ones. The default value is NULL: no specific exposure/treatment is considered. When a specific exposure/treatment is considered, it will be forced in the algorithm or related interactions will be tested when possible.}
  \item{cov.quanti}{The name(s) of the variable(s) related to the possible quantitative covariates. These variables must be numeric.}
  \item{cov.quali}{The name(s) of the variable(s) related to the possible qualitative covariates. These variables must be numeric with two levels: 0 and 1. A complete disjunctive form must be used for covariates with more levels.}
  \item{data}{A data frame for training the model in which to look for the variables related to the status of the follow-up time (\code{times}), the event (\code{failures}), the optional treatment/exposure (\code{group}) and the covariables included in the previous model (\code{cov.quanti} and \code{cov.quali}).}
  \item{k}{Number of knots.}
 }

\details{
The model is obtained by using the \code{scale="hazard"} in the \code{flexsurvreg} package.
}

\value{
\item{model}{The estimated model.}
\item{group}{The name of the variable related to the exposure/treatment.}
\item{cov.quanti}{The name(s) of the variable(s) related to the possible quantitative covariates.}
\item{cov.quali}{The name(s) of the variable(s) related to the possible qualitative covariates.}
\item{data}{The data frame used for learning. The first column is entitled \code{times} and corresponds to the observed follow-up times. The second column is entitled \code{failures} and corresponds to the event indicators. The other columns correspond to the predictors.}
\item{times}{A vector of numeric values with the times of the \code{predictions}.}
\item{hazard}{A vector of numeric values with the values of the cumulative baseline hazard function at the prediction \code{times}.}
\item{predictions}{A matrix with the predictions of survivals of each subject (lines) for each observed time (columns).}
}

\references{
Jackson, C. (2016). flexsurv: A Platform for Parametric Survival Modeling in R. Journal of Statistical Software, 70(8), 1-33. doi:10.18637/jss.v070.i08
}

\examples{
data(dataDIVAT2)

# The estimation of the model from the first 200 lignes with two knots
model <- LIB_PHspline(times="times", failures="failures", data=dataDIVAT2[1:200,],
        cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"), k=2)

# The predicted survival of the first subject of the training sample
  plot(y=model$predictions[1,], x=model$times, xlab="Time (years)",
       ylab="Predicted survival", col=1, type="l", lty=1, lwd=2, ylim=c(0,1))
}

\keyword{Super Learner Library}
