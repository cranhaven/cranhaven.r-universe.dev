\name{tuneRSF}
\alias{tuneRSF}

\title{
Tune a Survival Random Forest
}

\description{
This function finds the optimal nodesize, mtry, and ntree parameters for a survival random forest tree.
}

\usage{
tuneRSF(times, failures, group=NULL, cov.quanti=NULL,
cov.quali=NULL, data, nodesize, mtry, ntree)
}

\arguments{
  \item{times}{The name of the variable related the numeric vector with the follow-up times.}
  \item{failures}{The name of the variable related the numeric vector with the event indicators (0=right censored, 1=event).}
  \item{group}{The name of the variable related to the exposure/treatment. This variable shall have only two modalities encoded 0 for the untreated/unexposed patients and 1 for the treated/exposed ones. The default value is \code{NULL}: no specific exposure/treatment is considered. When a specific exposure/treatment is considered, it will be forced in the algorithm or related interactions will be tested when possible.}
  \item{cov.quanti}{The name(s) of the variable(s) related to the possible quantitative covariates. These variables must be numeric.}
  \item{cov.quali}{The name(s) of the variable(s) related to the possible qualitative covariates. These variables must be numeric with two levels: 0 and 1. A complete disjunctive form must be used for covariates with more levels.}
  \item{data}{A data frame for training the model in which to look for the variables related to the status of the follow-up time (\code{times}), the event (\code{failures}), the optional treatment/exposure (\code{group}) and the covariables included in the previous model (\code{cov.quanti} and \code{cov.quali}).}
    \item{nodesize}{The values of the node size optimized over.}
  \item{mtry}{The numbers of variables randomly sampled as candidates at each split optimized over.}
  \item{ntree}{The numbers of trees optimized over.}
 }

\details{
The function runs the \code{tune.rfsrc} function of the \code{randomForestSRC} package.
}

\value{
\item{optimal}{The value of lambda that gives the minimum mean cross-validated error.}
\item{results}{The data frame with the mean cross-validated errors for each lambda values.}
}

\references{
Ishwaran H. and Kogalur U.B. (2007). Random survival forests for R, Rnews, 7(2):25-31.
}

\examples{
data(dataDIVAT2)

tune.model <- tuneRSF(times="times", failures="failures", data=dataDIVAT2,
  cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"),
  nodesize=c(100, 250, 500), mtry=1, ntree=100)

tune.model$optimal # the estimated nodesize value

# The estimation of the training modelwith the corresponding lambda value
model <- LIB_RSF(times="times", failures="failures", data=dataDIVAT2,
  cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"),
  nodesize=tune.model$optimal$nodesize, mtry=1, ntree=100)

# The resulted predicted survival of the first subject of the training sample
plot(y=model$predictions[1,], x=model$times, xlab="Time (years)",
ylab="Predicted survival", col=1, type="l", lty=1, lwd=2, ylim=c(0,1))
}

\keyword{Tunning}
