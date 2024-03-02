\name{summary.sltime}
\alias{summary.sltime}

\title{
Summaries of a Super Learner
}
\description{
Return goodness-of-fit indicators of a Super Learner obtained by the function \code{survivalSL}.
}

\usage{
\method{summary}{sltime}(object,  method="sl", newdata=NULL,
ROC.precision=seq(.01,.99,.01), digits=7, ...)
}

\arguments{
  \item{object}{An object returned by the function \code{survivalSL}.}
  \item{method}{A character string with the name of the algorithm included in the SL for which the calibration plot is performed. The default is "sl" for the Super Learner.}
  \item{newdata}{An optional data frame containing the new sample for validation with covariate values, follow-up times, and event status. The default value is \code{NULL}, the calibration plot is performed from the same subjects of the training sample.}
   \item{ROC.precision}{An optional argument with the percentiles (between 0 and 1) of the prognostic variable used for computing each point of the time dependent ROC curve. 0 (min) and 1 (max) are not allowed. By default, the precision is \code{seq(.01,.99,.01)}.}
   \item{digits}{An optional integer for the number of digits to print when printing numeric values.}
     \item{...}{Additional arguments affecting the summary which are passed from \code{libsl} by default. It concerns the argument \code{times}, \code{failures}, and \code{pro.time}.}
}

\value{No return value for this S3 method.}

\details{
The following metrics are returned: "ci" for the concordance index at the prognostic time \code{pro.time}, "bs" for the Brier score at the prognostic time \code{pro.time}, "ibs" for the integrated Brier score up to the last observed time of event, "ibll" for the integrated binomial log-likelihood up to the last observed time of event, "bll" for the binomial Log-likelihood, "ribs" for the restricted Integrated Brier score up to the prognostic time \code{pro.time}, "ribll" for the restricted integrated binomial log-likelihood up to the last observed time of event, "bll" for the binomial log-likelihood, and "auc" for the area under the time-dependent ROC curve up to the prognostic time \code{pro.time}.
}

\seealso{
  \code{\link{survivalSL}}.
}

\examples{
data(dataDIVAT2)

dataDIVAT2$train <- 1*rbinom(n=dim(dataDIVAT2)[1], size = 1, prob=1/2)

# The training of the super learner with 2 algorithms from the
   # first 100 patients of the training sample
sl1 <- survivalSL(method=c("LIB_AFTgamma", "LIB_PHgompertz"),  metric="auc",
  data=dataDIVAT2[dataDIVAT2$train==1,][1:100,],  times="times", failures="failures",
  pro.time = 12,  cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"),
  cv=3)

# The prognostic capacities from the same training sample
summary(sl1)
}

\keyword{Summarizing}
