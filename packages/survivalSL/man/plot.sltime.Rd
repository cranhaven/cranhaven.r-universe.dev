\name{plot.sltime}
\alias{plot.sltime}

\title{
Calibration Plot for Super Learner
}

\description{
A calibration plot of a Super Learner obtained by the function \code{survivalSL}.
}

\usage{
\method{plot}{sltime}(x, method, n.groups, pro.time, newdata,
times, failures, ...)
}

\arguments{
  \item{x}{An object returned by the function \code{survivalSL}.}
  \item{method}{A character string with the name of the algorithm included in the SL for which the calibration plot is performed. The default is "sl" for the Super Learner.}
  \item{n.groups}{A numeric value with the number of groups by their class probabilities. The default is 5.}
  \item{pro.time}{The prognostic time at which the calibration plot of the survival probabilities.}
    \item{newdata}{An optional data frame containing the new sample for validation with covariate values, follow-up times, and event status. The default value is \code{NULL}, the calibration plot is performed from the same subjects of the training sample.}
  \item{times}{The name of the variable related the numeric vector with the follow-up times in \code{newdata} (optional argument only necessary when newdata is not \code{NULL}).}
  \item{failures}{The name of the variable related the numeric vector with the event indicators (0=right censored, 1=event) in \code{newdata} (optional argument only necessary when newdata is not \code{NULL}).}
  \item{...}{Additional arguments affecting the plot.}

}

\details{
The plot represents the observed survival and the related 95\% confidence intervals, which are respectively estimated by the Kaplan and Meier estimator and the Greenwood formula, against the mean of the predictive values for individuals stratified into groups of the same size according to the percentiles. The identity line is usually included for reference.
}

\value{No return value for this S3 method.}

\examples{
data(dataDIVAT2)

#The outcome model base on a Super Learner from the first 150 individuals of the data base
sl1 <- survivalSL( methods=c("LIB_AFTgamma", "LIB_PHgompertz"),  metric="ci",
  data=dataDIVAT2[1:150,],  times="times", failures="failures", group="ecd",
  cov.quanti=c("age"),  cov.quali=c("hla", "retransplant"), cv=3)

# The calibration plot from the validation sample of 150 patients
plot(sl1, method="sl", n.groups=5, pro.time=12, col=2,
     xlab="Predicted 12-year survival", ylab="Observed 12-year survival",
     newdata=dataDIVAT2[151:300,], times="times", failures="failures")
}

\seealso{
\code{\link{plot.default}}
}

\keyword{Plotting}
