\name{plot.libsl}
\alias{plot.libsl}

\title{
Calibration Plot for a Cox-like Model
}
\description{
A calibration plot of an object of the class \code{libsl} (library of survival super learner).
}

\usage{
\method{plot}{libsl}(x, n.groups=5, pro.time=NULL,
newdata=NULL, times=NULL, failures=NULL, ...)
}

\arguments{
  \item{x}{An object returned by a library of survival super learner.}
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

# The estimation of the model from the first 200 lignes
model <- LIB_COXall(times="times", failures="failures", data=dataDIVAT2,
  cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"))

# The calibration plot from the validation sample of 150 patients
plot(model, n.groups=5, pro.time=12, col=3,
     xlab="Predicted 12-year survival", ylab="Observed 12-year survival",
     newdata=dataDIVAT2[151:300,], times="times", failures="failures")
}

\seealso{
\code{\link{plot.default}}
}

\keyword{Plotting}
