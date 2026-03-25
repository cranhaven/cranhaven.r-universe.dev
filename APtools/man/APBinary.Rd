\name{APBinary}
\alias{APBinary}
\title{Estimating the AP and the AUC for Binary Outcome Data.}
\description{
  This function calculates the estimates of the AP and AUC
  for binary outcomes as well as their confidence intervals
  using the perturbation or the nonparametric bootstrap
  resampling method.
}
\usage{
  APBinary(status, marker, cut.values = NULL,
    method = "none", alpha = 0.95, B = 1000, weight = NULL)
}
\arguments{
  \item{status}{Binary indicator, 1 indicates case / the
  class of prediction interest and 0 otherwise.}

  \item{marker}{Numeric risk score. Data
  can be continuous or ordinal.}

  \item{cut.values}{risk score values to use as a cut-off for
  calculation of positive predictive values (PPV) and true
  positive fractions (TPF). The default value is NULL.}

  \item{method}{Method to obtain confidence intervals. The
  default is method = "none", in which case only point estimates will be given without confidence intervals.
  If method= "perturbation", then perturbation based CI will be calculated.
  If method = "bootstrap", then nonparametric bootstrap based CI will be calculated.}

  \item{alpha}{Confidence level.  The default level is
  0.95.}

  \item{B}{Number of resampling to obtain confidence
  interval. The default value is 1000.}
  \item{weight}{Optional. The default weight is 1, same
  object length as the "status" and "marker" object.
  Users can use their own weights, and the length of
  weight is required to be the same as the length of
  status.}
}
\value{
  an object of class "APBinary" which is a list with
  components:

  \item{ap_summary}{Summary of the AP, including the proportion of
  cases, a point estimate of AP, and their corresponding
  confidence intervals.}

  \item{auc_summary}{Summary of the AUC, including a point
  estimate of AUC with a confidence interval.}

  \item{PPV}{Available object, positive predictive values at the
  unique risk score in the data.}

  \item{TPF}{Available object, true positive fractions at the
  unique risk score in the data.}
}
\references{
  Yuan, Y., Su, W., and Zhu, M. (2015). Threshold-free
  measures for assessing the performance of medical
  screening tests. Frontiers in Public Health, 3.57.

  Bingying Li (2015) Threshold-free Measure for Assessing
  the Performance of Risk Prediction with Censored Data,
  MSc. thesis, Simon Fraser University, Canada
}
\examples{
status=c(rep(1,10),rep(0,1),rep(1,18),rep(0,11),rep(1,25),
	rep(0,44),rep(1,85),rep(0,176))
marker=c(rep(7,11),rep(6,29),rep(5,69),rep(4,261))
cut.values=sort(unique(marker)[-1])
out1 <- APBinary(status,marker,cut.values)
out1
out2 <- APBinary(status,marker,method="perturbation",
	alpha=0.90,B=1500)
out2
}

