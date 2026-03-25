\name{APSurv}
\alias{APSurv}
\title{Estimating the Time-dependent AP and AUC for Censored Time to Event Outcome Data.}
\description{
  This function calculates the estimates of the AP and AUC
  for censored time to event data as well as their
  confidence intervals using the perturbation or the
  nonparametric bootstrap resampling method. The estimation
  method is based on Yuan, Y., Zhou, Q. M., Li, B., Cai, H., Chow, E. J., Armstrong, G. T. (2018). A threshold-free summary index of prediction accuracy for censored time to event data. Statistics in medicine, 37(10), 1671-1681.
}
\usage{
  APSurv(stime, status, marker, t0.list, cut.values = NULL,
    method = "none", alpha = 0.95, B = 1000,
    weight = NULL, Plot = TRUE)
}
\arguments{
  \item{stime}{Censored event time.}

  \item{status}{Binary indicator of censoring. 1 indicates
  observing event of interest, 0 otherwise. Other values will be treated as competing risk event.}

  \item{marker}{Numeric risk score. Data
  can be continuous or ordinal.}

  \item{t0.list}{Prediction time intervals of interest. It
  could be one numerical value or a vector of numerical
  values, which must be in the range of stime.}

  \item{cut.values}{Risk score values to use as a cut-off for
  calculation of time-dependent positive predictive values
  (PPV) and true positive fractions (TPF). The default
  value is NULL.}

  \item{method}{Method to obtain confidence intervals. The
  default is method = "none", in which case only point estimates will be given without confidence intervals.
  If method= "perturbation", then perturbation based CI will be calculated.
  If method = "bootstrap", then nonparametric bootstrap based CI will be calculated.}

  \item{alpha}{Confidence level. The default level is
  0.95.}

  \item{B}{Number of resampling to obtain a confidence
  interval. The default value is 1000.}

  \item{weight}{Optional. The default value is NULL, in
  which case the observations are weighted by the inverse
  of the probability that their respective time-dependent
  event status (whether the event occurs within a specified
  time period) is observed. In estimating the probability,
  the survival function of the censoring time is estimated
  by a Kaplan-Meier estimator under the assumption that the
  censoring time is independent of both the event time and
  risks score.  Users can use their own weights, in which
  case the t0.list should be a scalar and the length of
  weight is required to be the same as the length of
  status.}

  \item{Plot}{Whether to plot the time-dependent AP versus
  the prediction time intervals.  The default value is
  TRUE, in which case the AP is evaluated at the time
  points which partition the range of the event times of
  the data into 100 intervals.}
}
\value{
  An object of class "APsurv" which is a list with
  components:

  \item{ap_summary}{Summary of estimated AP(s) at the specified
  prediction time intervals of interest. For each
  prediction time interval, the output includes the
  estimated event rate, a point estimate of the AP, the
  estimated scaled AP (ratio of the AP versus event rate),
  and their corresponding confidence intervals.}

  \item{auc_summary}{Summary of AUC at the specified prediction
  time intervals of interest. For each prediction time
  intervals, the output includes the estimated event rate
  and a point estimate of AUC with a confidence interval.}

  \item{PPV}{Available object, time-dependent positive predictive
  values at the unique risk score in the data.}

  \item{TPF}{Available object, time-dependent true positive
  fractions at the unique risk score in the data.}
}
\references{
  Yuan, Y., Zhou, Q. M., Li, B., Cai, H., Chow, E. J., Armstrong, G. T. (2018). A threshold-free summary index of prediction accuracy for censored time to event data. Statistics in medicine, 37(10), 1671-1681.

  Bingying Li (2015) Threshold-free Measure for Assessing
  the Performance of Risk Prediction with Censored Data,
  MSc. thesis, Simon Fraser University, Canada
}
\examples{
library(APtools)
data(mayo)
t0.list=seq(from=min(mayo[,1]),to=max(mayo[,1]),length.out=5)[-c(1,5)]
cut.values=seq(min(mayo[,3]),max(mayo[,3]),length.out=10)[-10]
out <- APSurv(stime=mayo[,1],status=mayo[,2],marker=mayo[,3],
	t0.list=t0.list,cut.values=cut.values,method='bootstrap',
	alpha=0.90,B=500,weight=rep(1,nrow(mayo)),Plot=FALSE)
out
}

