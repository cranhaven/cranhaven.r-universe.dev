\name{CompareAP}
\alias{CompareAP}
\title{Comparison of two risk scores based on the differences and ratio of their APs.}
\description{
  This function estimates the difference between and the
  ratio of two APs in order to compare two markers for
  censored time to event data or binary data. The
  corresponding confidence intervals are provided.
}
\usage{
  CompareAP(status, marker1, marker2, stime = NULL,
    t0.list = NULL, method = "none", alpha = 0.95,
    B = 1000, weight = NULL, Plot = TRUE)
}
\arguments{
  \item{status}{Binary indicator. For binary data, 1
  indicates case and 0 otherwise. For survival data, 1
  indicates event and 0 otherwise.}

  \item{marker1}{Risk score 1 (to be compared to risk score
  2). Its length is required to be the same as the length
  of status.}

  \item{marker2}{Risk score 2 (to be compared to risk score
  1). Its length is required to be the same as the length
  of status.}

  \item{stime}{Censored event time. If dealing with binary
  outcome, skip this argument which is set to be NULL.}

  \item{t0.list}{Prediction time intervals of interest for
  event time outcome. It could be one numerical value or a
  vector of numerical values, which must be in the range of
  stime. It is set to be NULL if stime is NULL.}

  \item{method}{Method to obtain confidence intervals. The
  default is method = "none", in which case only point estimates will be given without confidence intervals.
  If method= "perturbation", then perturbation based CI will be calculated.
  If method = "bootstrap", then nonparametric bootstrap based CI will be calculated.}

  \item{alpha}{Confidence level. The default level is
  0.95.}

  \item{B}{Number of resampling for obtaining a confidence
  interval. The default value is 1000.}

  \item{weight}{Optional argument for event time data, i.e.
  stime is not NULL. Its default value is NULL, in which
  the observations are weighted by the inverse of the
  probability that their respective time-dependent event
  status (whether the event occurs within a specified time
  period) is observed. In estimating the probability, the
  survival function of the censoring time is estimated by a
  Kaplan-Meier estimator under the assumption that the
  censoring time is independent of both the event time and
  risks score. Users can use their own weights, in which
  case the t0.list should be a scalar and the length of
  weight is required to be the same as the length of
  status.}

  \item{Plot}{Optional argument for event time data, i.e.
  stime is not NULL. For binary data, it is set to FALSE.
  For event time data, its default value is TRUE and three
  plots are generated: 1) the time-dependent AUC of two
  markers; 2) the time-dependent AP of two markers; and 3)
  the time-dependent ratio of APs, all versus the
  prediction time intervals. The quantities in 1)-3) are
  evaluated at the time points which partition the range of
  the event times of the data to 100 intervals.}
}
\value{
  \item{dap_summary}{Summary of the APs of two markers and the
  differences (AP1-AP2) and their ratio (AP1/AP2). For
  event time data, these quantities are estimated at the
  specified prediction time intervals. The output includes
  the estimated event rate/proportion of cases, point
  estimates of the APs of the two markers, point estimates
  of the difference between and ratio of the two APs as
  well as their respective confidence intervals.}
}
\references{
  Yuan, Y., Zhou, Q. M., Li, B., Cai, H., Chow, E. J., Armstrong, G. T. (2018). A threshold-free summary index of prediction accuracy for censored time to event data. Statistics in medicine, 37(10), 1671-1681.

  Yuan, Y., Su, W., and Zhu, M. (2015). Threshold-free
  measures for assessing the performance of medical
  screening tests. Frontiers in Public Health, 3.57.

  Bingying Li (2015) Threshold-free Measure for Assessing
  the Performance of Risk Prediction with Censored Data,
  MSc. thesis, Simon Fraser University, Canada
}
\examples{
library(APtools)
status=c(rep(1,10),rep(0,1),rep(1,18),rep(0,11),rep(1,25),
	rep(0,44),rep(1,85),rep(0,176))
marker1=c(rep(7,11),rep(6,29),rep(5,69),rep(4,261))
marker2=c(rep(7,17),rep(6,29),rep(5,70),rep(4,254))
out_binary <- CompareAP(status,marker1,marker2)
out_binary
data(mayo)
t0.list=seq(from=min(mayo[,1]),to=max(mayo[,1]),length.out=5)[-c(1,5)]
out_survival <- CompareAP(status=mayo[,2],marker1=mayo[,3],
	marker2=mayo[,4],stime=mayo[,1],t0.list=t0.list,
	method='bootstrap',alpha=0.90,B=500,
	weight=rep(1,nrow(mayo)),Plot=FALSE)
out_survival
}

