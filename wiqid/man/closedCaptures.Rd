\name{Closed Captures}
\alias{closedCapM0}
\alias{closedCapMb}
\alias{closedCapMt}
\alias{closedCapMtcov}
\alias{closedCapMh2}
\alias{closedCapMhJK}

\title{
Analysis of mark-recapture data for closed populations
}
\description{
Functions to analyse the classical models for closed populations without individual covariates, ie. full likelihood models.
}
\usage{
closedCapM0(CH, ci = 0.95, ciType=c("normal", "MARK"), ...)

closedCapMb(CH, ci = 0.95, ciType=c("normal", "MARK"), ...)

closedCapMt(CH, ci = 0.95, ciType=c("normal", "MARK"), ...)

closedCapMtcov(CH, model=list(p~1), data=NULL, ci = 0.95,
    ciType=c("normal", "MARK"), ...)

closedCapMh2(CH, ci = 0.95, ciType=c("normal", "MARK"), ...)

closedCapMhJK(CH, ci = 0.95)

}
\arguments{
  \item{CH}{
a 0/1 capture history matrix, animals x occasions; NAs not permitted. For functions \code{closedCapM0}, \code{closedCapMh2} and \code{closedCapMhJK}, \code{CH} can be a vector of capture frequencies of length equal to the number of occasions - trailing zeros are required.
}
  \item{model}{
a list of formulae symbolically defining a linear predictor for p in terms of covariates.
}
  \item{data}{
a data frame containing the variables in the model, with one row for each occasion.
}
  \item{ci}{
the required confidence interval.
}
  \item{ciType}{
the method used to calculate the confidence interval for population size (N); see Details.
}
  \item{...}{
other arguments passed to \code{\link{nlm}}.
}
}
\details{
Model M0 assumes all animals have the same capture probability.

Model Mb allows for a permanent behavioural response: capture probability is different for animals that have already been captured at least once.

Model Mh2 and the jackknife estimator allow for heterogeneity in capture probability.

The likelihood maximization routine produces an estimate and standard error for beta = log(\emph{f0}), where \emph{f0} is the number of animals never captured. If \code{ciType == "normal"}, a confidence interval for beta is calculated as beta +/- crit * SE.beta, where crit is the appropriate multiplier for the confidence interval required, 1.96 for a 95\% CI. This confidence interval is then back-transformed to the real scale and added to the number of animals captured (M[t+1]) to give estimates of N.

If \code{ciType == "MARK"}, the method used by MARK to calculate confidence intervals is used (see MARK help page for Closed Capture Models and Burnham et al (1987, p212)).
The beta values are back-transformed with f0.hat = exp(beta) and SE.f0 = SE.beta * f0.hat, and hence CV = SE.f0 / f0.hat. The confidence limits are then

Lower = f0.hat / C + M[t+1]

Upper = f0.hat * C + M[t+1]

where C = exp(crit * sqrt(log(1 + CV^2))).

Confidence intervals for capture probabilities are always calculated on the logit scale and back-transformed to real values.
}
\value{
Returns an object of class \code{wiqid}, which is a list with the following elements:

  \item{call}{The call used to produce the results}
  \item{beta}{Values of the coefficients of the terms in the linear predictors, with standard errors and confidence intervals.}
  \item{beta.vcv }{The variance-covariance matrix for the beta estimates.}
  \item{real}{Estimates of population size (N) and probability of detection on the real scale, with confidence intervals.}
  \item{logLik}{a vector with elements for log(likelihood), number of parameters, and effective sample size. If the variance-covariance matrix cannot be calculated, the second element should be \code{NA}.}

The jackknife estimator does not use likelihood maximisation, so elements \code{beta} and \code{beta.vcv} are NULL and \code{logLik = NA}.

There are \code{print}, \code{logLik}, and \code{nobs} methods for class \code{wiqid}.
}
\references{
Basic work on mark-recapture for closed populations is in:

Otis, D L; K P Burnham; G C White; D R Anderson. 1978. Statistical inference from capture data on closed animal populations. \emph{Wildlife Monographs} 62:1-135.

White, G C; D R Anderson; K P Burnham; D L Otis. 1982. \emph{Capture-recapture and removal methods for sampling closed populations}. Los Alamos National Laboratory, Los Alamos NM.

Calculation of the confidence interval for N is in:

Burnham, K.P., Anderson, D.R., White, G.C., Brownie, C., & Pollock, K.H. 1987. \emph{Design and analysis methods for fish survival experiments based on release-recapture}. American Fisheries Society, Bethesda MD.

The jackknife estimator is described in:

Burnham, K P; W S Overton. 1979. Robust estimation of population size when capture probabilities vary among animals. \emph{Ecology} 60:927-936.

Rexstad, E; K Burnham 1992. \emph{User's guide for interactive program CAPTURE}. USGS Patuxent.

Data sets in the examples are from:

White et al, op. cit.

Karanth, Nichols, Kumar, Link, Hines (2004) Tigers and their prey: Predicting carnivore densities from prey abundance. PNAS 101:4854-4858

}
\author{
Mike Meredith
}

\examples{
# Data from White et al (1982):
freq1 <- c(50, 46, 35, 24, 14, 5, 0) # there were 7 capture occasions
closedCapM0(freq1)
closedCapM0(freq1, ci=0.8)
closedCapMh2(freq1)
closedCapMhJK(freq1)

# Kanha tiger data from Karanth et al (2004)
data(KanhaTigers)
closedCapM0(KanhaTigers)
closedCapMb(KanhaTigers)
closedCapMh2(KanhaTigers)
closedCapMhJK(KanhaTigers)
closedCapMt(KanhaTigers)
closedCapMtcov(KanhaTigers, p~.Time)
# Generate some mythical covariates:
covars <- data.frame(Temp = runif(ncol(KanhaTigers), 15, 25),
  Cloud = sample(0:8, ncol(KanhaTigers), replace=TRUE))
closedCapMtcov(KanhaTigers, p~Cloud, data=covars)

# Compare the normal (default) and MARK confidence intervals for N:
rbind(closedCapMt(KanhaTigers)$real[1, ],
      closedCapMt(KanhaTigers, ciType="MARK")$real[1, ])
}
