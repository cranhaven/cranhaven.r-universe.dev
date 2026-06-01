\name{aids.id}
\alias{aids.id}
\docType{data}
\title{
Survival Time Data for each of Patients on the AIDS Data
}
\description{
In the AIDS data, survival data can be summarized for each of patients.  Times to death were also recorded with 40 percent of censoring rate.
}
\usage{data("aids.id")}
\format{
  A data frame with 467 observations on the following 12 variables.
  \describe{
    \item{\code{patient}}{patients identifier for 467 patients}
    \item{\code{Time}}{time to death or censoring}
    \item{\code{death}}{status with 0 denoting censoring and 1 death for patient}
    \item{\code{CD4}}{CD4 cells count}
    \item{\code{obstime}}{time points at which the CD4 cells count was recorded}
    \item{\code{drug}}{a factor with levels \code{ddC} (zalcitabine) and \code{ddI} (didanosine)}
    \item{\code{gender}}{a factor with levels \code{female} and \code{male}}
    \item{\code{prevOI}}{a factor with levels \code{noAIDS} (previous opportunistic infection at study entry) and \code{AIDS} (no previous infection)}
    \item{\code{AZT}}{a factor with levels \code{intolerance} (AZT intolerance) and \code{failure} (AZT failure)}
    \item{\code{start}}{start of time in the first interval}
    \item{\code{stop}}{end of time in the first interval}
    \item{\code{event}}{status with 0 denoting censoring and 1 death in the first interval}
  }
}

\references{
Rizopoulos, D. (2015). JM: Joint Modeling of Longitudinal and Survival
Data. R package version 1.4-2.
}
