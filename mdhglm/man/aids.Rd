\name{aids}
\alias{aids}
\docType{data}
\title{Repatead Measures on AIDS Data}
\description{
Both longitudinal and survival data were collected in a recent clinical
trial to compare the efficacy and safety of two antiretroviral drugs in
treating patients who had failed or were intolerant of zidovudine (AZT)
therapy (Rizopoulos, 2015). In this trial, 467 HIV-infected patients were
enrolled and randomly assigned to receive either didanosine (ddI) or
zalcitabine (ddC). The number of CD4 cells per cubic millimeter of blood
were recorded at study entry, and again at the 2, 6, 12, and 18 month
visits.
}
\usage{data("aids")}
\format{
  A data frame with 1405 observations on the following 12 variables.
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
    \item{\code{start}}{start of time in the interval}
    \item{\code{stop}}{end of time in the interval}
    \item{\code{event}}{status with 0 denoting censoring and 1 death in the interval}
  }
}

\references{
Rizopoulos, D. (2015). JM: Joint Modeling of Longitudinal and Survival
Data. R package version 1.4-2.
}

