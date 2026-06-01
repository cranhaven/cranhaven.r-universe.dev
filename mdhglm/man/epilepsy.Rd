\name{epilepsy}
\alias{epilepsy}
\docType{data}
\title{
Epilepsy Seizures Data
}
\description{
Thall and Vail (1990) presented longitudinal data from a clinical trial of 59 epileptics, who were
randomized to a new drug or a placebo (T=0 or T=1). Baseline data were available at the start of
the trial; the trial included the logarithm of the average number of epileptic seizures recorded in the
8-week period preceding the trial (B), the logarithm of age (A), and visit (V: a linear trend, coded
(-3,-1,1,3)/10). A multivariate response variable (y) consists of the seizure counts during 2-week
periods before each of four visits to the clinic.
}
\usage{data("epilepsy")}
\format{
  A data frame with 236 observations on the following 7 variables.
  \describe{
    \item{\code{y}}{seizure counts during 2-week periods before each of four visits to the clinic}
    \item{\code{T}}{treatment (0=new drug, 1=placebo)}
    \item{\code{B}}{average number of epileptic seizures in the 8-week period preceding the trial}
    \item{\code{A}}{logarithm of age (in years) of each patient}
    \item{\code{V}}{linear trend coded (-3, -1, 1, 3)/10 for four visits of each patient}
    \item{\code{patient}}{patient number for 59 epileptics}
    \item{\code{id}}{observation number for 236 observations}
  }
}
\references{
Thall, P.F. and Vail, S.C. (1990). Some covariance models for longitudinal count data with overdispersion,
Biometrics, 46, 657--671.
}

