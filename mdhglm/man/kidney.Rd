\name{kidney}
\alias{kidney}
\docType{data}
\title{
Kidney Infection Data
}
\description{
This data consist of times until the first and second recurrences of kidney infection
in 38 patients using a portable dialysis machine (McGilchrist and Aisbett, 1991; Therneau and Lumley, 2015). Each survival
time is the time until infection since the insertion of the catheter.
}
\usage{data("kidney")}
\format{
  A data frame with 76 observations on the following 7 variables.
  \describe{
    \item{\code{id}}{patients identifier for 38 patients}
    \item{\code{time}}{time until infection since the insertion of the catheter}
    \item{\code{status}}{event stuatus (=1; infection, =0; censoring)}
    \item{\code{age}}{age of patients in years}
    \item{\code{sex}}{1=male, 2=female}
    \item{\code{disease}}{a factor for disease type with levels \code{Other}, \code{GN}, \code{AN} and \code{PKD}}
    \item{\code{frail}}{frailty estimate from original paper}
  }
}
\references{
McGilchrist, C.A. and Aisbett, C.W. (1991). Regression with frailty in
survival analysis. Biometrics, 47, 461--466.

Therneau, T. and Lumley, T. (2015). survival: Survival Analysis. R
package version 2.38-3.
}

