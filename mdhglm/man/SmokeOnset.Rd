\name{SmokeOnset}
\alias{SmokeOnset}
\docType{data}
\title{Smoke Onset Data
}
\description{
For 1,556 students in the Los Angels area, onset of smoking are
observed at each of three timepoints a1; a2 and a3 where a1 is starting
time for investigation, a2 is the 1 year follow-up and a3 is the 2 year
follow-up (Hedeker, et al., 2015). These event times are grouped at the three intervals [0, a1),
[a1, a2) and [a2, a3).
}
\usage{data("SmokeOnset")}
\format{
  A data frame with 1556 observations on the following 10 variables.
  \describe{
    \item{\code{school}}{schools identifier in the Los Angels area}
    \item{\code{class}}{classrooms identifier}
    \item{\code{student}}{students identifier}
    \item{\code{smkonset}}{coded by i when the event occurs in the ith time interval}
    \item{\code{event}}{=1 (smoked), =0 (otherwise)}
    \item{\code{int}}{vector of ones representing intercept}
    \item{\code{SexMale}}{gender of the student (0=Female, 1=Male)}
    \item{\code{cc}}{indicating whether the school was randomized to a social-resistance classroom
curriculum (1=Yes, 0=No)}
    \item{\code{tv}}{indicating whether the school was randomized to a media (television) intervention
(1=Yes, 0=No)}
    \item{\code{cctv}}{indicating whether the school was randomized to CC combined with TV}
  }
}
\references{
Hedeker, D., Archer, K.J., Nordgren, R. and gibbons, R.D. (2015). mixor: Mixed-Effects Ordinal Regression Analysis. R
package version 1.0.3.
}

