\name{ICU}
\alias{ICU}
\docType{data}
\title{Intensive Care Unit Patients}
\description{
Data for a sample of 200 patients at an Intensive Care Unit (ICU)
}
\format{
  A data frame with 200 observations on the following 9 variables.
  \tabular{rl}{
    \code{ID} \tab {Patient ID code}\cr
    \code{Survive} \tab {\code{1}=patient survived to discharge or \code{0}=patient died}\cr
    \code{Age} \tab {Age (in years)}\cr
    \code{AgeGroup} \tab {\code{1}= young (under 50), \code{2}= middle (50-69), \code{3} = old (70+)}\cr
    \code{Sex} \tab {\code{1}=female or \code{0}=male}\cr
    \code{Infection} \tab {\code{1}=infection suspected or \code{0}=no infection}\cr
    \code{SysBP} \tab {Systolic blood pressure (in mm of Hg)}\cr
    \code{Pulse} \tab {Heart rate (beats per minute)}\cr
    \code{Emergency} \tab {\code{1}=emergency admission or \code{0}=elective admission}\cr
  }
}
\details{
This dataset contains information for a sample of 200 patients
who were part of a larger study conducted in a hospital's Intensive Care Unit (ICU). Since an ICU
often deals with serious, life-threatening cases, a key variable to study is patient survival, which is
coded in the Survive variable as 1 if the patient lived to be discharged and 0 if the patient died.
}
\source{
Data downloaded from The Data and Story Library (DASL), http://lib.stat.cmu.edu/DASL/Datafiles/ICU.html.
}

\keyword{datasets}
