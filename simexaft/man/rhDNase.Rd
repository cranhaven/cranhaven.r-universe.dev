\name{rhDNase}
\alias{rhDNase}
\docType{data}
\title{
rhDNase Data Set
}
\description{
This is a dataset reported by Fuchs et al. (1994) for a double-blind randomized multicenter clinical trial designed to evaluate the effect of rhDNase, a recombinant deoxyribonuclease I 
enzyme, versus placebo on the occurrence of respiratory exacerbations among patients with cystic fibrosis. Data on the occurrence and resolution of all exacerbations were recorded for 645 patients in this trial.
For more details about the dataset feature, see Cook and Lawless (2007). Here we only include the first record of the patients that have etype=1.
}
\usage{data(rhDNase)}
\format{
  A data frame with 641 observations on the following 11 variables.
  \describe{
    \item{\code{id}}{ patient identifier}
    \item{\code{trt}}{ the treatment assignment, trt=1 if patient receive rhDNase and 0 if patent receive placebo}
    \item{\code{fev}}{ baseline measurement of forced expiratory volume}
    \item{\code{fev2}}{ baseline measurement of forced expiratory volume}
    \item{\code{time1}}{ the start of a period indicating when subjects become " at risk" for a transition}
    \item{\code{time2}}{ if etype=1 then time2 corresponds the onset of an exacerbation (or censoring) 
    and if etype=2 then time2 corresponds to the time of a resolution of an exacerbation (or censoring)}
    \item{\code{status}}{ status equals 1 if time2 is a transition time and equals 0 if it is a censoring time}
    \item{\code{etype}}{ the indicator of the nature of the event time recorded in time2}
    \item{\code{enum}}{ the cumulative number of lines in the data frame for each individual}
    \item{\code{enum1}}{ the cumulative number of exacerbation-free periods}
    \item{\code{enum2}}{ a numeric vector}
  }
}

\source{
Cook, R. J. and Lawless, J. F. (2007). The Statistical Analysis of Recurrent Events. Springer, New York.
}

\seealso{\code{\link[simexaft]{simexaft}}}

\keyword{datasets}


