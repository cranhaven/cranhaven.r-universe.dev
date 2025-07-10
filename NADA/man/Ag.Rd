\name{Ag}
\alias{Silver}
\docType{data}
\title{Dissolved silver concentrations from water analyses} 
\description{
    This dataset was used by Helsel and Cohn (1988) to verify their
    software.  It is provided for code validation purposes.
}
\usage{data(Silver)}
\format{
    A list containing 56 observations with items 'obs' and 'censored'.
    'obs' is a numeric vector of all observations (both censored
    and uncensored).  'censored' is a logical vector indicating where
    an element of 'obs' is censored (a less-than value).
}
\source{Helsel and Cohn (1988)}
\references{
    Dennis R. Helsel and Timothy A. Cohn (1988), Estimation of descriptive
    statistics for multiply censored water quality data, Water Resources
    Research vol. 24, no. 12, pp.1997-2004 
}
\keyword{datasets}
