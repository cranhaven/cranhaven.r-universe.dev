\name{CloudSeeding2}
\alias{CloudSeeding2}
\docType{data}
\title{Cloud Seeding Experiment (Four Seasons)}
\description{
Rainfall amounts from a cloud seeding experiment
}
\format{
  A data frame with 108 observations on the following 8 variables.
  \tabular{rl}{
    \code{Period} \tab {ID for time period}\cr
    \code{Seeded} \tab {Treatment coded as \code{S}=seeded or  \code{U}=unseeded}\cr
    \code{Season} \tab {Coded as \code{Autumn}, \code{Spring}, \code{Summer}, or \code{Winter}}\cr
   \code{TE} \tab {Rainfall in East (treatment)}\cr
    \code{TW} \tab {Rainfall in West (treatment}\cr
    \code{NC} \tab {Rainfall in North (control)}\cr
    \code{SC} \tab {Rainfall in South (control)}\cr
    \code{NWC} \tab {Rainfall in Northwest (control)}\cr
  }
}
\details{
Researchers were interested in whether seeded clouds would produce more
rainfall. An experiment was conducted in Tasmania between 1964 and 1971 and rainfall amounts
were measured in inches per rainfall period. The researchers measured the amount of rainfall in
two target areas: East (TE) and West (TW). They also measured the amount of rainfall in three
control locations. Clouds were coded as being either seeded (treatment) or unseeded (control).
A subset (only Winter months) of these data is stored in CloudSeeding.  All rainfall amounts are in inches.
}
\source{
Data were accessed from the website www.statsci.org/data/oz/cloudtas.html. This is the web home of the
Australasian Data and Story Library (OzDASL).
}
\references{
A.J. Miller, D.E. Shaw, L.G. Veitch, and E.J. Smith, (1979) "Analyzing the results of a
cloud-seeding experiment in Tasmania" in Communications in Statistics: Theory and Methods, A8 (10), pp. 1017-1047.
}

\keyword{datasets}
