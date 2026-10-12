\name{CloudSeeding}
\alias{CloudSeeding}
\docType{data}
\title{Cloud Seeding Experiment (Winter Only)}
\description{
Rainfall amounts from a cloud seeding experiment (winter only)
}
\format{
  A data frame with 28 observations on the following 7 variables.
  \tabular{rl}{
    \code{Seeded} \tab {Treatment coded as \code{S}=seeded or  \code{U}=unseeded}\cr
    \code{Season} \tab {All in \code{Winter}}\cr
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
This is a subset (only Winter months) of the larger CloudSeeding2 dataset.  All rainfall amounts are in inches.
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
