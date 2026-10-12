\name{LostLetter}
\alias{LostLetter}
\docType{data}
\title{Return Rates for "Lost" Letters}
\description{
Which "lost" letters will be returned by the public?
}
\format{
  A data frame with 140 observations on the following 8 variables.
  \tabular{rl}{
    \code{Location} \tab {Where letter was "lost": \code{DesMoines}, \code{GrinnellCampus}, or \code{GrinnellTown}}\cr
    \code{Address} \tab {Address on the letter: \code{Confederacy} or \code{Peaceworks}}\cr
    \code{Returned} \tab {\code{1}=letter was returned or \code{0}=letter was not returned}\cr
    \code{DesMoines} \tab {Indicator for letters left in Des Moines}\cr
    \code{GrinnellTown} \tab {Indicator for letters left in the town of Grinnell}\cr
    \code{GrinellCampus} \tab {Indicator for letters left on the Grinnell campus}\cr
    \code{Peaceworks} \tab {Indicator for letters addressed to Iowa Peaceworks}\cr
    \code{Confederacy} \tab {Indicator for letters addressed to Friends of the Confederacy}\cr
  }
}
\details{
In 1999 Grinnell College students Laurelin Muir and Adam Gratch conducted an experiment for an introductory statistics class.
They intentionally "lost" 140 letters in either the
city of Des Moines, the town of Grinnell, or on the Grinnell College campus. Half of each sample
were addressed to Friends of the Confederacy and the other half to Iowa Peaceworks.
The students kept track of which letters were eventually returned.
}
\source{
Student project at Grinnell College
}

\keyword{datasets}
