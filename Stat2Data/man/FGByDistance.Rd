\name{FGByDistance}
\alias{FGByDistance}
\docType{data}
\title{Results of NFL Field Goal Attempts}
\description{
Field goal results in the National Football League (NFL) by distance
}
\format{
  A data frame with 51 observations on the following 7 variables.
  \tabular{rl}{
    \code{Row} \tab {Case ID}\cr
    \code{Dist} \tab {Distance of the attempt (in yards)}\cr
    \code{N} \tab {Number of kicks attempted from that distance}\cr
    \code{Makes} \tab {Number of kicks made from that distance}\cr
    \code{PropMakes} \tab {Proportion of attempts made}\cr
    \code{Blocked} \tab {Number of kicks blocked}\cr
    \code{PropBlocked} \tab {Proportion of kicks blocked}\cr
  }
}
\details{
This dataset summarizes all 8520 field goals attempted by place kickers in the National Football League (NFL)
during regular season games for the 2000 through the 2008 seasons.
Results are counts (attempted, made, and blocked) and proportions (made and blocked) for each distance.
}
\source{
We thank Sean Forman and Doug Drinen of Sports Reference LLC for providing us with the NFL field goal data
set.
}

\keyword{datasets}
