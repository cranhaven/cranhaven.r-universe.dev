\name{Orings}
\alias{Orings}
\docType{data}
\title{Space Shuttle O-Rings}
\description{
Number of damaged O-rings on space shuttle launches and launch temperature
}
%\useage{data(Orings)}
\format{
  A dataset with 24 observations on the following 2 variables.
  \tabular{rl}{
    \code{Temp} \tab {Code for temperature (in degrees F): \code{Above65} \code{Below65}}\cr
    \code{Failures} \tab {Number of O-ring failures}\cr
  }
}
\details{
The space shuttle Challenger exploded shortly after liftoff in 1987.  The subsequent investigation focused on the
failure of O-ring seals, which allowed liquid hydrogen and oxygen to mix and explode. These failures might be
related to temperature at the launch site which was near freezing (32 degrees F) on that day.
This dataset shows the number of O-ring failures on previous shuttle launches, along with an indicator for whether the
temperature was above or below 65 degrees F.
}
\source{
Data can be found in "Risk analysis of the space shuttle: Pre-challenger prediction of failure" by Siddhartha R.
Dalal, Edward B. Fowlke, and Bruce Hoadley in Journal of the American Statistical Association, Vol. 84, No. 408
(Dec. 1989), pp 945-957
}

\keyword{datasets}
