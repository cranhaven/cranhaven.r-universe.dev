\name{Utilities}
\alias{parseInputs}
\alias{buildLegend}

\title{
Utility Functions for the \pkg{addScales} package
}
\description{
Non-exported functions meant for package internal use.
}
\usage{
parseInputs(val)

buildLegend(legend,
   lloc = c("top","bottom","right","left"),
   spacing = 1.3)
}
\arguments{
  \item{val}{
      \code{scaleline} or \code{legend} argument of the \code{addScales} call to be parsed
 }

  \item{legend}{
      \code{legend} list after being parsed: a list of 2 textGrobs with names \code{c("h","v")}
   }
  \item{lloc}{
      one of "top","bottom","left", or "right" where the legend will be placed
   }
  \item{spacing}{
      horizontal/vertical spacing between the \code{"h"} and
      and \code{"v"} components of the legend (which depends on
      \code{lloc})
   }
}

\value{
   For \code{parseInputs}, properly parsed 2-component lists with component names
   "h" and "v" in that order.

   For \code{buildLegend}, a  \pkg{grid} \code{frameGrob} for use as the
   \code{fun} component for a \code{legend} of a \code{trellis} object.
}



\author{
Bert Gunter \email{bgunter.4567@gmail.com}
}

\keyword{internal}

