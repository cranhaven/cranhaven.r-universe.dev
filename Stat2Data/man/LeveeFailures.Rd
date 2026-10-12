\name{LeveeFailures}
\alias{LeveeFailures}
\docType{data}
\title{Levee Failures along the Mississippi River}
\description{
Factors relating to Mississippi River levee failure
}
\format{
  A data frame with 82 observations on the following 14 variables.
  \describe{
    \item{\code{Failure}}{Did the levee fail? (\code{1}=yes or \code{0}=no)}
    \item{\code{Year}}{Year}
    \item{\code{RiverMile}}{Location along the river (mile marker)}
    \item{\code{Sediments}}{Sediments present? (\code{1}=yes or \code{0}=no)}
    \item{\code{BorrowPit}}{Borrow pit present? (\code{1}=yes or \code{0}=no)}
    \item{\code{Meander}}{Type of meander (\code{1}=inside bend, \code{2}=outside bend, \code{3}=chute, \code{4}=straight)}
    \item{\code{ChannelWidth}}{Width of the river channel (in meters)}
    \item{\code{FloodwayWidth}}{Width of floodway (in meters, levee to levee, levee to bluff, or bluff to bluff, as appropriate)}
    \item{\code{ConstrictionFactor}}{Constriction of the floodway over time (1880s to present)}
    \item{\code{LandCover}}{\code{1}=open water, \code{2}=grassy, \code{3}=agricultural, \code{4}=forest}
    \item{\code{VegWidth}}{Vegative buffer width (in meters)}
    \item{\code{Sinuosity}}{River length divided by valley length for 10 miles up- and down-valley from levee site}
    \item{\code{Dredging}}{Dredging intensity}
    \item{\code{Revetement}}{Is there a stone structure (wall) meant to hold up the bank?  (\code{1}=yes or \code{0}=no)}
  }
}
\details{
The goal of this investigation was to test the relative importance of geologic, geomorphic, and other physical factors that have led to levee failures through the past century along much of the Mississippi River.
}
\source{
A. Flor, N. Pinter, W.F. Remo (2010), "Evaluating Levee Failure Susceptibility on the Mississippi River Using Logistic Regression Analysis," Engineering Geology, Vol. 116, pp. 139-148


}
\keyword{datasets}
