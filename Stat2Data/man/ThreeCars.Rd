\name{ThreeCars}
\alias{ThreeCars}
\docType{data}
\title{Prices of Three Used Car Models (2007)}
\description{
Compare prices for Porsche, Jaguar, and BMW cars offered for sale at an internet site
}
%\useage{data(ThreeCars)}
\format{
  A dataset with 90 observations on the following 8 variables.
  \tabular{rl}{
    \code{CarType} \tab {\code{BMW}, \code{Jaguar}, or \code{Porsche}}\cr
    \code{Price} \tab {Asking price (in $1,000's)}\cr
    \code{Age} \tab {Age of the car (in years)}\cr
    \code{Mileage} \tab {previous miles driven (in 1,000's)}\cr
    \code{Car} \tab {\code{0}=Porsche, \code{1}=Jaguar, \code{2}=BMW}\cr
    \code{Porsche} \tab {Indicator with \code{1}= Porsche and \code{0}=otherwise}\cr
    \code{Jaguar} \tab {Indicator with \code{1}= Jaguar and \code{0}=otherwise}\cr
    \code{BMW} \tab {Indicator with \code{1}= BMW and \code{0}=otherwise}\cr
  }
}
\details{
Two students collected samples of Porsche, Jaguar, and BMW cars that were offered for sale at an internet site.
In addition to asking price, they recorded the model year (converting to age) and mileage of each advertised car.
The PorschePrice dataset (from the first edition) has only the Porsche data and the PorscheJaguar dataset has the data for those two models.

This dataset has been updated (with different car models) to ThreeCars2017 for the second edition.
}
\source{
Student project data collected from autotrader.com in Spring 2007.
}

\keyword{datasets}
