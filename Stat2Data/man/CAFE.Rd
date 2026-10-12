\name{CAFE}
\alias{CAFE}
\docType{data}
\title{US Senate Votes on Corporate Average Fuel Economy Bill}
\description{
Senate votes for Corporate Average Fuel Economy (CAFE) bill
}
\format{
  A data frame with 100 observations on the following 7 variables.
  \tabular{rl}{
    \code{Senator} \tab {Senator's name}\cr
    \code{State} \tab {Code for senator's state}\cr
    \code{Party} \tab {party affiliation: \code{D}=Democrat, \code{I}=Independent, \code{R}=Republican}\cr
    \code{Contribution} \tab {Contributions from car manufactures (dollars)}\cr
    \code{LogContr} \tab {Log of (Contribution+1)}\cr
    \code{Dem} \tab {\code{1}=Democrat/Independent \code{0}=Republican}\cr
    \code{Vote} \tab {\code{1}=yes  or \code{0}=no}\cr
  }
}
\details{
The Corporate Average Fuel Economy (CAFE) Bill was proposed by Senators John McCain and
John Kerry to improve the fuel economy of cars and light trucks sold in the United States.
However a critical vote on an amendment in March of 2002 threatened to indefinitely postpone CAFE.
The amendment charged the National Highway Traffic Safety Administration to develop a new standard,
the effect being to put on indefinite hold the McCain-Kerry bill. It passed by a vote of 62-38.
A political question of interest is whether there is evidence of monetary influence on a senator's
vote. Scott Preston, a professor of statistics at SUNY, Oswego, collected data on this vote which
includes the vote of each senator (1=Yes or 0=No) and
monetary contributions that each of the 100 senators received over his or her lifetime from the car
manufacturers.
}
\source{
Thanks to Prof. Scott Preston from SUNY Oswego for the data.
}

\keyword{datasets}
