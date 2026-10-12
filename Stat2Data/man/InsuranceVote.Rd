\name{InsuranceVote}
\alias{InsuranceVote}
\docType{data}
\title{Congressional Votes on a Health Insurance Bill}
\description{
Congressional votes on an ObamaCare health insurance bill in 2009
}
\format{
  A dataset with 435 observations on the following 9 variables.
  \tabular{rl}{
    \code{Party} \tab {Party affiliation: \code{D}=Democrat or \code{R}=Republican}\cr
    \code{Dist.} \tab {Congressional district (State-Number)}\cr
    \code{InsVote} \tab {Vote on the health insurance bill: \code{1}=yes or \code{0}=no}\cr
    \code{Rep} \tab {Indicator for Republicans}\cr
    \code{Dem} \tab {Indicator for Democrats}\cr
    \code{Private} \tab {Percentage of non-senior citizens in district with private health insurance}\cr
    \code{Public} \tab {Percentage of non-senior citizens in district with public health insurance}\cr
    \code{Uninsured} \tab {Percentage of non-senior citizens in district with no health insurance}\cr
    \code{Obama} \tab {District winner in 2008 presidential election: \code{1}=Obama \code{0}=McCain}\cr
  }
}
\details{
On 7 November 2009 the U.S. House of Representatives voted, by the
narrow margin of 220-215, for a bill to enact health insurance reform. Most Democrats voted yes
while almost all Republicans voted no. This dataset contains data for each of the 435
representatives.
}
\source{
Insurance data are from the American Community Survey\cr
(http://www.census.gov/acs/www/data_documentation/data_main/).
Roll call of congressional votes on this bill can be found
at \cr
http://clerk.house.gov/evs/2009/roll887.xml.
}

\keyword{datasets}
