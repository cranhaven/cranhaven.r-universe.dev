\name{Election16}
\alias{Election16}
\docType{data}
\title{2016 U.S. Presidential Election}
\description{
2016 presidential election and state demographic data
}
\format{
  A data frame with 50 observations on the following 8 variables.
  \describe{
    \item{\code{State}}{State name}
    \item{\code{Abr}}{Abbreviation for state name}
    \item{\code{Income}}{Per capita income in the state}
    \item{\code{HS}}{Percent high school grads}
    \item{\code{BA}}{Percent college grads}
    \item{\code{Adv}}{Percent with advanced degrees}
    \item{\code{Dem.Rep}}{Democratic lean - Republican lean in 2015 Gallup poll}
    \item{\code{TrumpWin}}{Trump won the state? (\code{1}=yes or \code{0}=no)}
  }
}
\details{
This dataset contains information from all 50 states and the District of Columbia for the 2016 U.S. presidential election.  It is similar to Election08 for the 2008 election.
}
\source{
Income data from\cr
https://www.census.gov/search-results.html?q=per+capita+income+by+state&search.x=0 &search.y=0&search=submit&page=1&stateGeo=none&searchtype=web&cssp=SERP

2015 data via American Community Survey\cr
https://en.wikipedia.org/wiki/List_of_U.S._states_by_educational_attainment
from Bureau, U.S. Census. "2011-2015 American Community Survey 5-Year Estimates. factfinder.census.gov. Retrieved 2017-01-19.

http://www.gallup.com/poll/188969/red-states-outnumber-blue-first-time-gallup-tracking.aspx

}
\keyword{datasets}
