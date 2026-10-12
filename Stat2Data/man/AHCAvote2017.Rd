\name{AHCAvote2017}
\alias{AHCAvote2017}
\docType{data}
\title{Congressional Votes on American Health Care Act (in 2017)}
\description{
Congressional votes on the American Health Care Act in 2017
}
\format{
  A data frame with 430 observations on the following 11 variables.
  \describe{
    \item{\code{STATE}}{State name}
    \item{\code{Dist}}{Congressional district}
    \item{\code{Party}}{Party affiliation (\code{D}=Democrat, \code{R}=Republican)}
    \item{\code{Dem}}{\code{1}=Democrat, \code{0}=Republican}
    \item{\code{Rep}}{\code{1}=Republican, \code{0}=Democrat}
    \item{\code{uni2013}}{Percentage of citizens without health care in 2013}
    \item{\code{uni2015}}{Percentage of citizens without health care in 2015}
    \item{\code{uniChange}}{uni2015 - uni2013}
    \item{\code{Member}}{Name of representative}
    \item{\code{AHCAvote}}{\code{1}=yes, \code{0}=no}
    \item{\code{Trump}}{\code{1}=Trump won district, \code{0}=Clinton won district}
  }
}
\details{
On May 4, 2017, the U.S. House of Representatives voted, by the narrow margin of 217-213, to pass the American Health Care Act. Most Republicans voted Yes, while all Democrats voted No.
}
\source{
https://fivethirtyeight.com/features/obamacare-has-increased-insurance-coverage-everywhere/

https://docs.google.com/spreadsheets/d/1VfkHtzBTP5gf4jAu8tcVQgsBJ1IDvXEHjuMqYlOgYbA/edit#gid=0

https://www.nytimes.com/interactive/2017/05/04/us/politics/house-vote-republican-health-care-bill.html

}
\keyword{datasets}
