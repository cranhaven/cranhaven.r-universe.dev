\name{phones}
\alias{phones}
\docType{data}
\title{  CBS Telephone Survey    }
\description{
  Respondents to the CBS telephone survey classified by opinion, number of
  residential telephone lines, and number of adults in the household. 
}
\usage{data(phones)}
\format{
  A data frame with 27 observations on the following 7 variables.
  \describe{
    \item{\code{adults}}{number of adults in household}
    \item{\code{preference}}{a factor with levels \code{Bush} \code{Dukakis} 
                                                  \code{No opinion/other}}
    \item{\code{lines.1}}{number with one tele line}
    \item{\code{lines.2}}{number with 2 tele lines}
    \item{\code{lines.3}}{number with 3 tele lines}
    \item{\code{lines.4}}{number with 4 tele lines}
    \item{\code{lines.Q}}{number with unknown tele lines}
  }
}
\details{
  This is used in exercises pages 242--243 in the book.
}
\examples{
data(phones)     
summary(phones)
}
\keyword{datasets}
