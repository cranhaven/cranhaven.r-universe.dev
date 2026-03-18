\name{newyork}
\alias{newyork}
\docType{data}
\title{ Population of Municipalities in New York  }
\description{
   Population in all 804 municipalities in new York state in 1960, and two
   independent random samples from it. 
}
\usage{data(newyork)}
\format{
  A data frame with 804 observations on the following 2 variables.
  \describe{
    \item{\code{population}}{a numeric vector Population}
    \item{\code{code}}{ 400 if in sample 1, 300 if in sample 2, 200
                if in both, and 100 if in neither}
  }
}
\details{
  Discussed on page 265 of second edition.
}
\examples{
data(newyork)
str(newyork)
}
\keyword{datasets}
