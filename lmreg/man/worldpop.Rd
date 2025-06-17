\name{worldpop}
\alias{worldpop}
\docType{data}
\title{
World population data
}
\description{
The midyear population of the world for the years 1981-2000.
}
\usage{data(worldpop)}
\format{
  A data frame with 20 observations on the following 2 variables.
  \describe{
    \item{\code{Year}}{Calendar year}
    \item{\code{Pop.billion}}{Population (in billion)}
  }
}

\source{
U.S. Census Bureau, International Data Base (http://www.census.gov/ipc/www/idbnew.html)
}

\examples{
data(worldpop)
head(worldpop)
}
\keyword{datasets}
