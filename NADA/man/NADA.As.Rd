\name{As}
\alias{As}
\alias{NADA.As}
\docType{data}
\title{Dissolved arsenic concentrations in ground water of U.S.}
\description{
    This dataset is a random selection of dissolved arsenic analyses taken
    during the U.S. Geological Survey's National Water Quality Assessment
    program (NAWQA).  
}
\usage{
    data(NADA.As)
}
\format{
    A list containing 50 observations with items `obs' and `censored'.
    `obs' is a numeric vector of all arsenic observations (both censored
    and uncensored).  `censored' is a logical vector indicating where
    an element of `obs' is censored (a less-than value).
}
\source{U.S. Geological Survey National Water Quality Assessment
        Data Warehouse}
\references{
    The USGS NAWQA site at \url{http://water.usgs.gov/nawqa}
}
\keyword{datasets}
