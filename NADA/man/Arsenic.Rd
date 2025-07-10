\name{Arsenic}
\alias{Arsenic}
\alias{Arsenic}
\docType{data}
\title{Dissolved arsenic concentrations in ground water of U.S.}
\description{
    This dataset is a random selection of dissolved arsenic analyses taken
    during the U.S. Geological Survey's National Water Quality Assessment
    program (NAWQA).  
}
\usage{
    data(Arsenic)
}
\format{
    A list containing 50 observations with items `As', `AsCen', `Aquifer'.
    `As' is a numeric vector of all arsenic observations (both censored
    and uncensored).  `AsCen' is a logical vector indicating where
    an element of `As' is censored (a less-than value).  `Aquifer' is
    a grouping factor of hypothetical hydrologic sources for the data.

}
\source{U.S. Geological Survey National Water Quality Assessment
        Data Warehouse}
\references{
    The USGS NAWQA site at \url{http://water.usgs.gov/nawqa}
}
\keyword{datasets}
