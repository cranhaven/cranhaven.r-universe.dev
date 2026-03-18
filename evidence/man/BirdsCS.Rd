\name{BirdsCS}
\alias{BirdsCS}
\docType{data}
\title{ Simulated clutch size data for birds with different nesting locations. }
\description{
  These made-up data do respect the average clutch sizes (number of eggs
  laid in a single brood) and incubation periods that were observed in
  different European bird species with four different types of nests,
  as reported in Case(2000).
}
\usage{
data(BirdsCS)
}
\format{
  A data frame with 40 observations on the following 3 variables:
  \describe{
    \item{\code{Nest}}{kind of nest, a factor with levels \code{hole},
      \code{roofed}, \code{niche}, and \code{open}}
    \item{\code{Inc.Per}}{average duration of the incubation period
      (days)}
    \item{\code{ClutchSize}}{the typical number of eggs in a nest}
  }
  }
\source{
  Case, T.J. An Illustrated Guide to Theoretical Ecology. Oxford
  University Press, New York.
  
  van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\examples{
data(BirdsCS)
library(graphics)
coplot(ClutchSize ~ Inc.Per | Nest, BirdsCS, panel=panel.smooth)
}
\keyword{datasets}

