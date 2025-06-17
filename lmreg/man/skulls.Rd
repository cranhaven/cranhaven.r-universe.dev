\name{skulls}
\alias{skulls}
\docType{data}
\title{
Egyptian skull development
}
\description{
Measurements of male Egyptian skulls from time periods ranging from 4000 BC to 150 AD.
}
\usage{data(skulls)}
\format{
  A data frame with 150 observations on the following 5 variables.
  \describe{
    \item{\code{MB}}{Maximal breadth (in mm)}
    \item{\code{BH}}{Basibregmatic height (in mm)}
    \item{\code{BL}}{Basialveolar length (in mm)}
    \item{\code{NH}}{Nasal height (in mm)}
    \item{\code{Year}}{Approximate Year of Skull Formation (negative = B.C., positive = A.D.)}
  }
}

\source{
Thomson, A. and Randall-Maciver, R. (1905) Ancient Races of the Thebaid, Oxford University Press, Oxford.
}

\examples{
data(skulls)
head(skulls)
}
\keyword{datasets}
