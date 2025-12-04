\name{Temburong}
\alias{Temburong}
\alias{TemburongBA}
\docType{data}
\title{
Tree species count data
}
\description{
Basal area and number of individual trees >= 5cm dbh of each species in a 1ha plot in Ulu Temburong - Brunei. There were 1012 trees of 276 species.
}
\usage{data(Temburong)}

\format{
The data set consists of two objects:

  \code{Temburong} is a vector of length 276, with the counts of each species.

  \code{TemburongBA} is a similar vector with the basal area (ie. the sum of the cross-sectional area at breast height of all of trees) of each species.
}
\source{
Small, A; T G Martin; R L Kitching; K M Wong. 2004. Contribution of tree species to the biodiversity of a 1 ha Old World rainforest in Brunei, Borneo. \emph{Biodiversity and Conservation} 13:2067-2088.
}
\examples{
data(Temburong)
richChao1(Temburong)
biodShannon(TemburongBA)
}
\keyword{datasets}
