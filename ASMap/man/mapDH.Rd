\name{mapDH}
\alias{mapDH}
\docType{data}
\title{A constructed linkage map for a doubled haploid wheat population}
\description{A constructed linkage map for a doubled
  haploid wheat population in the form of a constructed \pkg{qtl} object.
}
\usage{data(mapDH)}
\format{This data relates to a fully constructed linkage map of 599
  markers genotyped on 218 individuals. The linkage map consists of 23
  linkage groups spanning the whole genome. 584 markers are from the
  orignal map with an additonal 12 co-located markers and 3 slightly
  distorted markers. The map was constructed using the MSTmap algorithm
  integrated in\code{\link{mstmap.cross}} with geentic distances estimated
  using the \code{"kosambi"} mapping function. The data is in \pkg{qtl}
  format with a class structure \code{c("bc","cross")}. See
    \code{read.cross} documentation for more details on the
  format of this object.
}
\examples{

data(mapDH, package = "ASMap")

}
\keyword{datasets}