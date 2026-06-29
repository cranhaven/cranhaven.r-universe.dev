\name{mapBC}
\alias{mapBC}
\docType{data}
\title{A constructed linkage map for a backcross barley population}
\description{A constructed linkage map for a backcross
  barley population in the form of a constructed \pkg{qtl} object.
}
\usage{data(mapBC)}
\format{This data relates to a fully constructed linkage map of 3019
  markers genotyped on 300 individuals spanning the 7 linkage groups of
  the barley genome. The map was constructed using the MSTmap algorithm
  integrated in \code{\link{mstmap.cross}} with geentic distances estimated
  using the \code{"kosambi"} mapping function. The data is in \pkg{qtl}
  format with a class structure \code{c("bc","cross")}. See
    \code{read.cross()} documentation for more details on the
  format of this object.
}
\references{
Taylor, J., Butler, D. (2017) R Package ASMap: Efficient Genetic
Linkage Map Construction and Diagnosis. Journal of Statistical Software,
\bold{79}(6), 1--29.
}
\examples{

data(mapBC, package = "ASMap")

}
\keyword{datasets}