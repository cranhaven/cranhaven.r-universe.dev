\name{mapF2}
\alias{mapF2}
\docType{data}
\title{Simulated constructed linkage map for a self pollinated F2 barley population}
\description{Simulated constructed linkage map for a self pollinated F2
  barley population in the form of an \pkg{qtl} object.
}
\usage{data(mapF2)}
\format{This data relates to a fully constructed linkage map of 700
  simulated markers genotyped on 250
  individuals. The map consists of 7 linkage groups, each contaning 100
  markers spanning an approximate linkage group length of 200cM. The map was
  constructed using \code{\link{mstmap.cross}} from the \pkg{ASMap} package and
  map distances were estimated using the \code{"kosambi"}
  mapping function. The data is in R/qtl format with a class structure
  \code{c("bcsft","cross")}.
}
\examples{

data(mapF2, package = "ASMap")

}
\keyword{datasets}