\name{coquettes}
\alias{coquettes}
\docType{data}
\title{
Distribution of coquette hummingbirds in Northern South America
}
\description{
Phylogeny and distributions for the coquette clade of hummingbirds in Ecuador, Peru and Colombia
}
\usage{data(coquettes)}
\format{
a \code{nodiv_data} object with the phylogeny and distributions of all species
}
\source{
Graham, C.H., Parra, J.L., Tinoco, B.A., Stiles, F.G. & McGuire, J.A. (2012). Untangling the influence of ecological and evolutionary factors on trait variation across hummingbird assemblages. Ecology, 93, S99-S111.
}
\examples{
data(coquettes)
plot(coquettes, shapefill = terrain.colors(64), pch = 21)
}
\keyword{datasets}