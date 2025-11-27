\name{FCMapper-package}
\alias{FCMapper-package}
\alias{FCMapper}
\docType{package}
\title{
Fuzzy Cognitive Mapping
}
\description{
This package provides several functions to create and manipulate fuzzy cognitive maps.It is based on FCMapper for Excel, distributed at http://www.fcmappers.net/joomla/, developed by Michael Bachhofer and Martin Wildenberg.

Maps are inputted as adjacency matrices. Attributes of the maps and the equilibrium values of the concepts (including with user-defined contrained values) can be calculated. The maps can be graphed with a function that calls "igraph". Multiple maps with shared concepts can be aggregated.

}
\details{
\tabular{ll}{
Package: \tab FCMapper\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-12-16\cr
License: \tab GPL-2\cr
}
Functions:

-\code{\link{changes.scenario}}: Finds equilibrium values of concepts with no fixes values.

-\code{\link{check.matrix}}: Checks that fuzzy cognitive map is square and values are between -1 and 1.

-\code{\link{combine.maps}}: Aggregates multiple fuzzy cognitive maps into one.

-\code{\link{comp.scenarios}}: Compares the equilibrium values of two scenarios.

-\code{\link{comp.maps}}: Calculates similarity indices (S2 and Jaccard) of two coginitive maps. 

-\code{\link{concept.indices}}: Calculated concept-level indices.

-\code{\link{graph.fcm}}: Plots fuzzy cognitive map by calling igraph package

-\code{\link{matrix.indices}}: Calculates matrix-level indices.

-\code{\link{nochanges.scenario}}: Finds equilibrium values of concepts with user-defined fixed values.



}
\author{
Shaun Turney and Michael Bachhofer

Maintainer: Shaun Turney <shaun.turney@mail.mcgill.ca>
}
\references{

\url{http://www.fcmappers.net/}

Kosko, B. (1986). Fuzzy cognitive maps. International journal of man-machine studies, 24(1), 65-75.

Glykas, M. (2010). Fuzzy Cognitive Maps. Advances in Theory, Methodologies, Tools and Applications, Series: Studies in Fuzziness and Soft Computing.

Ozesmi, U., & Ozesmi, S. L. (2004). Ecological models based on people's knowledge: a multi-step fuzzy cognitive mapping approach. Ecological Modelling, 176(1), 43-64.

}

\keyword{ package }

