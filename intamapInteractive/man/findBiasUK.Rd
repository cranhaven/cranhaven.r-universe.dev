\name{findBiasUK}
\alias{findBiasUK}
\title{ Finding the regional biases using GLM
}
\description{ Method for identifying regional biases (in most cases biases between countries)}
\usage{findBiasUK(object)}
\arguments{
\item{object}{an object of class \code{\link[sp:SpatialPoints]{SpatialPointsDataFrame}}, at least 
              containing observations and a regional identification code (\code{regCode})}  
} 
\value{ 
A \code{\link[base:data.frame]{data.frame}} with the biases for each country with uncertainty.
}
\author{ Olivier Baume }
\seealso{\code{\link{findRegionalBias}}}
\keyword{spatial}
