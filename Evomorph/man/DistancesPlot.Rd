\name{DistancesPlot}
\alias{DistancesPlot}
\title{
Plot the Procrustes distances between a set of shapes and a reference
}
\description{
	\code{DistancesPlot} creates a distances plot using \pkg{ggplot2} (Wickman 2009).
}
\usage{
DistancesPlot(dist)
}
\arguments{
  \item{dist}{Distances matrix}
}
\details{
	Use \code{ggplot2} to create a plot where each point represent distance between a particular shape and a reference shape.
}
\value{
	
Return an \pkg{ggplot2} object
	
}
\references{H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2009.}
\author{Juan Manuel Cabrera}
\examples{

data("simulated_shape_distances")

#Plot distances between a set of simulated shapes a reference.

DistancesPlot(simulated_shape_distances)
}
