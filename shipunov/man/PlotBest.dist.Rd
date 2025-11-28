\name{PlotBest.dist}
\alias{PlotBest.dist}
\title{Dotchart which reflects the "best" base distance method}

\description{Plots dotchart with shows correspondences between data and various base distances}

\usage{

PlotBest.dist(data, distances=c("euclidean", "maximum", "manhattan",
"canberra", "binary", "minkowski"))

}

\arguments{
\item{data}{Data frame with values}
\item{distances}{Distances to use}
}

\details{

Shows the "best" distance method. Please note that this is a mere
visualization, and numbers are used only to understand the relative
correspondence between raw data and distances.

Uses maximal correlations between multidimensional scaling of distance
object (converted internally to Euclidean) and PCA of data. Both MDS and
PCA use two dimensions.

}

% \value{}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{

PlotBest.dist(iris[, -5])

PlotBest.dist(t(moldino))

}

\keyword{hplot}
