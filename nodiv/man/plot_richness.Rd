\name{plot_richness}
\alias{plot_richness}
\alias{plot_node}

\title{
Plot a richness map for an object of type distrib_data, nodiv_data or nodiv_result
}
\description{
Create a raster or point plot showing the species richness of an object. Useful for comparing the species richness of sister clades
}
\usage{
plot_richness(distrib_data, ...)
plot_node(nodiv_data, node = basal_node(nodiv_data), sites = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distrib_data}{
An object of types \code{distrib_data}, \code{nodiv_data} or \code{nodiv_result}
}
  \item{nodiv_data}{
An object of types \code{nodiv_data} or \code{nodiv_result}
}
  \item{node}{
The name or internal number of the node to be plotted
}
  \item{sites}{
If \code{sites = "all"} the plotting region retains all sites (including sites unoccupied by that node)
}
  \item{\dots}{
Further arguments to pass to the plotting function
}
}
\details{
The plot will be done as a spatial grid or as colored points, depending on the \code{type} of the \code{nodiv_result} object. \code{plot_richness} plots the richness of the entire dataset, whereas \code{plot_node} plots the species richness of a given node.
}
\value{
If \code{distrib_data} has \code{type = raster}, a raster object of the SOS values is returned. 
}

\author{
Michael Krabbe Borregaard
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

