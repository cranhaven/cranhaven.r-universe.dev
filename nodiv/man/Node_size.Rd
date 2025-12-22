\name{Node_size}
\alias{Node_size}
\alias{Node_sites}
\alias{Node_species}
\alias{Node_occupancy}

\title{
Attributes of individual clades
}
\description{
Functions that summarize the species that descend from a node, and their distribution.
}
\usage{
Node_size(nodiv_data, node = NULL)
Node_sites(nodiv_data, node, names = TRUE)
Node_species(nodiv_data, node, names = TRUE)
Node_occupancy(nodiv_data, node = NULL)
}

\arguments{
  \item{node}{
A character or numeric variable specifying one or more nodes in the phylogeny
}
  \item{nodiv_data}{
An object of type \code{nodiv_data} or \code{nodiv_result}. \code{Node_species} also accepts objects of type \code{phylo}
}
  \item{names}{
If \code{TRUE}, return a character vector of species names; otherwise return the species index numbers in the community matrix 
}
}
\details{
If no \code{node} is specified in \code{Node_size} or \code{Node_occupancy}, the function will calculate the value for all nodes in the phylogeny
}
\value{
For \code{Node_size} and \code{Node_occupancy} the number of species or number of occupied sites of the nodes; or a vector giving this value for all nodes. For \code{Node_species} or \code{Node_sites} a character vector of names of species descending from a node, or the sites where they occur.
}

\author{
Michael Krabbe Borregaard
}

