\name{Nspecies}
\alias{Nspecies}
\alias{Nsites}
\alias{richness}
\alias{occupancy}
\alias{sites}
\alias{species}
\alias{coords}

\title{
Basic attributes of \code{distrib_data} objects
}
\description{
Calculates the species richness and site occupancy of a \code{distrib_data} object
}
\usage{
Nspecies(distrib_data)
Nsites(distrib_data)
richness(distrib_data, sites = NULL)
occupancy(distrib_data, species = NULL)
sites(distrib_data)
species(distrib_data)
coords(distrib_data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{distrib_data}{
An object of type \code{distrib_data}, \code{nodiv_data} or \code{nodiv_result}
}
  \item{sites}{
The site for which to return the richness
}
  \item{species}{
The species for which to return the occupancy
}
}

\value{
\code{Nspecies} gives the number of species in the dataset, and \code{Nsites} the number of sites. 
\code{richness} gives a numeric vector with the species richness of all sites.
\code{occupancy} gives the number of sites occupied by all species.
\code{sites} gives a list of site names in the object. 
\code{species} gives a list of species names in the object. 
\code{nodes} gives a node labels or node numbers 
\code{coords} returns the spatial coordinates of points or grid cell centers 
}

\author{
Michael Krabbe Borregaard
}

