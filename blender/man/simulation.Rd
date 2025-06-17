\name{simulation}
\alias{blender.reduce}
\alias{blender.shuffle}

\title{
  Simulate reduced richness landscapes
}
\description{
  \code{reduce} randomly selects species for inclusion in a reduced-richness
  landscape based on the original.  \code{shuffle} randomizes species locations
  among sites.
}
\usage{
  blender.reduce(landscape, richness)
  blender.shuffle(landscape)
}

\arguments{
  \item{landscape}{
     A binary presence-absence \code{data.frame} for species (rows) 
     at sites(columns)  
}
  \item{richness}{
     An integer number of species to include in a reduced landscape
}
}
\details{
   Species are sampled by \code{reduce} without replacement.  Species
   are randomized by \code{shuffle} in order to preserve their occupancy
   rates (row sums) but not local richness (column sums).
}
\value{
   A \code{data.frame} based on \code{landscape}
}
\references{
  These functions were used in the simulations in 
  Harris, D. J., K. G. Smith, and P. J. Hanly. 2011.
  "Occupancy is nine-tenths of the law: 
  Occupancy rates determine the homogenizing and differentiating effects
  of exotic species." The American Naturalist.
}
\author{
  David J. Harris \code{<DavHarris@UCDavis.edu>}
}
