\name{blender.basics}
\alias{blender.basics}
\alias{jstar}
\alias{jbar}
\alias{pstar}

\title{
  Basic landscape calculations
}
\description{
  \code{jbar} calculates average Jaccard similarity among sites (columns)
  in your landscape as the expected ratio of the intersection between two
  sites to to their union:
  
  \code{J.Bar = mean(intersection/union)}
  
  \code{jstar} gives an approximation of this value from species
  occupancy rates (row sums) as the ratio of the expected intersection
  between two randomly chosen sites to the expected union:
  
  \code{J.Star = mean(intersection)/mean(union)}
  
  \code{pstar} gives the "effective occupancy" of a landscape, defined in
  Harris et al. (2011). A landscape composed entirely of species with this
  occupancy rate would have the same J.Star value as the input landscape.
   
}
\usage{
  jbar(x)
  jstar(x, n = NULL)
  pstar(x, n = NULL)
}

\arguments{
  \item{x}{
	For \code{jbar}, a binary \code{data.frame} with species as rows and
	sites as columns.  For \code{jstar} and \code{pstar}, either a
	\code{data.frame} or a  \code{numeric} vector containing the proportion
	of sites occupied by each species.
}
  \item{n}{
    The number of sites in your landscape.  Only needed for \code{jstar}
    and \code{pstar} if \code{x} is \code{numeric}.
}
}
\references{
  Harris, D. J., K. G. Smith, and P. J. Hanly. 2011.
  "Occupancy is nine-tenths of the law: 
  Occupancy rates determine the homogenizing and differentiating effects
  of exotic species." The American Naturalist.

}
\author{
  David Jay Harris \code{<DavHarris@UCDavis.edu>}
}


\seealso{
  \link{blend}
}
\examples{
  data(PLANTS)
  
  # Calculate key values for Wyoming from raw data
  landscape = PLANTS[["WY native table"]]
  
  jbar(landscape)
  jstar(landscape)
  pstar(landscape)
  
  
  # jstar and pstar also work if given row means and landscape sizes.
  # jbar requires spatial information that is lost during this averaging.
  occupancy = rowMeans(landscape)
  nsites = ncol(landscape)
  
  jstar(occupancy, nsites)
  pstar(occupancy, nsites)
}