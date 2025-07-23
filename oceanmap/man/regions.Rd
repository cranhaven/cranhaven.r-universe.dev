\name{regions}
\alias{regions}
\title{
Returns two-row summary table of a specified region.}
\description{
Reorganizes summary information of a specified region from the \link{region_definitions}set into a two-row dataframe.
Region definitions can be added, backed up or restored by \link{add.region} or deleted by calling \link{delete.region}.

\bold{ATTENTION!} When reinstalling or updating the oceanmap package, previous region definitions are getting lost! It is therefore highly recommanded to take and restore own backups (see: \code{backup} and \code{restore}).
}

\usage{
regions(label)
}
\arguments{
  \item{label}{
Character string indicating the name of the region of interest. If missing, list of available regions in the \link{region_definitions}-dataset will be returned by a error message. 
}
}

\value{
a two-row dataframe with the following header, containing the summary information of the region specified:

\code{xlim} \code{ylim} \code{dim} \code{name} \code{cbx} \code{cby} \code{align} \code{gradient} \code{figdim} \code{grid.res}

\item{\code{xlim} & \code{ylim}}{
the spatial extent of the region
}

\item{\code{dim}}{
the number of grid points for both x & y-dimension
}

\item{\code{name}}{
the long name of the region
}
\item{\code{cbx} & \code{cby}}{
x & y-coordinates for colorbar
}
\item{\code{align}}{a vector defining the color-\code{gradient} of the colorbar (\code{x} for horizontal, and \code{y} for vertical), as well as the \code{margin} where the colorbar ticks should be plotted, relative to the colorbar rectangle (\code{'l'} left, \code{'r'} right and \code{'b'} for bottom)
}

\item{\code{figdim}}{
the region-specific default plot device size
}

\item{\code{grid.res}}{
the default grid resolution in degrees
}
}

\seealso{ \code{ \link{v}, \link{plotmap}} } 


\author{
Robert K. Bauer
}

\examples{
## Example: return summary table for the Gulf of Lions
data('region_definitions')
region_definitions[region_definitions$label=='lion',] # select raw region data summary 
regions('lion') # return formatted summary table
}
