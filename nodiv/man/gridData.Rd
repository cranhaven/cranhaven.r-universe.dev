\name{gridData}
\alias{gridData}

\title{
Apply a grid (2D bins) to nodiv objects of type 'points'
}
\description{
Takes an object of type \code{distrib_data} or \code{nodiv_data}, and puts the point distributions on a grid. Can also be used for resampling grid data at a coarser scale. If the comm matrix has abundances (any one value larger than 1), they are summed across points in the grid cell.
}
\usage{
gridData(dist_data, cellsize_x = 1, cellsize_y = cellsize_x, xll_corner, yll_corner)
}

\arguments{
  \item{dist_data}{
An object of type \code{distrib_data} or \code{nodiv_data} 
}
  \item{cellsize_x}{
The size of the bins along the X (longitudinal) axis, in units of the spatial coordinates of the original points
}
  \item{cellsize_y}{
The size of the bins along the Y (latitudinal) axis, in units of the spatial coordinates of the original points
}
  \item{xll_corner}{
The x coordinate of the lower left corner of the lower left grid cell of the intended grid. The corner must be left of all points in the data set
}
  \item{yll_corner}{
The y coordinate of the lower left corner of the lower left grid cell of the intended grid. The corner must be below all points in the data set
}
}

\value{
The return value is the original \code{dist_data} object with gridded distribution data
}

\author{
Michael Krabbe Borregaard
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{distrib_data}}, ~~~
}
\examples{
## Not run: 
data(coquettes)
new <- gridData(coquettes, 2)
plot_richness(new)
## End(Not run)
}

