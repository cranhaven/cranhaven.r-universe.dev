\name{krige_surf}
\alias{krige_surf}
\alias{krige_new_data}
\alias{kriged_surfaces}

\title{
Interpolate functional characteristics over a grid
}
\description{
\code{krige_surf()} performs kriging (i.e., interpolation) of one or more functional characteristics that are spatially distributed over a morphospace to create a smoothly kriged surface. Interpolated values can also be produced for a new dataset given their coordinates in morphological space.
}
\usage{
krige_surf(fnc_df, grid = NULL, resample = 100, padding = 1.2, hull = NULL, 
  alpha = 1, new_data = NULL)
           
krige_new_data(x, new_data)
}
\arguments{
  \item{fnc_df}{
a \code{fnc_df} object; the output of a call to \code{\link{as_fnc_df}}, which contains coordinates in morphological space and values of functional characteristics for the warps used to create the kriged surface.
}
  \item{grid}{
a matrix or data frame containing the grid of points over which the surface is to be interpolated. Should be the output of a call to \code{\link{resample_grid}}. If \code{NULL}, the grid will be formed by calling \code{resample_grid()} on the inputs.
}
  \item{resample}{
the number of points (or pixels) in the x and y dimensions over which to create the grid. Default is 100 for a kriged surface of 100x100=10,000 pixels. Passed to \code{\link{resample_grid}}. Ignored when \code{grid} is not \code{NULL}.
}
  \item{padding}{
a number representing how much to expand the grid beyond the ranges of the x- and y-coordinates. For example, \code{padding = 1.2} (the default) expands the grid by 20\% of the coordinates' ranges in each direction. Must be a number greater than or equal to 1. Large numbers imply greater extrapolation, and whatever padding is added will be negated if \code{hull = TRUE}. Passed to \code{\link{resample_grid}}. Ignored when \code{grid} is not \code{NULL}.
}
  \item{hull}{
method to to restrict kriging to an alpha hull to prevent extrapolation beyond the coordinates present in \code{fnc_df}. Passed to \code{\link{resample_grid}}, which uses \code{alphahull} or \code{concaveman} packages. Default is \code{\link[alphahull:ahull]{alphahull::ahull}}. If no hull is desired set \code{hull = NULL}. If \code{NULL}, kriging will take place over a rectangular grid that spans the boundaries of the coordinates in \code{fnc_df}. Ignored when \code{grid} is not \code{NULL}
}

  \item{alpha}{
the alpha value used to create the alpha hull. Passed to \code{\link{resample_grid}} and eventually to \code{\link[alphahull:ahull]{alphahull::ahull}}. Ignored when \code{grid} is not \code{NULL}.
}
  \item{new_data}{
a dataset of coordinates for a new sample; the values of the functional characteristics for this sample will be interpolated using the kriged surface.
}
  \item{x}{
a \code{"kriged_surfaces"} object; the output of a call to \code{krige_surf()}.
}
}
\details{
\code{krige_surf()} implements the \code{\link[automap:autoKrige]{automap::autoKrige}} function on one or more spatially distributed traits to produce an interpolated (or extrapolated) surface of evenly spaced gridpoints. This is done by automatiically finding the best variogram fit for each of the non-corrdinate variables in \code{fnc_df}. For details on automatic variogram fitting, see \code{\link[automap:autoKrige]{automap::autoKrige}}.

Input data in \code{fnc_df} can be unevenly distributed (direct from speciments), or gridded (determined from evenly distributed hypothetical shapes) in morphospace. Trait data inputted directly from specimen measurements will be subject to error based on the how unevenly points are distributed, with high resoultion gridded datapoints producing the least potential reconstruction error (see Smith et al 2021).

By default \code{krige_surf} will create a \code{hull} to strictly prevent any extrapolation beyond the provided data. This will produce the most conservative landscapes. If \code{hull} is set to \code{FALSE}, then the function will reconstruct a rectangle determined by the XY coordinate ranges supplied in \code{fnc_df}. Padding will be applied by default (an extra 20\%) as defined by \code{padding}, to expand the rectangle beyond the supplied points. Reconstructions without a \code{hull} would be most appropriate for trait data determined from evenely spaced hypothetical gridpoints. If \code{grid} is provided the function will strictly interpolate at these gridded points.  


\code{krige_new_data()} adds a new data set to the supplied \code{kriged_surfaces} object and interpolates the values of the functional characteristics on the suppllied sample. This should only be used to add a new dataset to a \code{kriged_surfaces} object produced without \code{new_data} supplied or to replace an existing \code{new_data} component.
}
\value{
An object of class \code{kriged_surfaces} containing the following components:
\item{fnc_df}{the original data frame of functional characteristics passed to \code{fnc_df}.}
\item{autoKrige}{a named list of the kriged surfaces, one for each functional characteristic. Each surface is of class \code{autoKrige}, the output of the call to \code{\link[automap:autoKrige]{automap::autoKrige}}.}
\item{dataframes}{a list of two data frames, \code{grid} and \code{new_data}. \code{grid} contains the coordinates of the kriged surface grid (in the \code{x} and \code{y} columns) as well as the interpolated values of the functional characteristics. \code{new_data} contains the sample coordinates supplied to \code{new_data} as well as the interpolated values of the functional characteristics for each sample. This second component is absent if \code{new_data = NULL} in the call to \code{krige_surf()}.}

For \code{krige_new_data()}, the original \code{kriged_surfaces} object, with the \code{$dataframes$new_data} component containing the sample coordinates supplied to \code{new_data} as well as the interpolated values of the functional characteristics for each sample.
}

\references{
Smith, S. M., Stayton, C. T., & Angielczyk, K. D. (2021). How many trees to see the forest? Assessing the effects of morphospace coverage and sample size in performance surface analysis. Methods in Ecology and Evolution, 12(8), 1411-1424.
}

\seealso{
\code{\link{plot.kriged_surfaces}} for plotting the kriged surfaces.

\code{\link{as_fnc_df}} for creating the input dataset.

\code{\link{resample_grid}} for creating the grid over which the kriging occurs prior to using \code{krige_surf}.

\code{\link[automap:autoKrige]{automap::autoKrige}} for the function that does the kriging.
}
\examples{
data("warps")

warps_fnc <- as_fnc_df(warps)

grid <- resample_grid(warps, hull = "concaveman", plot = TRUE)

kr_surf <- krige_surf(warps_fnc, grid = grid)
kr_surf

# Add new data
data("turtles")
kr_surf <- krige_new_data(kr_surf, new_data = turtles)
kr_surf
plot(kr_surf)

# Doing it all in one step:
\dontrun{
kr_surf <- krige_surf(warps_fnc, new_data = turtles, hull = "alphahull")
kr_surf
}

# No hull and padding
kr_surf <- krige_surf(warps_fnc, new_data = turtles, hull = NULL, padding = 1.2)
plot(kr_surf)


}