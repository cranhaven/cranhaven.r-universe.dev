\name{plot_points}
\alias{plot_points}
\alias{plot_grid}
\alias{plot_species}

\title{
Plot a variable on spatial points or a spatial grid using a color scale.
}
\description{
Plots a variable either as colored points or as a colored raster. \code{plot_sitestat} is a wrapper function that dispatches the other functions according to the \code{type} of \code{distrib_data}.
}
\usage{
plot_points(x, coords, col, shape = NULL, 
  shapefill = "grey",  zlim= NULL,  zoom_to_points = FALSE, 
  pch = 16, bg = par("bg"), legend = TRUE, ...)
plot_grid(x, coords, col, shape = NULL, 
  shapefill = "grey",  shapeborder = NA, zlim = NULL, 
  zoom_to_points = FALSE, legend = TRUE, gridcol, gridlwd, 
  gridsites, overlay_shape = FALSE, 
  colscale = c("equal_interval","quantiles"), legendlabels = NULL, ...)
plot_species(distrib_data, species, col = c("darkgreen", "red"), ...)
}

\arguments{
  \item{x}{
A numerical variable to be plotted using a color scale
}
  \item{coords}{
A matrix or \code{data.frame} with two columns, or an object inheriting from \code{SpatialPoints}, of the same length as \code{x}
}
  \item{col}{
The color palette to use for the color scale. Can be given as color names, a vector of hex colors from a call to a color palette (e.g., \code{terrain.colors(64)}), 
or as the name of one of nodiv's internal palettes. Possibillities are \code{"parula", "jet", "blackbody", "HMblueyellow", "HMrainbow", 
"HMlinear_optimal", "HMoptimal_scale", "cube1", "cubeyf1"}. Alternatively \code{col} can be one of \code{"auto", "ramp", "monochrome", "divergent" or "individual"}, allowing \code{nodiv} to choose a palette of the given type
}
  \item{shape}{
A shape file giving a map to use as a background for the plot
}
  \item{shapefill}{
If shape is a polygon, the color to use to fill the polygons. If shape is a line shapefile, the color to use for plotting lines
}
  \item{shapeborder}{
If shape is a polygon, the color to use for plotting the line border
}
  \item{zlim}{
The values to use for the extremes of the color scale
}
  \item{zoom_to_points}{
Given a shapefile, the function will set the outer limits of the plot to the bounding box of the shapefile. If \code{zoom_to_points} is \code{TRUE}, the outer limits will be set by the bounding box of the data points instead.
}
  \item{pch}{
The point character to use for plotting points
}
  \item{bg}{
The central color of points when using a pch value between 21 and 25
}
  \item{distrib_data}{
An object of type \code{distrib_data}, \code{nodiv_data} or \code{nodiv_result} to use for plotting x.
}
  \item{legend}{
Whether to plot a color legend
}
  \item{species}{
The number or name of the species to be plotted
}
  \item{gridcol}{
If specified, plots the sampling grid cell borders in this color
}
  \item{gridlwd}{
If specified, set the line width of overlaid grid cell borders
}
  \item{gridsites}{
If specified, which grid cells to draw cell borders around
}
  \item{overlay_shape}{
Whether to overlay the shape file border on top of the points - only works if shape is a vector shapefile
}
  \item{colscale}{
Specifies whether to define the colors on an equal interval or a quantile-based color scale
}
  \item{legendlabels}{
Specifies custom labels for the colorbar legend
}
  \item{\dots}{
Additional arguments to pass to plot
}
}
\details{
Note that if \code{shape} is a \code{raster} object, the points may shift relative to the background when resizing the image. This seems to be due to a bug in the \code{raster} package. 
}

\author{
Michael Krabbe Borregaard 
}

