\name{plot.wtd_lscp}
\alias{plot.wtd_lscp}
\alias{plot.grp_Wprime}
\alias{plot.by_Wprime}

\title{
Plot Adaptive Landscapes
}
\description{
These \code{plot} plot methods plot an adaptive landscape, a weighted combination of functional surfaces. These landscape arise from calls to \code{\link{calc_lscp}}, \code{\link{calc_all_lscps}}, \code{\link{calcGrpWprime}}, and \code{\link{calcWprimeBy}}.
}
\usage{
\method{plot}{wtd_lscp}(x, alpha = 1, pt.col = "black", 
     interpolate = TRUE, contour = TRUE, ...)
\method{plot}{grp_Wprime}(x, alpha = 1, pt.col = "black", 
     interpolate = TRUE, contour = TRUE, ...)
\method{plot}{by_Wprime}(x, level, ncol = 1, alpha = 1,
     pt.col = "black", interpolate = TRUE, contour = TRUE, 
     ...)
}

\arguments{
  \item{x}{
a \code{wtd_lscp}, \code{grp_Wprime}, or \code{by_Wprime} object, the output of a call to \code{\link{calc_lscp}}, \code{\link{calcGrpWprime}}, or \code{\link{calcWprimeBy}}, respectively.
}
  \item{alpha}{
the transparency of the points for the data sample. A number between 0 (fully transparent) and 1 (fully opaque). Passed to \code{\link[ggplot2:geom_point]{ggplot2::geom_point}}.
}
  \item{pt.col}{
the color of the points for the data sample. Passed to \code{\link[ggplot2:geom_point]{ggplot2::geom_point}}.
}
  \item{interpolate}{
whether to interpolate across pixels in the grid. Passed to \code{\link[ggplot2:geom_raster]{ggplot2::geom_raster}}.
}
  \item{contour}{
whether to display contours in the grid.
}
  \item{level}{
which level of the \code{by} (subgrouping) variable to be plotted. If missing, all will be plotted.
}
  \item{ncol}{
when multiple subgroups are plotted, in how many columns should the plots be arranged.
}
  \item{\dots}{
ignored.
}
}
\details{
These plotting functions are wrappers for \code{ggplot2} raster plotting functions. For more precise control of raster plotting see \code{\link[ggplot2:geom_raster]{ggplot2::geom_raster}}.
}
\value{
A \code{ggplot} object that can be further adjusted using functions from \pkg{ggplot2}.
}
\seealso{
\code{\link{calc_lscp}}, \code{\link{calc_all_lscps}}, \code{\link{calcGrpWprime}}, and \code{\link{calcWprimeBy}} for the functions used to create the objects that are plotted

\code{\link{plot.kriged_surfaces}} for plotting functional surfaces prior to combining them into an adaptive landscape.

\code{\link[ggplot2:geom_raster]{ggplot2::geom_raster}}, \code{\link[ggplot2:geom_point]{ggplot2::geom_point}}, and \code{\link[ggplot2:geom_contour]{ggplot2::geom_contour}} for the underlying plotting functions.
}
\examples{
data("warps")
data("turtles")

warps_fnc <- as_fnc_df(warps, func.names = c("hydro", "fea"))

kr_surf <- krige_surf(warps_fnc, new_data = turtles)

weights <- c(hydro = .5, fea = .5)

w_lscp <- calc_lscp(kr_surf, weights = weights)

plot(w_lscp)
plot(w_lscp, countour = FALSE, pt.col = "white")

# See help("calc_lscp"), help("calcGrpWprime"), and 
# help("calcWprimeBy") for examples when used with
# those functions
}
