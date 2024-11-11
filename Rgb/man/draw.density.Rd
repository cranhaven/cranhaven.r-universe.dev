\name{draw.density}
\alias{draw.density}
\title{
  Two-dimensional density plot of a track
}

\description{
  This function is similar to \code{\link{draw.points}}, but draws a 2D density plot of the points instead of the points themselves.
}

\usage{
  draw.density(slice, start, end, column = "value", cex.lab = 1, bty = "o",
    fg = "#000000", pal = grDevices::grey, border = NA, depth = 8, dpi = 7, bw.x = 0.005,
    bw.y = 0.2, precision = 1, skewing = 1.75, ...)
}

\arguments{
  \item{slice}{
    A \code{data.frame} holding the data to plot, with elements in rows and data in columns.
}
  \item{start}{
    Single integer value, the left boundary of the window, in base pairs.
}
  \item{end}{
    Single integer value, the right boundary of the window, in base pairs.
}
  \item{column}{
    Single character value, the name of the \code{slice} column to use for bar heights.
}
  \item{cex.lab}{
    See \code{par}.
}
  \item{bty}{
    See \code{par}.
}
  \item{fg}{
    Single character value, defining the color of the foreground (axes, labels...) as an english name or a hexadecimal code. Similar to \code{par}'s argument but not relying on it.
}
  \item{pal}{
    A function returning a set of colors when provided a set of intensities between 0 and 1 (typically \code{\link[grDevices]{grey}}), for the various levels of density.
}
  \item{border}{
    The color to use for polygon borders (as a name, an integer or an hexadecimal character description). Special value \code{NA} can also be used to disable borders.
}
  \item{depth}{
    Single integer value, the amount of different ranges to simplify the density into before plotting (corresponds to the \code{nlevels} argument of the \code{\link[graphics]{filled.contour}} function).
}
  \item{dpi}{
    Single numeric value, the Dots Per Inches resolution of the grid on which \code{\link[MASS]{kde2d}} will compute the density. The final grid size thus also depends on the figure dimensions, as provided in inches by \code{par("din")}.
}
  \item{bw.x}{
    Single numeric value, the bandwidth to use for density estimation on the X (genomic) axis. Notice this value will be multiplied by the genomic width of the plotted window (in Mbp), to enforce a similar resolution at all zoom levels.
}
  \item{bw.y}{
    Single numeric value, the bandiwdth to use for density estimation on the Y axis. It was typically chosen for a Y axis ranging from -1 to 1, wider axes could require wider bandwidths.
}
  \item{precision}{
    Single numeric value, providing a simpler way to control the sharpness of the density plot than setting \code{depth}, \code{dpi}, \code{bw.x} and \code{bw.y} individually. The default \code{precision} of 1 has no effect on these 4 other arguments, values lower than 1 produce more averaged plots, values higher than 1 produce plots on which individual points are easier to distinguish from each others. 0 corresponds to an unreachable state of "null precision", this argument should thus be defined as a strictly positive number.
}
  \item{skewing}{
    Single numeric value, defining how the color scale should be skewed toward small values. Higher \code{skewing} values makes zones of low density (typically isolated points) darker and decreases the overall contrast.
}
  \item{\dots}{
    Further arguments to be passed to \code{\link{draw.bg}}.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{draw.bg}}, \code{\link{draw.boxes}}, \code{\link{draw.hist}}, \code{\link{draw.pileup}}, \code{\link{draw.points}}, \code{\link{draw.seq}}, \code{\link{draw.steps}}
}

