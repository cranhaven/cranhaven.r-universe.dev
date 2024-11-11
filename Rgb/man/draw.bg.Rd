\name{draw.bg}
\alias{draw.bg}
\title{
  Background for track plots
}

\description{
  This function draws the background for the other track ploting functions.
}

\usage{
  draw.bg(start, end, ylab = "", ylab.horiz = FALSE, ysub = as.character(NA),
    mar = c(0.2, 5, 0.2, 1), xaxt = "s", yaxt = "n", yaxs = "r", ylim = c(0, 1),
    cex.lab = 1, cex.axis = 1, mgp = c(3, 1, 0), tck = NA, tcl = -0.5,
    xaxp = as.numeric(NA), yaxp = as.numeric(NA), bty = "o", las = 0, xgrid = TRUE,
    new = FALSE, bg = NA, bg.inner = NA, fg = "#000000", ...)
}

\arguments{
  \item{start}{
    Single integer value, the left boundary of the window, in base pairs.
}
  \item{end}{
    Single integer value, the right boundary of the window, in base pairs.
}
  \item{ylab}{
    The name of the Y axis. See \code{par}.
}
  \item{ylab.horiz}{
    Single logical value, whether to print \code{ylab} horizontally or not.
}
  \item{ysub}{
    Similar to \code{ylab}, but written on a closer line when \code{yaxt} is "n". Can be \code{NA} to disable it.
}
  \item{mar}{
    A numerical vector of the form "c(bottom, left, top, right)" which gives the number of lines of margin to be specified on the four sides of the plot. See \code{par}.
}
  \item{xaxt}{
    Whether to plot an X axis ("s") or not ("n"). See \code{par}.
}
  \item{yaxt}{
    Whether to plot an Y axis ("s") or not ("n"). If no Y axis is drawn, \code{ysub} may be used to plot a sub-title to the Y axis. See \code{par}.
}
  \item{yaxs}{
    Y axis style, "r" enlarges the Y limits by 4 percents on each side for a cleaner look, "i" will not. See \code{par}.
}
  \item{ylim}{
    The Y axis limits as a numerical vector of the form "c(start, end)" of the plot. Note that start > end is allowed and leads to a "reversed axis". Use "NULL" to guess the axis range from the data. See \code{plot.default}.
}
  \item{cex.lab}{
    The relative character size of x and y axis labels (default: 1). See \code{par}.
}
  \item{cex.axis}{
    The relative character size of x and y axis annotations (default: 1). See \code{par}.
}
  \item{mgp}{
    Length 3 vector defining the distance between the plot area and respectively the Y axis label, Y axis annotations and Y axis line (default: 3, 1, 0). See \code{par}.
}
  \item{tck}{
    The length of tick marks as a fraction of the smaller of the width or height of the plot (default: NA, meaning using \code{tcl} instead). See \code{par}.
}
  \item{tcl}{
    The absolute length of a tick marks. Note that positive numbers put them inside the plot area (default: -0.5). See \code{par}.
}
  \item{xaxp}{
    Length 3 vector defining the ticks on the X axis : X of first tick, X of last tick and number of intervals between them (default: NA). See \code{par}.
}
  \item{yaxp}{
    Length 3 vector defining the ticks on the Y axis : Y of first tick, Y of last tick and number of intervals between them (default: NA). See \code{par}.
}
  \item{bty}{
    A character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box. See \code{par}.
}
  \item{las}{
    The direction of both X and Y axis labels: 0 for labels parallel to the axes, 1 for horizontal labels, 2 for labels perpendicular to the axes and 3 for vertical labels. See \code{par}.
}
  \item{xgrid}{
    Single logical value, whether to draw a grid on X axis or not.
}
  \item{new}{
    Single logical value, whether to plot on top of previous track (\code{TRUE}) or in a new track (\code{FALSE}, default).
}
  \item{bg}{
    Single character value, defining the color of the background (margins included) as an english name or a hexadecimal code. Similar to \code{par}'s argument but not relying on it, \code{NA} corresponds to the default \code{par}-based behavior.
}
  \item{bg.inner}{
    Single character value, defining the color of the background (margins excluded) as an english name or a hexadecimal code. \code{NA} corresponds to the default \code{par}-based behavior.
}
  \item{fg}{
    Single character value, defining the color of the foreground (axes, labels...) as an english name or a hexadecimal code. Similar to \code{par}'s argument but not relying on it.
}
  \item{\dots}{
    Not used, only ignores other arguments.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{draw.boxes}}, \code{\link{draw.density}}, \code{\link{draw.hist}}, \code{\link{draw.pileup}}, \code{\link{draw.points}}, \code{\link{draw.seq}}, \code{\link{draw.steps}}
}

