\name{draw.hist}
\alias{draw.hist}
\title{
  Histogram plot of a track
}

\description{
  This function draws a slice of a track content, with a distinct vertical bar for each track element.
}

\usage{
  draw.hist(slice, start, end, column = "value", fillColor = "#666666",
    border = "#666666", cex.lab = 1, origin = 0, bty = "o", fg = "#000000",
    ylim = NA, ...)
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
  \item{fillColor}{
    The color to fill vertical bars with (as a name, an integer or an hexadecimal character description). It can alternatively be a function without argument, which returns a vector of as many colors as \code{slice} has rows. It can make direct variable call to any argument described on this page (including custom arguments passed via "...").
}
  \item{border}{
    The color to use for box borders (as a name, an integer or an hexadecimal character description). It can alternatively be a function without argument, which returns a vector of as many colors as \code{slice} has rows. It can make direct variable call to any argument described on this page (including custom arguments passed via "..."). Special values \code{NA} and \code{"fillColor"} can also be used to disable borders or use \code{fillColor} respectively.
}
  \item{cex.lab}{
    The relative character size of x and y axis labels (default: 1). See \code{par}.
}
  \item{origin}{
    Single numeric value, the Y value of the horizontal side common to all boxes. Can also be the name of a \code{slice} numeric column to use as a segment-specific origin.
}
  \item{bty}{
    A character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box. See \code{par}.
}
  \item{fg}{
    Single character value, defining the color of the foreground (axes, labels...) as an english name or a hexadecimal code. Similar to \code{par}'s argument but not relying on it.
}
  \item{ylim}{
    Numeric vector of length two, defining the Y axis boundaries. Any or both of them can be NA, meaning the missing boundary will be inferred from the data to plot.
}
  \item{\dots}{
    Further arguments to be passed to \code{\link{draw.bg}}.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{draw.bg}}, \code{\link{draw.boxes}}, \code{\link{draw.density}}, \code{\link{draw.pileup}}, \code{\link{draw.points}}, \code{\link{draw.seq}}, \code{\link{draw.steps}}
}

