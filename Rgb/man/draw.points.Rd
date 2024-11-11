\name{draw.points}
\alias{draw.points}
\title{
  Scatter plot of a track
}

\description{
  This function draws a slice of a track content, with a distinct point for each track element.
}

\usage{
  draw.points(slice, start, end, column = "value", pointColor = "#666666",
    cex.lab = 1, cex = 0.6, pch = "+", bty = "o", fg = "#000000", ...)
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
  \item{pointColor}{
    The color to use for points (as a name, an integer or an hexadecimal character description). It can alternatively be a function without argument, which returns a vector of as many colors as \code{slice} has rows. It can make direct variable call to any argument described on this page (including custom arguments passed via "...").
}
  \item{cex.lab}{
    See \code{par}.
}
  \item{cex}{
    See \code{par}.
}
  \item{pch}{
    See \code{par}.
}
  \item{bty}{
    See \code{par}.
}
  \item{fg}{
    Single character value, defining the color of the foreground (axes, labels...) as an english name or a hexadecimal code. Similar to \code{par}'s argument but not relying on it.
}
  \item{\dots}{
    Further arguments to be passed to \code{\link{draw.bg}}.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{draw.bg}}, \code{\link{draw.boxes}}, \code{\link{draw.density}}, \code{\link{draw.hist}}, \code{\link{draw.pileup}}, \code{\link{draw.seq}}, \code{\link{draw.steps}}
}

