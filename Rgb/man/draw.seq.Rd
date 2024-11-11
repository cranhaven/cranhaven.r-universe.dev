\name{draw.seq}
\alias{draw.seq}
\title{
  Plot a sequence of nucleotides
}

\description{
  This function draws a slice of a character vector, with labels and distinct colors for each nucleotide.
}

\usage{
  draw.seq(slice = NULL, start, end, bty = "o", labelCex = 0.75,
    bases = c(A = "#44CC44", C = "#4444CC", G = "#FFCC00", T = "#CC4444"),
    maxRange = 500, cex.lab = 1, fg = "#000000", ...)
}

\arguments{
  \item{slice}{
    Character vector, with a single letter per element.
}
  \item{start}{
    Single integer value, the left boundary of the window, in base pairs.
}
  \item{end}{
    Single integer value, the right boundary of the window, in base pairs.
}
  \item{bty}{
    A character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box. See \code{par}.
}
  \item{labelCex}{
    Single numeric value, character expansion factor for labels.
}
  \item{bases}{
    Named character vector, defining the color to use for each nucleotide (names have to be uppercase, \code{slice} is converted to uppercase for matching).
}
  \item{maxRange}{
    Single integer value, nothing will be ploted if the plot window is wider by this value (in bases).
}
  \item{cex.lab}{
    The relative character size of x and y axis labels (default: 1). See \code{par}.
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
  \code{\link{track.fasta-class}}
  
  \code{\link{draw.bg}}, \code{\link{draw.boxes}}, \code{\link{draw.density}}, \code{\link{draw.hist}}, \code{\link{draw.pileup}}, \code{\link{draw.points}}, \code{\link{draw.steps}}
}

