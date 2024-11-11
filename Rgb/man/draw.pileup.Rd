\name{draw.pileup}
\alias{draw.pileup}
\title{
  Pileup plot of a BAM track
}

\description{
  This function draws a slice of a sequence pileup, highlighting polymorphisms.
}

\usage{
  draw.pileup(slice, start, end, ylim = NA, bty = "o", label = TRUE, labelCex = 0.75,
    bases = c(A = "#44CC44", C = "#4444CC", G = "#FFCC00", T = "#CC4444"),
    maxRange = 500, cex.lab = 1, alphaOrder = 3, alphaMin = 0.1, fg = "#000000", ...)
}

\arguments{
  \item{slice}{
    An integer matrix of read counts, with nucleotides in rows and positions in columns. Both dimensions must be named.
}
  \item{start}{
    Single integer value, the left boundary of the window, in base pairs.
}
  \item{end}{
    Single integer value, the right boundary of the window, in base pairs.
}
  \item{ylim}{
    See \code{plot.default}. \code{NA} will compute a dynamic ylim fiting the data.
}
  \item{bty}{
    A character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box. See \code{par}.
}
  \item{label}{
    Single logical value, whether to print nucleotide on bars or not.
}
  \item{labelCex}{
    Single numeric value, character expansion factor for labels.
}
  \item{bases}{
    Named character vector, defining the color to use for each nucleotide.
}
  \item{maxRange}{
    Single integer value, nothing will be ploted if the plot window is wider by this value (in bases).
}
  \item{cex.lab}{
    The relative character size of x and y axis labels (default: 1). See \code{par}.
}
  \item{alphaOrder}{
    Single numeric value, the order of the formula controlling the transparency. Increase this value to increase sensitivity to rare variants.
}
  \item{alphaMin}{
    Single numeric value, the minimal intensity in the formula controlling the transparency (between 0 and 1). Perfectly homozyguous positions will typically use this intensity of color.
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
  \code{\link{track.bam-class}}
  
  \code{\link{draw.bg}}, \code{\link{draw.boxes}}, \code{\link{draw.density}}, \code{\link{draw.hist}}, \code{\link{draw.points}}, \code{\link{draw.seq}}, \code{\link{draw.steps}}
}

