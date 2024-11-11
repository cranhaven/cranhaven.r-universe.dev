\name{singlePlot}
\alias{singlePlot}
\title{
  Whole genome plot of a drawable list
}

\description{
  This function plots a drawable list on all the chromosomes side by side, with a constant X axis scale. It mainly defines a custom layout for a series of \code{\link{browsePlot}} calls.
}

\usage{
  singlePlot(drawables, columns = 4, exclude = c("X", "Y"), add = c(5e6, 15e6),
    vertical = FALSE, capWidth = "1 cm", spacer = "1 cm", finalize = TRUE,
    cap.border = "black", cap.font.col = "black", cap.bg.col = NA, cap.adj = c(0.5, 0.5),
    cap.cex = 2, cap.font = 2, mar = c(0,0,0,0), bty = "n", xaxt = "n", xgrid = FALSE,
    yaxt = "n", ylab = "", ysub = "", ...)
}

\arguments{
  \item{drawables}{
    A \code{\linkS4class{drawable.list}} object containing the tracks to plot, as returned by \code{\link{drawable.list}} or \code{\link{tk.tracks}}.
}
  \item{columns}{
    Single integer value, column count in the chromosome layout.
}
  \item{exclude}{
    Character vector, the names of the chromosomes to not plot.
}
  \item{add}{
    Numeric vector of length 2, the margins to add before and after each chromosome, in base pairs.
}
  \item{vertical}{
    Single logical value, whether to produce a plot showing chromosomes horizontally or vertically. Actually the figure will need to be manually rotated in the resulting file.
}
  \item{capWidth}{
    Single value defining the width of the chromosome name caps, in a suitable format for \code{layout} \code{widths} argument.
}
  \item{spacer}{
    Single value defining the height of the inter-chromosome gaps, in a suitable format for \code{layout} \code{heights} argument.
}
  \item{finalize}{
    Single logical value, whether to fill unused space with a blank plot and return to default layout or allow further manual addition. If \code{FALSE}, the next plot will fill the whole blank space left.
}
  \item{cap.border}{
    Single value defining the color of chromosome cap borders. See the \code{border} argument of \code{rect} for further details.
}
  \item{cap.font.col}{
    Single value defining the color of chromosome names in the caps. See the \code{font} argument of \code{par} for further details.
}
  \item{cap.bg.col}{
    Single value defining the color of chromosome cap background. See the \code{col} argument of \code{rect} for further details.
}
  \item{cap.adj}{
    Numeric vector of length 2 defining the X and Y adjustment of the chromosome names in the caps, ranging between 0 (left / bottom) to 1 (right / top).
}
  \item{cap.cex}{
    Single value defining the character expansion factor for chromosome names in the caps. See the \code{cex} argument of \code{par} for further details.
}
  \item{cap.font}{
    Single value defining the font type of chromosome names in the caps. See the \code{font} argument of \code{par} for further details.
}
  \item{mar}{
    Refer to \dots described here-after. To disable the overriding and let each track define its own value as usual, set this parameter to \code{NULL}.
}
  \item{bty}{
    Refer to \dots described here-after. To disable the overriding and let each track define its own value as usual, set this parameter to \code{NULL}.
}
  \item{xaxt}{
    Refer to \dots described here-after. To disable the overriding and let each track define its own value as usual, set this parameter to \code{NULL}.
}
  \item{xgrid}{
    Refer to \dots described here-after. To disable the overriding and let each track define its own value as usual, set this parameter to \code{NULL}.
}
  \item{yaxt}{
    Refer to \dots described here-after. To disable the overriding and let each track define its own value as usual, set this parameter to \code{NULL}.
}
  \item{ylab}{
    Refer to \dots described here-after. To disable the overriding and let each track define its own value as usual, set this parameter to \code{NULL}.
}
  \item{ysub}{
    Refer to \dots described here-after. To disable the overriding and let each track define its own value as usual, set this parameter to \code{NULL}.
}
  \item{\dots}{
    Further arguments will be passed to \code{\link{browsePlot}} for each chromosome, which itself will pass it to the \code{draw} method of each track, overriding the class default and object setting.
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{browsePlot}}, \code{\linkS4class{drawable.list}}, \code{\linkS4class{drawable}}
}

