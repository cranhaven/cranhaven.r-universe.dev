\name{heat.scale}
\alias{heat.scale}
\title{
  Plots a heat map color scale, for legend
}

\description{
  This function plots a color scale using a custom color palette, to legend \code{\link{heat.map}} derivated functions.
}

\usage{
  heat.scale(zlim, col.heatmap, at = -10:10, labels = NULL, horiz = TRUE,
    robust = FALSE, customMar = FALSE, title=NA)
}

\arguments{
  \item{zlim}{
    Numeric vector of length 2, minimum and maximum of values in the palette. Should correspond to \code{zlim} in \code{\link{heat.map}}, consider to use \code{\link{heat.map}} invisible return to get special values.
}
  \item{col.heatmap}{
    Character vector of colors used in the heat map. Should correspond to \code{col.heatmap} in \code{\link{heat.map}}, consider to use \code{\link{heat.map}} invisible return to get special values.
}
  \item{at}{
    Numeric vector, values shown in the axis.
}
  \item{labels}{
    Character vector as long as \code{at}, defining the values to show at \code{at}.
}
  \item{horiz}{
    Single logical value, whether to plot an horizontal or a vertical scale.
}
  \item{robust}{
    Single logical value, whether to legend \code{median} and \code{mad} or \code{mean} and \code{sd}. Should correspond to \code{\link{heat.map}} \code{norm.robust} value.
}
  \item{customMar}{
    Single logical value, whether to skip the call to \code{\link{par}} to set \code{mar} or not.
}
  \item{title}{
    Single character value, the axis title to use (\code{NA} for automatic generation).
}
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{heat.map}}, \code{\link{clusterize}}, \code{\link{predict.LPS}}
}

