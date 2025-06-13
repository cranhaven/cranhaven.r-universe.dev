\name{heat}
\alias{heat}
\alias{heat.lin}
\alias{heat.exp}

\title{
  Heatmap palette generation
}

\description{
  This function generates a ramp of colors for \code{\link{heat.map}} derivated functions.
}

\usage{
  heat(colors = c("#8888FF", "#000000", "#FF4444"), n = 256, shapeFun = heat.exp, ...)
  heat.exp(n, part, base = 1.015)
  heat.lin(n, part)
}

\arguments{
  \item{colors}{
    Character vector of length 3, determining starting, middle and final colors.
}
  \item{n}{
    Single integer value, amount of colors / values to generate.
}
  \item{shapeFun}{
    Function taking at least 2 arguments : \code{n} and \code{part}. \code{\link{heat.exp}} and \code{\link{heat.lin}} are provided as examples.
}
  \item{\dots}{
    Further arguments to \code{heat} will be passed to \code{shapeFun}.
}
  \item{part}{
    Single integer, defined as 1 while generating colors between the first two boundaries, and 2 otherwise.
}
  \item{base}{
    Single numeric value, base for exponential slope.
}
}

\value{
  \code{heat} returns a character vector of colors in hexadecimal representation.
  
  \code{heat.lin} and \code{heat.expr} return \code{n} numeric values, defining a curve whose slope will be mimiced during color interpolation.
}

\author{
  Sylvain Mareschal
}

\seealso{
  \code{\link{colorRampPalette}}
  
  \code{\link{heat.map}}, \code{\link{clusterize}}, \code{\link{predict.LPS}}
}

\examples{
  # Classical heatmap colors
  palette <- heat(c("green", "black", "red"))
  heat.scale(zlim=c(-2,2), col.heatmap=palette)
  
  # Two distinct shapes provided
  heat.scale(zlim=c(-2,2), col.heatmap=heat(shapeFun=heat.lin))
  heat.scale(zlim=c(-2,2), col.heatmap=heat(shapeFun=heat.exp))
}

