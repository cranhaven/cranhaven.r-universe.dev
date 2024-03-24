\name{wall}
\alias{wall}
\title{Open a new frame for a path diagram}
\usage{
  wall(xlim = c(0, 1), ylim = c(0, 1), xpd = TRUE, ...)
}
\arguments{
  \item{xlim}{Numeric vector of length 2 giving the x
  coordinate range. Default \code{xlim = c(0, 1)}.}

  \item{ylim}{Numeric vector of length 2 giving the y
  coordinate range. Default \code{ylim = c(0, 1)}.}

  \item{xpd}{Logical value to indicate if all plotting is
  clipped to the figure region. The default is c(1, 1, 1,
  1).}

  \item{...}{other graphical arguments passed on to
  \code{\link{plot.window}}.}
}
\description{
  Use this function to open a white canvas to start drawing
  a path diagram. By default, \code{wall} opens a new plot
  window from 0 to 1 in both axes.
}
\details{
  \code{wall} calls \code{plot.new()} and
  \code{plot.window()} to open a new plot frame.
}
\examples{
\dontrun{
 # latent variables
 attack = latent("ATTACK", x=0.35, y=0.7, rx=0.08, ry=0.06)
 defense = latent("DEFENSE", x=0.35, y=0.3, rx=0.08, ry=0.06)
 success = latent("SUCCESS", x=0.65, y=0.5, rx=0.08, ry=0.06)

 # open diagram
 wall()

 # draw latent variables
 draw(attack)
 draw(defense)
 draw(success)

 # add arrows
 arrow(from=attack, to=success, start="east", end="west")
 arrow(from=defense, to=success, start="east", end="west")
 }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{manifest}}, \code{\link{latent}},
  \code{\link{draw}}
}

