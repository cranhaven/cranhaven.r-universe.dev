\name{arrow}
\alias{arrow}
\title{Plot arrow between variables}
\usage{
  arrow(from, to, start = "east", end = "west",
    length = 0.1, angle = 10, code = 2, col = "#d2def1",
    lwd = 3, ...)
}
\arguments{
  \item{from}{An object of either class \code{"manifest"}
  or \code{"latent"}. This object is the origin of the
  arrow.}

  \item{to}{An object of either class \code{"manifest"} or
  \code{"latent"}. This object is the destination of the
  arrow.}

  \item{start}{Character string to specify the starting
  direction of the arrow. Options are \code{"north"},
  \code{"south"}, \code{"east"}, \code{"west"}.}

  \item{end}{Character string to specify the ending
  direction of the arrow. Options are \code{"north"},
  \code{"south"}, \code{"east"}, \code{"west"}.}

  \item{length}{length of the edges of the arrow head (in
  inches).}

  \item{angle}{angle from the shaft of the arrow to the
  edge of the arrow head.}

  \item{code}{integer code, determining kind of arrows to
  be drawn.}

  \item{col}{color of the arrow.}

  \item{lwd}{width of the arrow.}

  \item{...}{other arguments passed on to \code{arrows}.}
}
\description{
  Use this function to draw connecting arrows between
  manifest and latent variables.
}
\examples{
\dontrun{
 # latent variables
 attack = latent("ATTACK", x=0.35, y=0.7, rx=0.08, ry=0.06)
 defense = latent("DEFENSE", x=0.35, y=0.3, rx=0.08, ry=0.06)
 success = latent("SUCCESS", x=0.65, y=0.5, rx=0.08, ry=0.06)

 # open wall
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

