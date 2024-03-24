\name{latent}
\alias{latent}
\title{Set specifications of a latent variable}
\usage{
  latent(label = "latent", x = 0.5, y = 0.5, rx = 0.05,
    ry = 0.05, border = "white", lwd = 2, fill = "#5f8bd7",
    col = "white", cex = 1, vfont = NULL, font = 2,
    family = "sans")
}
\arguments{
  \item{label}{A character string with the label to be
  displayed.}

  \item{x}{x-axis coordinate for center of ellipse.}

  \item{y}{y-axis coordinate for center of ellipse.}

  \item{rx}{long radius of ellipse.}

  \item{ry}{short radius of ellipse.}

  \item{border}{color of the border.}

  \item{lwd}{width of border line.}

  \item{fill}{color to fill the ellipse}

  \item{col}{color of the label.}

  \item{cex}{numeric character expansion of the label.}

  \item{vfont}{font family of the label.}

  \item{font}{An integer specifying which font to use for
  the label. See \code{\link{par}}}

  \item{family}{The name of a font family for drawing text.
  Standard values are \code{"serif"}, \code{"sans"} and
  \code{"mono"}.}
}
\value{
  An object of class \code{"latent"}, which is a list with
  the specified parameters to draw latent variables.
}
\description{
  Use this function to specify the graphic characteristics
  of a latent variable. The specifications will be used by
  the function \code{draw} to plot latent variables (in a
  path diagram).
}
\details{
  Latent variables are drawn as ellipses using the function
  \code{\link{plotellipse}}
}
\examples{
\dontrun{
 # latent variables
 attack = latent("ATTACK", x=0.35, y=0.7, rx=0.08, ry=0.06)
 defense = latent("DEFENSE", x=0.35, y=0.3, rx=0.08, ry=0.06)
 success = latent("SUCCESS", x=0.65, y=0.5, rx=0.08, ry=0.06)

 # opwn wall
 wall()
 title("Drawing three latent variables", col.main="gray20")

 # draw variables
 draw(attack)
 draw(defense)
 draw(success)
 }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{manifest}}, \code{\link{draw}}
}

