\name{manifest}
\alias{manifest}
\title{Set specifications of a manifest variable}
\usage{
  manifest(label = "manifest", x = 0.5, y = 0.5,
    width = NULL, height = 0.1, border = "white",
    fill = "#9dbafa", lwd = 1, col = "gray20", cex = 1,
    vfont = NULL, font = 1, family = "sans")
}
\arguments{
  \item{label}{A character string with the label to be
  displayed.}

  \item{x}{x-axis coordinate for center of rectanlge.}

  \item{y}{y-axis coordinate for center of rectangle.}

  \item{width}{width of the rectangle.}

  \item{height}{height of the rectangle.}

  \item{border}{color of the border.}

  \item{fill}{color to fill the rectangle.}

  \item{lwd}{width of the border.}

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
  An object of class \code{"manifest"}, which is a list
  with the specified parameters to draw manifest variables.
}
\description{
  Use this function to specify the graphic characteristics
  of a manifest variable. The specifications will be used
  by the function \code{draw} to plot manifest variables
  (in a path diagram).
}
\details{
  Manifest variables are drawn as rectangles.
}
\examples{
\dontrun{
 # manifest variables
 eggs = manifest("eggs", x=0.3, y=0.7, width=NULL, height=0.08)
 milk = manifest("milk", x=0.4, y=0.6, width=NULL, height=0.08)
 flour = manifest("flour", x=0.5, y=0.5, width=NULL, height=0.08)
 sugar = manifest("sugar", x=0.6, y=0.4, width=NULL, height=0.08)
 butter = manifest("butter", x=0.7, y=0.3, width=NULL, height=0.08)

 # open wall
 wall()
 title("Five manifest variables", col.main="gray20")

 # draw manifest variables
 draw(eggs)
 draw(milk)
 draw(flour)
 draw(sugar)
 draw(butter)
 }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{latent}}, \code{\link{draw}}
}

