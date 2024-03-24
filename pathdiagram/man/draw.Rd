\name{draw}
\alias{draw}
\title{Draw manifest and latent variables}
\usage{
  draw(obj)
}
\arguments{
  \item{obj}{An object of either class \code{"manifest"} or
  \code{"latent"}}
}
\description{
  Use this function to draw either manifest or latent
  variables on a plot.
}
\examples{
\dontrun{
 # manifest variables
 ingredients = list(
   eggs = manifest("eggs", x=0.3, y=0.7, width=0.10, height=0.08),
   milk = manifest("milk", x=0.3, y=0.6, width=0.10, height=0.08),
   flour = manifest("flour", x=0.3, y=0.5, width=0.10, height=0.08),
   sugar = manifest("sugar", x=0.3, y=0.4, width=0.10, height=0.08),
   butter = manifest("butter", x=0.3, y=0.3, width=0.10, height=0.08)
 )

 # latent variables
 pancakes = latent("PANCAKES", x=0.6, y=0.6, rx=0.09, ry=0.07)
 waffles = latent("WAFFLES", x=0.6, y=0.4, rx=0.09, ry=0.07)

 # open wall
 wall()

 title("Toy Path Diagram", col.main="gray20")
 # draw manifest variables
 for (i in 1:length(ingredients)) {
    draw(ingredients[[i]])
 }

 # draw latent variables
 draw(pancakes)
 draw(waffles)
 # draw arrows
 for (i in 1:length(ingredients)) {
    arrow(from=ingredients[[i]], to=pancakes, start="east", end="west")
    arrow(from=ingredients[[i]], to=waffles, start="east", end="west")
 }
 }
}
\author{
  Gaston Sanchez
}
\seealso{
  \code{\link{manifest}}, \code{\link{latent}}
}

