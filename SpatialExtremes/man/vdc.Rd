\name{vdc}
\alias{vdc}
\title{
  Van der Corput Sequence
}
\description{
  This function generates the three dimensional Van der Corput sequence
  on the half unit Sphere of \eqn{R^3}{R^3} - eventually randomly
  rotated.
}
\usage{
vdc(n, rand.rot = FALSE)
}
\arguments{
  \item{n}{Integer. The length of the sequence.}
  \item{rand.rot}{Logical. Should the sequence be randomly rotated?}
}
\value{
A matrix giving the coordinates of the points in the half unit sphere.
}
\references{
Freulon, X., de Fouquet, C., 1991. Remarques sur la pratique des bandes
tournantes a trois dimensions. Cahiers de geostatistique, Fascicule
1, Centre de Geostatistique, Ecole des Mines de Paris, Fontainebleau,
pp. 101--117.
}
\author{
Mathieu Ribatet
}
\examples{
vdc(10)
}
\keyword{distribution}
