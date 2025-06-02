\name{COSha10map}
\alias{COSha10map}
\docType{data}
\title{
Map of total soil carbon stock (t/ha) at 0-10 cm depth
}
\description{
Map of total soil carbon stock (t ha\eqn{^{-1}}) at 0-10 cm depth at La Libertad Research Center. The map was obtained through ordinary kriging interpolation. Spatial reference system: UTM 18N
}
\usage{data(COSha10map)}
\format{
  The format is:
Formal class 'SpatialPixelsDataFrame' [package "sp"] 
}
\source{
Santacruz, A., 2010. \emph{Design of optimal spatial sampling networks for the monitoring of soil organic carbon at La Libertad Research Center through the application of genetic algorithms}. M.Sc. Thesis. National University of Colombia, Bogota. 162 p. (In Spanish)
}
\references{
Santacruz, A., Rubiano, Y., Melo, C., 2014. \emph{Evolutionary optimization of spatial sampling networks designed for the monitoring of soil carbon}. In: Hartemink, A., McSweeney, K. (Eds.). \emph{Soil Carbon. Series: Progress in Soil Science}. (pp. 77-84). Springer. \href{https://link.springer.com/chapter/10.1007/978-3-319-04084-4_8}{[link]}

Santacruz, A., 2011. \emph{Evolutionary optimization of spatial sampling networks. An application of genetic algorithms and geostatistics for the monitoring of soil organic carbon}. Editorial Academica Espanola. 183 p. ISBN: 978-3-8454-9815-7 (In Spanish)
}
\seealso{
\code{\link{COSha10}}
}
\examples{
data(COSha10map)
data(lalib)
summary(COSha10map)
l1 = list("sp.polygons", lalib)
spplot(COSha10map, "var1.pred", main="Soil carbon stock (t/ha) at 0-10 cm depth", 
    col.regions=bpy.colors(100), scales = list(draw =TRUE), xlab ="East (m)", 
    ylab = "North (m)", sp.layout=list(l1))
}
\keyword{datasets}
