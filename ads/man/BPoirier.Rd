\encoding{latin1}
\name{BPoirier}
\alias{BPoirier}
\docType{data}
\title{Tree spatial pattern in Beau Poirier plot, Haye forest, France}
\description{
Spatial pattern of 162 beeches, 72 oaks and 3 hornbeams in a 1-ha 140 yr-old temperate forest plot in Haye, France. 
}
\usage{data(BPoirier)}
\format{
A list with 8 components:\cr
\code{$rect    } is a vector of coordinates \eqn{(xmin,ymin,xmax,ymax)} of the origin and the opposite corner of a 110 by 90 m rectangular plot.\cr
\code{$tri1    } is a list of vertex coordinates \eqn{(ax,ay,bx,by,cx,cy)} of contiguous triangles covering the denser part of the plot.\cr
\code{$tri2    } is a list of vertex coordinates \eqn{(ax,ay,bx,by,cx,cy)} of contiguous triangles covering the sparser part of the plot.\cr
\code{$poly1   } is a list of vertex coordinates \eqn{(x,y)} of the polygon enclosing \code{BPoirier$tri1}.\cr
\code{$poly2   } is a list of two polygons vertex coordinates \eqn{(x,y)} enclosing \code{BPoirier$tri2}.\cr
\code{$trees   } is a list of tree coordinates \eqn{(x,y)}.\cr
\code{$species } is a factor with 3 levels \eqn{("beech","oak","hornbeam")} corresponding to species names of the trees.\cr
\code{$dbh     } is a vector of tree size (diameter at breast height in cm). 
}
\source{
Parde, J. 1981. De 1882 a 1976/80 : les places d'experience de sylviculture du hetre en foret domaniale de Haye. \emph{Revue Forestiere Francaise}, 33: 41-64.
}

\references{
Goreaud, F. 2000. \emph{Apports de l'analyse de la structure spatiale en foret temperee a l'etude et la modelisation des peuplements complexes}. These de doctorat, ENGREF, Nancy, France.\cr\cr
Pelissier, R. & Goreaud, F. 2001. A practical approach to the study of spatial structure in simple cases of heterogeneous vegetation. \emph{Journal of Vegetation Science}, 12: 99-108.
}
\examples{
data(BPoirier)
BP.spp <- spp(BPoirier$trees, mark=BPoirier$species, window=BPoirier$rect)
plot(BP.spp)
}
\keyword{datasets}
