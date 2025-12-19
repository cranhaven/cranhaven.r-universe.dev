\encoding{latin1}
\name{Couepia}
\alias{Couepia}
\docType{data}
\title{Spatial pattern of Couepia caryophylloides in Paracou, a canopy tree species of French Guiana.}
\description{
Spatial pattern of 34 mature individuals and 173 young individuals of the tree species \emph{Couepia caryophylloides} (Chrysobalanaceae) in a 25-ha forest plot in Paracou, French Guiana.
}
\usage{data(Couepia)}
\format{
A list with 4 components:\cr
\code{$rect  } is a vector of coordinates \eqn{(xmin,ymin,xmax,ymax)} of the origin and the opposite corner of a 500 by 500 m rectangular plot.\cr
\code{$tri   } is a list of vertex coordinates \eqn{(ax,ay,bx,by,cx,cy)} of contiguous triangles covering swampy parts of the plot.\cr
\code{$trees } is a list of tree coordinates \eqn{(x,y)}.\cr
\code{$stage } is a factor with 2 levels \eqn{("mature","young")}.\cr
}
\source{
  Collinet, F. 1997. \emph{Essai de regroupement des principales especes structurantes d'une foret dense humide d'apres l'analyse de leur repartition spatiale (foret de Paracou - Guyane).} These de doctorat, Universite Claude Bernard, Lyon, France.
}
\references{
Goreaud, F. & P?Pelissier, R. 2003. Avoiding misinterpretation of biotic interactions with the intertype \emph{K12}-function: population independence vs. random labelling hypotheses. \emph{Journal of Vegetation Science}, 14: 681-692.
}
\examples{
data(Couepia)
coca.spp <- spp(Couepia$trees, mark=Couepia$stage, window=Couepia$rect, triangles=Couepia$tri)
plot(coca.spp)
}
\keyword{datasets}
