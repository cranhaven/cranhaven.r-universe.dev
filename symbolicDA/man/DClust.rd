\name{DClust}
\alias{DClust}
\title{Dynamical clustering based on distance matrix}
\description{Dynamical clustering of objects described by symbolic and/or classic (metric, non-metric) variables based on distance matrix}
\usage{
DClust(dist, cl, iter=100)
}
\arguments{
\item{dist}{distance matrix} 
\item{cl}{number of clusters or vector with initial prototypes of clusters} 
\item{iter}{maximum number of iterations}
}
\value{
a vector of integers indicating the cluster to which each object is allocated
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl}, Justyna Wilk \email{justyna.wilk@ue.wroc.pl}
Department of Econometrics and Computer Science, Wroclaw University of Economics, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Bock, H.H., Diday, E. (eds.) (2000), \emph{Analysis of Symbolic Data. Explanatory Methods for Extracting Statistical Information from Complex Data}, Springer-Verlag, Berlin.

Diday, E., Noirhomme-Fraiture, M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester, pp. 191-204.

Diday, E. (1971), \emph{La methode des Nuees dynamiques}, Revue de Statistique Appliquee, Vol. 19-2, pp. 19-34.

Celeux, G., Diday, E., Govaert, G., Lechevallier, Y., Ralambondrainy, H. (1988), \emph{Classifcation Automatique des Donnees}, Environnement Statistique et Informatique - Dunod, Gauthier-Villards, Paris.

}
\details{
See file \url{../doc/DClust_details.pdf} for further details
}
\seealso{
\code{\link{SClust}}, \code{\link{dist_SDA}}; \code{dist} in \code{stats} library; \code{dist.GDM} in \code{clusterSim} library; \code{pam} in \code{cluster} library
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
#data("cars",package="symbolicDA")
#sdt<-cars
#dist<-dist_SDA(sdt, type="U_3")
#clust<-DClust(dist, cl=5, iter=100)
#print(clust)

}
\keyword{cluster}
