\name{SClust}
\alias{SClust}
\title{Dynamical clustering of symbolic data}
\description{Dynamical clustering of symbolic data based on symbolic data table}
\usage{
SClust(table.Symbolic, cl, iter=100, variableSelection=NULL, objectSelection=NULL)
}
\arguments{
\item{table.Symbolic}{symbolic data table} 
\item{cl}{number of clusters or vector with initial prototypes of clusters} 
\item{iter}{maximum number of iterations}
\item{variableSelection}{vector of numbers of variables to use in clustering procedure or NULL for all variables}
\item{objectSelection}{vector of numbers of objects to use in clustering procedure or NULL for all objects}
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

Diday, E., Noirhomme-Fraiture, M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester, pp. 185-191.

Verde, R. (2004), \emph{Clustering Methods in Symbolic Data Analysis}, In: D. Banks, L. House, E. R. McMorris, P. Arabie, W. Gaul (Eds.), Classification, clustering and Data mining applications, Springer-Verlag, Heidelberg, pp. 299-317.

Diday, E. (1971), \emph{La methode des Nuees dynamiques}, Revue de Statistique Appliquee, Vol. 19-2, pp. 19-34.

Celeux, G., Diday, E., Govaert, G., Lechevallier, Y., Ralambondrainy, H. (1988), \emph{Classifcation Automatique des Donnees}, Environnement Statistique et Informatique - Dunod, Gauthier-Villards, Paris.
}
\details{
See file \url{../doc/SClust_details.pdf} for further details
}
\seealso{
\code{\link{DClust}}; \code{\link{kmeans}} in \code{stats} library
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
#data("cars",package="symbolicDA")
#sdt<-cars
#clust<-SClust(sdt, cl=3, iter=50)
#print(clust)
}
\keyword{cluster}
