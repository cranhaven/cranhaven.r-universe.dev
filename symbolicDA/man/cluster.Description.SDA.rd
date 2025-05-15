\name{cluster.Description.SDA}
\alias{cluster.Description.SDA}
\title{description of clusters of symbolic objects}
\description{description of clusters of symbolic objects is obtained by a generalisation operation using in most cases descriptive statistics calculated separately for each cluster and each symbolic variable. 
}
\usage{
cluster.Description.SDA(table.Symbolic, clusters, precission=3)
}
\arguments{
\item{table.Symbolic}{Symbolic data table}
\item{clusters}{a vector of integers indicating the cluster to which each object is allocated}
\item{precission}{Number of digits to round the results}
}
\value{
A List of cluster numbers, variable number and labels.

The description of clusters of symbolic objects which differs according to the symbolic variable type:

- for interval-valued variable:

"min value" - minimum value of the lower-bounds of intervals observed for objects belonging to the cluster

"max value" - maximum value of the upper-bounds of intervals observed for objects belonging to the cluster

- for multinominal variable:

"categories" - list of all categories of the variable observed for symbolic belonging to the cluster

- for multinominal with weights variable:

"min probabilities" - minimum weight of each category of the variable observed for objects belonging to the cluster

"max probabilities" - maximum weight of each category of the variable observed for objects belonging to the cluster

"avg probabilities" - average weight of each category of the variable calculated for objects belonging to the cluster

"sum probabilities" - sum of weights of each category of the variable calculated for objects belonging to the cluster
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl}, Justyna Wilk \email{justyna.wilk@ue.wroc.pl}
Department of Econometrics and Computer Science, Wroclaw University of Economics, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Billard, L., Diday, E. (eds.) (2006), \emph{Symbolic Data Analysis. Conceptual Statistics and Data Mining}, Wiley, Chichester.

Verde, R., Lechevallier, Y., Chavent, M. (2003), \emph{Symbolic clustering interpretation and visualization}, "The Electronic Journal of Symbolic Data Analysis", Vol. 1, No 1.

Bock, H.H., Diday, E. (eds.) (2000), \emph{Analysis of symbolic data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Diday E., Noirhomme-Fraiture, M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.
}

\seealso{
\code{\link{SClust}},\code{\link{DClust}}; \code{\link{hclust}} in \code{stats} library; \code{pam} in \code{cluster} library
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
#data("cars",package="symbolicDA")
#y<-cars
#cl<-SClust(y, 4, iter=150)
#print(cl)
#o<-cluster.Description.SDA(y, cl)
#print(o)
}
\keyword{cluster}
