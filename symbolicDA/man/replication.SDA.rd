\name{replication.SDA}
\alias{replication.SDA}
\title{Modification of replication analysis for cluster validation of symbolic data}
\description{Replication analysis for cluster validation of symbolic data}
\usage{
replication.SDA(table.Symbolic, u=2, method="SClust", S=10, fixedAsample=NULL, ...)
}
\arguments{
\item{table.Symbolic}{symbolic data table}
\item{u}{number of clusters given arbitrarily}
\item{method}{clustering method: "SClust" (default), "DClust", "single", "complete", "average", "mcquitty", "median", "centroid", "ward", "pam", "diana"}
\item{S}{the number of simulations used to compute average adjusted Rand index}
\item{fixedAsample}{if NULL \emph{A} sample is generated randomly, otherwise this parameter contains object numbers arbitrarily assigned to \emph{A} sample}
\item{...}{additional argument passed to \code{\link{dist_SDA}} function}
}
\value{
\item{A}{3-dimensional array containing data matrices for A sample of objects in each simulation (first dimension represents simulation number, second - object number, third - variable number)}
\item{B}{3-dimensional array containing data matrices for B sample of objects in each simulation (first dimension represents simulation number, second - object number, third - variable number)}
\item{medoids}{3-dimensional array containing matrices of observations on u representative objects (medoids) for A sample of objects in each simulation (first dimension represents simulation number, second - cluster number, third - variable number)}
\item{clusteringA}{2-dimensional array containing cluster numbers for A sample of objects in each simulation (first dimension represents simulation number, second - object number)}
\item{clusteringB}{2-dimensional array containing cluster numbers for B sample of objects in each simulation (first dimension represents simulation number, second - object number)}
\item{clusteringBB}{2-dimensional array containing cluster numbers for B sample of objects in each simulation according to 4 step of replication analysis procedure (first dimension represents simulation number, second - object number)}
\item{cRand}{value of average adjusted Rand index for S simulations}
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl}, Justyna Wilk \email{justyna.wilk@ue.wroc.pl}
Department of Econometrics and Computer Science,Wroclaw University of Economics, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Breckenridge, J.N. (2000), \emph{Validating cluster analysis: consistent replication and symmetry}, "Multivariate Behavioral Research", 35 (2), 261-285. Available at: \doi{10.1207/S15327906MBR3502_5}.

Gordon, A.D. (1999), \emph{Classification}, Chapman and Hall/CRC, London. ISBN 9781584880134.

Hubert, L., Arabie, P. (1985), \emph{Comparing partitions}, "Journal of Classification", no. 1, 193-218. Available at: \doi{10.1007/BF01908075}.

Milligan, G.W. (1996), \emph{Clustering validation: results and implications for applied analyses}, In P. Arabie, L.J. Hubert, G. de Soete (Eds.), \emph{Clustering and classification}, World Scientific, Singapore, 341-375. ISBN 9789810212872.

Bock H.H., Diday E. (eds.) (2000), \emph{Analysis of Symbolic Data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Diday E., Noirhomme-Fraiture M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.
}
\details{
See file \url{../doc/replicationSDA_details.pdf} for further details
}
\seealso{
\code{\link{dist_SDA}}, \code{\link{SClust}}, \code{\link{DClust}}; \code{hclust} in \code{stats} library; \code{pam} in \code{cluster} library; \code{replication.Mod} in \code{clusterSim} library
}
\examples{
#data("cars",package="symbolicDA")
#set.seed(123)
#w<-replication.SDA(cars, u=3, method="SClust", S=10)
#print(w)
}
\keyword{cluster}
\keyword{multivariate}
