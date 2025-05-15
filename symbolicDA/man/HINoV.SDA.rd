\name{HINoV.SDA}
\alias{HINoV.SDA}
\title{Modification of HINoV method for symbolic data}
\description{
Carmone, Kara and Maxwell's Heuristic Identification of Noisy Variables (HINoV) method for symbolic data}
\usage{
HINoV.SDA(table.Symbolic, u=NULL, distance="H", Index="cRAND",method="pam",...)
}
\arguments{
\item{table.Symbolic}{symbolic data table}
\item{u}{number of clusters}
\item{distance}{symbolic distance measure as parameter type in \code{\link{dist_SDA}}}
\item{method}{clustering method: "single", "ward", "complete", "average", "mcquitty", "median", "centroid", "pam" (default), "SClust", "DClust"}
\item{Index}{"cRAND" - adjusted Rand index (default); "RAND" - Rand index}
\item{...}{additional argument passed to \code{\link{dist_SDA}} function}
}
\value{
\item{parim}{\emph{m} x \emph{m} symmetric matrix (\emph{m} - number of variables). Matrix contains pairwise adjusted Rand (or Rand) indices for partitions formed by the \emph{j}-th variable with partitions formed by the \emph{l}-th variable}
\item{topri}{sum of rows of \code{parim}}
\item{stopri}{ranked values of \code{topri} in decreasing order}
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl}, Justyna Wilk \email{justyna.wilk@ue.wroc.pl}
Department of Econometrics and Computer Science, Wroclaw University of Economics, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Bock, H.H., Diday, E. (eds.) (2000), \emph{Analysis of Symbolic Data. Explanatory Methods for Extracting Statistical Information from Complex Data}, Springer-Verlag, Berlin.

Diday, E., Noirhomme-Fraiture, M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.

Carmone, F.J., Kara, A., Maxwell, S. (1999), \emph{HINoV: a new method to improve market segment definition by identifying noisy variables}, "Journal of Marketing Research", November, vol. 36, 501-509.

Hubert, L.J., Arabie, P. (1985), \emph{Comparing partitions}, "Journal of Classification", no. 1, 193-218. Available at: \doi{10.1007/BF01908075}.

Rand, W.M. (1971), \emph{Objective criteria for the evaluation of clustering methods}, "Journal of the American Statistical Association", no. 336, 846-850. Available at: \doi{10.1080/01621459.1971.10482356}.

Walesiak, M., Dudek, A. (2008), \emph{Identification of noisy variables for nonmetric and symbolic data in cluster analysis}, In: C. Preisach, H. Burkhardt, L. Schmidt-Thieme, R. Decker (Eds.), Data analysis, machine learning and applications, Springer-Verlag, Berlin, Heidelberg, 85-92. Available at: \doi{1007/978-3-540-78246-9_11}
}
\details{
For HINoV in symbolic data analysis there can be used methods based on distance matrix such as hierarchical ("single", "ward", "complete", "average", "mcquitty", "median", "centroid") and optimization methods ("pam", "DClust") and also methods based on symbolic data table ("SClust").

See file \url{../doc/HINoVSDA_details.pdf} for further details
}
\seealso{
\code{DClust}, \code{SClust}, \code{dist_SDA}; \code{HINoV.Symbolic}, \code{dist.Symbolic} in \code{clusterSim} library; \code{hclust} in \code{stats} library; \code{pam} in \code{cluster} library
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
#data("cars",package="symbolicDA")
#r<- HINoV.SDA(cars, u=3, distance="U_2")
#print(r$stopri)
#plot(r$stopri[,2], xlab="Variable number", ylab="topri",
#xaxt="n", type="b")
#axis(1,at=c(1:max(r$stopri[,1])),labels=r$stopri[,1])
}
\keyword{cluster}
