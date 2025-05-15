\name{PCA.spaghetti.SDA}
\alias{PCA.spaghetti.SDA}
\title{principal component analysis for symbolic objects described by symbolic interavl variables. \emph{Spaghetti} algorithm}
\description{principal component analysis for symbolic objects described by symbolic interavl variables. \emph{Spaghetti} algorithm}
\usage{
PCA.spaghetti.SDA(t,pc.number=2)
}
\arguments{
\item{t}{symbolic interval data: a 3-dimensional table, first dimension represents object number, second dimension - variable number, and third dimension contains lower- and upper-bounds of intervals (Simple form of symbolic data table)}
\item{pc.number}{number of principal components}
}
\details{
See file \url{../doc/PCA_SDA.pdf} for further details
}
\value{
Data in reduced space (symbolic interval data: a 3-dimensional table)
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} 

Department of Econometrics and Computer Science, University of Economics, Wroclaw, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Billard L., Diday E. (eds.) (2006), \emph{Symbolic Data Analysis, Conceptual Statistics and Data Mining}, John Wiley & Sons, Chichester.

Bock H.H., Diday E. (eds.) (2000), \emph{Analysis of symbolic data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Diday E., Noirhomme-Fraiture M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.
}
\seealso{
\code{\link{PCA.centers.SDA}},
\code{\link{PCA.mrpca.SDA}},
\code{\link{PCA.spca.SDA}},
\code{\link{PCA.vertices.SDA}}
}
\examples{
# Example will be available in next version of package, thank You for your patience :-)
}
\keyword{PCA}
\keyword{Principal components analysis}
