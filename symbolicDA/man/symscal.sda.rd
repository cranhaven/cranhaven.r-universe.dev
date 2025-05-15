\name{symscal.SDA}
\alias{symscal.SDA}
\title{Multidimensional scaling for symbolic interval data -  SymScal algorithm}
\description{Multidimensional scaling for symbolic interval data -  symScal algorithm}
\usage{
symscal.SDA(x,d=2,calculateDist=FALSE)
}
\arguments{
\item{x}{symbolic interval data: a 3-dimensional table, first dimension represents object number, second dimension - variable number, and third dimension contains lower- and upper-bounds of intervals (Simple form of symbolic data table)}
\item{d}{Dimensionality of reduced space}
\item{calculateDist}{if TRUE x are treated as raw data and min-max dist matrix is calulated. See details}
}
\details{
SymScal, which was proposed by Groenen et. al. (2005), is an adaptation of well-known nonmetric multidimensional scaling for symbolic data. It is an iterative algorithm that uses STRESS objective function. This function is unnormalized. IScal, like Interscal and SymScal, requires interval-valued dissimilarity matrix. Such dissmilarity matrix can be obtained from symbolic data matrix (that contains only interval-valued variables), judgements obtained from experts, respondents. See Lechevallier Y. (2001) for details on calculating interval-valued distance.
See file \url{../doc/Symbolic_MDS.pdf} for further details
}
\value{
\item{xprim}{coordinates of rectangles}
\item{STRESSSym}{final STRESSSym value}
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} 

Department of Econometrics and Computer Science, University of Economics, Wroclaw, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Billard L., Diday E. (eds.) (2006), \emph{Symbolic Data Analysis, Conceptual Statistics and Data Mining}, John Wiley & Sons, Chichester.

Bock H.H., Diday E. (eds.) (2000), \emph{Analysis of symbolic data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Diday E., Noirhomme-Fraiture M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.

Groenen P.J.F, Winsberg S., Rodriguez O., Diday E. (2006), I-Scal: multidimensional scaling of interval dissimilarities, \emph{Computational Statistics and Data Analysis}, 51, pp. 360-378. Available at: \doi{10.1016/j.csda.2006.04.003}.
}
\seealso{
\code{\link{iscal.SDA}},\code{\link{interscal.SDA}}
}
\examples{
# Example will be available in next version of package, thank You for your patience :-)
}
\keyword{MDS}
\keyword{Multidimensional}
\keyword{scaling}
