\name{interscal.SDA}
\alias{interscal.SDA}
\title{Multidimensional scaling for symbolic interval data -  InterScal algorithm}
\description{Multidimensional scaling for symbolic interval data -  InterScal algorithm}
\usage{
interscal.SDA(x,d=2,calculateDist=FALSE)
}
\arguments{
\item{x}{symbolic interval data: a 3-dimensional table, first dimension represents object number, second dimension - variable number, and third dimension contains lower- and upper-bounds of intervals (Simple form of symbolic data table)}
\item{d}{Dimensionality of reduced space}
\item{calculateDist}{if TRUE x are treated as raw data and min-max dist matrix is calulated. See details}
}
\details{
Interscal is the adaptation of well-known classical multidimensional scaling for symbolic data. The input for Interscal is the interval-valued dissmilirarity matrix. Such dissmilarity matrix can be obtained from symbolic data matrix (that contains only interval-valued variables), judgements obtained from experts, respondents. See Lechevallier Y. (2001) for details on calculating interval-valued distance.
See file \url{../doc/Symbolic_MDS.pdf} for further details
}
\value{
\item{xprim}{coordinates of rectangles}
\item{stress.sym}{final STRESSSym value}
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl}
Marcin Pełka \email{marcin.pelka@ue.wroc.pl}

Department of Econometrics and Computer Science, University of Economics, Wroclaw, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Billard L., Diday E. (eds.) (2006), \emph{Symbolic Data Analysis, Conceptual Statistics and Data Mining}, John Wiley & Sons, Chichester.

Bock H.H., Diday E. (eds.) (2000), \emph{Analysis of symbolic data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Diday E., Noirhomme-Fraiture M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.

Lechevallier Y. (ed.), \emph{Scientific report for unsupervised classification, validation and cluster analysis}, Analysis System of Symbolic Official Data - Project Number IST-2000-25161, project report.
}
\seealso{
\code{\link{iscal.SDA}},\code{\link{symscal.SDA}}
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
#sda<-parse.SO("samochody")
#data<-sda$indivIC
#mds<-interscal.SDA(data, d=2, calculateDist=TRUE)
}
\keyword{MDS}
\keyword{Multidimensional}
\keyword{scaling}
