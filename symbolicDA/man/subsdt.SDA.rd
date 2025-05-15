\name{subsdt.SDA}
\alias{subsdt.SDA}
\title{Subset of symbolic data table}
\description{This method creates symbolic data table containing only objects, whose indices are given in secong argument}
\usage{
subsdt.SDA(sdt,objectSelection)
}
\arguments{
\item{sdt}{Symbolic data table}
\item{objectSelection}{vector containing symbolic object numbers, default value - all objects from sdt}
}
\details{
see \code{\link{symbolic.object}} for symbolic data table R structure representation
}
\value{
Symbolic data table containing only objects, whose indices are given in secong argument. The result is of 'symbolic' class
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
\code{\link{generate.SO}},\code{\link{save.SO}},\code{\link{parse.SO}}
}
\examples{
# Example will be available in next version of package, thank You for your patience :-)
}
\keyword{cluster}
