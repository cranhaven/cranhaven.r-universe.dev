\name{save.SO}
\alias{save.SO}
\title{saves symbolic data table of 'symbolic' class to xml file}
\description{saves symbolic data table of 'symbolic' class to xml file (ASSO format)}
\usage{
save.SO(sdt,file)
}
\arguments{
\item{sdt}{Symbolic data table}
\item{file}{file name with extension}
}
\details{
see \code{\link{symbolic.object}} for symbolic data table R structure representation
}
\value{
No value returned
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
\code{\link{generate.SO}},\code{\link{subsdt.SDA}},\code{\link{parse.SO}}
}
\examples{
#data("cars",package="symbolicDA")
#save.SO(cars,file="cars_backup.xml")
}
\keyword{cluster}
