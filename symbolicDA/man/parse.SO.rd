\name{parse.SO}
\alias{parse.SO}
\title{Reading symbolic data table from ASSO-format XML file}
\description{Kohonen self organizing maps for sympbolic data with interval variables}
\usage{
parse.SO(file)
}
\arguments{
\item{file}{file name without xml extension} 
}
\details{
see \code{\link{symbolic.object}} for symbolic data table R structure representation
}
\value{
Symbolic data table parsed from XML file
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} 

Department of Econometrics and Computer Science, University of Economics, Wroclaw, Poland \url{http://keii.ue.wroc.pl/clusterSim/}
}
\references{
Billard L., Diday E. (eds.) (2006), \emph{Symbolic Data Analysis, Conceptual Statistics and Data Mining}, John Wiley & Sons, Chichester.

Bock H.H., Diday E. (eds.) (2000), \emph{Analysis of symbolic data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Diday E., Noirhomme-Fraiture M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.
}
\seealso{
\code{\link{save.SO}},\code{\link{generate.SO}}
}
\examples{
#cars<-parse.SO("cars")
}
\keyword{cluster}
