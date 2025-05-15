\name{generate.SO}
\alias{generate.SO}
\title{generation of artifficial symbolic data table with given cluster structure}
\description{generation of artifficial symbolic data table with given cluster structure}
\usage{
generate.SO(numObjects,numClusters,numIntervalVariables,numMultivaluedVariables)
}
\arguments{
\item{numObjects}{number of objects in each cluster}
\item{numClusters}{number of objects}
\item{numIntervalVariables}{Number of symbolic interval variables in generated data table}
\item{numMultivaluedVariables}{Number of symbolic multi-valued variables in generated data table}
}
\value{
\item{data}{symbolic data table with given cluster structure}
\item{clusters}{vector with cluster numbers for each object}
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} 

Department of Econometrics and Computer Science, University of Economics, Wroclaw, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Billard L., Diday E. (eds.) (2006), \emph{Symbolic Data Analysis, Conceptual Statistics and Data Mining}, John Wiley & Sons, Chichester.

Bock H.H., Diday E. (eds.) (2000), \emph{Analysis of symbolic data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Diday E., Noirhomme-Fraiture M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.

User manual for SODAS 2 software, Software Report, Analysis System of Symbolic Official Data, Project no. IST-2000-25161, Paris. 
}
\seealso{
see \code{\link{symbolic.object}} for symbolic data table R structure representation
}
\examples{
# Example will be available in next version of package, thank You for your patience :-)
}
\keyword{symbolic}
\concept{SDA}
