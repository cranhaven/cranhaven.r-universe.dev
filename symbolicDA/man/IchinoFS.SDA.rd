\name{IchinoFS.SDA}
\alias{IchinoFS.SDA}
\title{Ichino's feature selection method for symbolic data}
\description{Ichino's method for identifiyng non-noisy variables in symbolic data set}
\usage{
IchinoFS.SDA(table.Symbolic)
}
\arguments{
\item{table.Symbolic}{symbolic data table}
}
\value{
\item{plot}{plot of the gradient illustrating combinations of variables, in which the axis of ordinates (Y) represents the maximum number of mutual neighbor pairs and the axis of the abscissae (X) corresponds to the number of features (m)}
\item{combination}{the best combination of variables, i.e. the combination most differentiating the set of objects}
\item{maximum results}{step-by-step combinations of variables up to m variables}
\item{calculation results}{..............}
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl}, Justyna Wilk \email{justyna.wilk@ue.wroc.pl}
Department of Econometrics and Computer Science, Wroclaw University of Economics, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Ichino, M. (1994), \emph{Feature selection for symbolic data classification}, In: E. Diday, Y. Lechevallier, P.B. Schader, B. Burtschy (Eds.), New Approaches in Classification and data analysis, Springer-Verlag, pp. 423-429.

Bock, H.H., Diday, E. (eds.) (2000), \emph{Analysis of symbolic data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Diday, E., Noirhomme-Fraiture, M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.
}
\details{
See file \url{../doc/IchinoFSSDA_details.pdf} for further details
}
\seealso{
\code{\link{HINoV.SDA}}; \code{HINoV.Symbolic} in \code{clusterSim} library
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
#data("cars",package="symbolicDA")
#sdt<-cars
#ichino<-IchinoFS.SDA(sdt) 
#print(ichino) 
}
\keyword{cluster}
