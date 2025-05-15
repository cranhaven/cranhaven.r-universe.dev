\name{kernel.SDA}
\alias{kernel.SDA}
\title{Kernel discriminant analysis for symbolic data}
\description{Kernel discriminant analysis for symbolic data}
\usage{
kernel.SDA(sdt,formula,testSet,h,...)
}
\arguments{
\item{sdt}{symbolic data table}
\item{formula}{a formula, as in the \code{lm} function}
\item{testSet}{vector with numbers objects ij test set}
\item{h}{kernel bandwith size}
\item{...}{argumets passed to dist_SDA functon}
}
\details{
Kernel discriminant analysis for symbolic data is based on the intensity estimatior (that is based on dissimiliarity measure for symbolic data) due to the fact that classical well-known density estimator can not be applied. Density estimator can not be applied due to the fact that symbolic objects are not object of euclidean space and the integral operator for symbolic data is not applicable.


For futher details see \url{../doc/Kernel_SDA.pdf.pdf}

}
\value{
vector of class belongines of each object in test set
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
\code{\link{dist_SDA}}
}
\examples{
# Example 1
# LONG RUNNING - UNCOMMENT TO RUN
#sda<-parse.SO("samochody")
#model<-kernel.SDA(sda, "Typ_samochodu~.", testSet=6:16, h=0.75)
#print(model)

}
\keyword{cluster}
