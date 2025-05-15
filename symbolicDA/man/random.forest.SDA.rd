\name{random.forest.SDA}
\alias{random.forest.SDA}
\title{Random forest algorithm for optimal split based decision tree for symbolic objects}
\description{Random forest algorithm for optimal split based decision tree for symbolic objects}
\usage{
random.forest.SDA(sdt,formula,testSet, mfinal = 100,...)
}
\arguments{
\item{sdt}{Symbolic data table}
\item{formula}{formula as in ln function}
\item{testSet}{a vector of integers indicating classes to which each objects are allocated in learnig set}
\item{mfinal}{number of partial models generated}
\item{...}{arguments passed to decisionTree.SDA function}
}
\details{
random.forest.SDA implements Breiman's random forest algorithm for classification of symbolic data set.
}
\value{
Section details goes here
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
}
\seealso{
\code{\link{bagging.SDA}},\code{\link{boosting.SDA}},\code{\link{decisionTree.SDA}}
}
\examples{
# Example will be available in next version of package, thank You for your patience :-)
}
\keyword{decision tree}
