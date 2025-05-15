\name{draw.decisionTree.SDA}
\alias{draw.decisionTree.SDA}
\title{Draws optimal split based decision tree for symbolic objects}
\description{Draws optimal split based decision tree for symbolic objects}
\usage{
draw.decisionTree.SDA(decisionTree.SDA,boxWidth=1,boxHeight=3)
}
\arguments{
\item{decisionTree.SDA}{optimal split based decision tree for symbolic objects (result of \code{decisionTree.SDA} function)}
\item{boxWidth}{witdh of single box in drawing}
\item{boxHeight}{height of single box in drawing}
}
\details{
Draws optimal split based decision (classification) tree for symbolic objects.
}
\value{
A draw of optimal split based decision (classification) tree for symbolic objects.
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
\code{\link{decisionTree.SDA}}
}
\examples{
# LONG RUNNING - UNCOMMENT TO RUN
# Files samochody.xml and wave.xml needed in this example 
# can be found in /inst/xml library of package

# Example 1
#sda<-parse.SO("samochody")
#tree<-decisionTree.SDA(sda, "Typ_samochodu~.", testSet=26:33)
#draw.decisionTree.SDA(tree,boxWidth=1,boxHeight=3)

# Example 2
#sda<-parse.SO("wave")
#tree<-decisionTree.SDA(sda, "WaveForm~.", testSet=1:30)
#draw.decisionTree.SDA(tree,boxWidth=2,boxHeight=3)
}
\keyword{decision tree}
