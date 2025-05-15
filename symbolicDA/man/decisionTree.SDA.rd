\name{decisionTree.SDA}
\alias{decisionTree.SDA}
\title{Decison tree for symbolic data}
\description{Optimal split based decision tree for symbolic objects}
\usage{
decisionTree.SDA(sdt,formula,testSet,treshMin=0.0001,treshW=-1e10,
tNodes=NULL,minSize=2,epsilon=1e-4,useEM=FALSE,
multiNominalType="ordinal",rf=FALSE,rf.size,objectSelection)
}
\arguments{
\item{sdt}{Symbolic data table}
\item{formula}{formula as in ln function}
\item{testSet}{a vector of integers indicating classes to which each objects are allocated in learnig set}
\item{treshMin}{parameter for tree creation algorithm}
\item{treshW}{parameter for tree creation algorithm}
\item{tNodes}{parameter for tree creation algorithm}
\item{minSize}{parameter for tree creation algorithm}
\item{epsilon}{parameter for tree creation algorithm}
\item{useEM}{use Expectation Optimalization algorithm for estinating conditional probabilities}
\item{multiNominalType}{"ordinal" - functione treats multi-nominal data as ordered or "nominal" functione treats multi-nomianal data as unordered (longer perfomance times)}
\item{rf}{if TRUE symbolic variables for tree creation are randomly chosen like in random forest algorithm}
\item{rf.size}{the number of variables chosen for tree creation if rf is true}
\item{objectSelection}{optional, vector with symbolic object numbers for tree creation }
}
\details{
For futher details see \url{../doc/decisionTree_SDA.pdf}
}

\value{
\item{nodes}{nodes in tree}
\item{nodeObjects}{contribution of each objects nodes in tree}
\item{conditionalProbab}{conditional probability of belonginess of nodes te classes}
\item{prediction}{predicted classes for objects from testSet}
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} 
Marcin Pelka \email{marcin.pelka@ue.wroc.pl}

Department of Econometrics and Computer Science, University of Economics, Wroclaw, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Billard L., Diday E. (eds.) (2006), \emph{Symbolic Data Analysis, Conceptual Statistics and Data Mining}, John Wiley & Sons, Chichester.

Bock H.H., Diday E. (eds.) (2000), \emph{Analysis of symbolic data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Diday E., Noirhomme-Fraiture M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.

}
\seealso{
\code{\link{bagging.SDA}},\code{\link{boosting.SDA}},\code{\link{random.forest.SDA}},\code{\link{draw.decisionTree.SDA}}
}
\examples{
# Example 1
# LONG RUNNING - UNCOMMENT TO RUN
# File samochody.xml needed in this example 
# can be found in /inst/xml library of package
#sda<-parse.SO("samochody")
#tree<-decisionTree.SDA(sda, "Typ_samochodu~.", testSet=1:33)
#summary(tree) # a very gerneral information
#tree  # summary information
}
\keyword{symbolic}
\concept{SDA}
