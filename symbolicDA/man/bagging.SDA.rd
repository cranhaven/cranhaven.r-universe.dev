\name{bagging.SDA}
\alias{bagging.SDA}
\title{Bagging algorithm for optimal split based on decision tree for symbolic objects}
\description{Bagging algorithm for optimal split based on decision (classification) tree for symbolic objects}
\usage{
bagging.SDA(sdt,formula,testSet, mfinal=20,rf=FALSE,...) 
}
\arguments{
\item{sdt}{Symbolic data table}
\item{formula}{formula as in ln function}
\item{testSet}{a vector of integers indicating classes to which each objects are allocated in learnig set}
\item{mfinal}{number of partial models generated}
\item{rf}{random forest like drawing of variables in partial models}
\item{...}{arguments passed to decisionTree.SDA function}
}
\details{
The bagging, which stands for bootstrap aggregating, was introduced by Breiman in 1996. The diversity of classifiers in bagging is obtained by using bootstrapped replicas of the training data. Different training data subsets are randomly drawn with replacement from the entire training data set. Then each training data subset is used to train a decision tree (classifier). Individual classifiers are then combined by taking a simple majority vote of their decisions. For any given instance, the class chosen by most number of classifiers is the ensemble decision.
}
\value{
An object of class bagging.SDA, which is a list with the following components:

\item{predclass}{the class predicted by the ensemble classifier}
\item{confusion}{the confusion matrix for ensemble classifier}
\item{error}{the classification error}
\item{pred}{?}
\item{classfinal}{final class memberships}
}
\author{
Andrzej Dudek \email{andrzej.dudek@ue.wroc.pl} 
Marcin Pełka \email{marcin.pelka@ue.wroc.pl}

Department of Econometrics and Computer Science, University of Economics, Wroclaw, Poland \url{http://keii.ue.wroc.pl/symbolicDA/}
}
\references{
Billard L., Diday E. (eds.) (2006), \emph{Symbolic Data Analysis, Conceptual Statistics and Data Mining}, John Wiley & Sons, Chichester.

Bock H.H., Diday E. (eds.) (2000), \emph{Analysis of symbolic data. Explanatory methods for extracting statistical information from complex data}, Springer-Verlag, Berlin.

Breiman L. (1996), \emph{Bagging predictors}, Machine Learning, vol. 24, no. 2, pp. 123-140. Available at: \doi{10.1007/BF00058655}.

Diday E., Noirhomme-Fraiture M. (eds.) (2008), \emph{Symbolic Data Analysis with SODAS Software}, John Wiley & Sons, Chichester.
}
\seealso{
\code{\link{boosting.SDA}},\code{\link{random.forest.SDA}},\code{\link{decisionTree.SDA}}
}
\examples{
#Example will be available in next version of package, thank You for your patience :-)
}
\keyword{cluster}
