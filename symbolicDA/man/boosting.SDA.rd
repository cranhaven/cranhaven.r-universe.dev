\name{boosting.SDA}
\alias{boosting.SDA}
\title{Boosting algorithm for optimal split based decision tree for symbolic objects}
\description{Boosting algorithm for optimal split based decision tree for symbolic objects, "symbolic" version of adabag.M1 algorithm}
\usage{
boosting.SDA(sdt,formula,testSet, mfinal = 20,...) 
}
\arguments{
\item{sdt}{Symbolic data table}
\item{formula}{formula as in ln function}
\item{testSet}{a vector of integers indicating classes to which each objects are allocated in learnig set}
\item{mfinal}{number of partial models generated}
\item{...}{arguments passed to decisionTree.SDA function}
}
\details{
Boosting, similar to bagging, also creates an ensemble of classifiers by resampling the data. The results are then combined by majority voting. Resampling in boosting provides the most informative training data for each consecutive classifier. In each iteration of boosting three weak classifiers are created: the first classifier C1 is trained with a random subset of the training data. The training data subset for the next classifier C2 is chosen as the most informative subset, given C1.C2 is trained on a training data only half of wich is correctly classified by C1 and the other half is misclassified. The third classifier C3 is trained with instances on which C1 and C2 disagree. Then the three classifiers are combined through a three-way majority vote.
}
\value{
\item{formula}{a symbolic description of the model that was used}
\item{trees}{trees built whlie making the ensemble}
\item{weights}{weights for each object from test set}
\item{votes}{final consensus clustering}
\item{class}{predicted class memberships}
\item{error}{error rate of the ensemble clustering}
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
\code{\link{bagging.SDA}},\code{\link{random.forest.SDA}},\code{\link{decisionTree.SDA}}
}
\examples{
#Example will be available in next version of package, thank You for your patience :-)
}
\keyword{cluster}
