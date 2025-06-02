\name{classification_report}
\alias{classification_report}
\title{Calculates classification performance metrics.}
\usage{
classification_report(
y_true,
y_pred
)
}

\arguments{
\item{y_true}{A vector with the true labels.}

\item{y_pred}{A vector with the predicted labels.}
}

\value{
Returns a list with the following entries: 
\item{\code{metrics}}{A table with the precision, recall and f1-score for each class.}
\item{\code{confusion_matrix}}{The confusion matrix.}
\item{\code{accuracy}}{The accuracy.}
\item{\code{mutual_information}}{The mutual information between the true and the predicted classes.}
}

\description{
Calculates the confusion matrix and several performance metrics.
}

\examples{
#Example 1
X <- iris[,1:4]
y <- iris$Species
model <- copulaClassifier(X = X, y = y, copula = "frank",
                      distribution = "kernel", graph_model = "tree")
y_pred <- copulaPredict(X = X, model = model)
classification_report(y_true = y, y_pred = y_pred$class)

#Example 2
X <- iris[,1:4]
y <- iris$Species
model <- copulaClassifier(X = X, y = y, copula = c("frank","clayton"), 
                        distribution = "kernel", graph_model = "chain")
y_pred <- copulaPredict(X = X, model = model)
classification_report(y_true = y, y_pred = y_pred$class)

}

