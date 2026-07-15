\name{modelEval}
\alias{modelEval}

\title{ Statistical evaluation of predictions }
\description{
  Using predictions of given model produced by \code{\link{predict.CoreModel}} and correct labels,
  computes  some statistics evaluating the quality of the model.
}
\usage{
modelEval(model=NULL, correctClass, predictedClass, 
          predictedProb=NULL, costMatrix=NULL, 
          priorClProb = NULL, avgTrainPrediction = NULL, beta = 1) 
}

\arguments{
  \item{model}{ The model structure as returned by \code{\link{CoreModel}}, or NULL if some other predictions are evaluated. }
  \item{correctClass}{ A vector of correct class labels for classification problem and function values for regression problem. }
  \item{predictedClass}{ A vector of predicted class labels for classification problem and function values for regression problem. }
  \item{predictedProb}{ An optional matrix of predicted class probabilities for classification.}
  \item{costMatrix}{ Optional cost matrix can provide nonuniform costs for classification problems. }
  \item{priorClProb}{ If \code{model=NULL} a vector of prior class probabilities shall be provided in case of classification. }
  \item{avgTrainPrediction}{ If \code{model=NULL} mean of prediction values on training set shall be provided in case of regression. }
  \item{beta}{For two class problems \code{beta} controls the relative importance of precision and recall in F-measure.}
  }
\details{
   The function uses the \code{model} structure as returned by \code{\link{CoreModel}},
   \code{predictedClass} and \code{predictedProb} returned by 
   \code{\link{predict.CoreModel}}. Predicted values are compared with true values 
   and some statistics are computed  measuring the quality of predictions.
   In classification only one of the \code{predictedClass} and \code{predictedProb} can be NULL
   (one of them is computed from the other under assumption that class label is assigned to the most probable class). 
   Some of the returned statistics  are defined only for two class problems, for which the 
   confusion matrix specifying the number of instances of true/predicted class is
   defined as follows,
      \tabular{lcc}{
         true/predicted class \tab  positive           \tab negative     \cr
                   positive   \tab true positive (TP)  \tab false negative (FN) \cr
                   negative   \tab false positive (FP) \tab true negative (TN)
       }
  Optional cost matrix can provide nonuniform costs for classification problems. For regression
  problem this parameter is ignored. The costs can be different from the ones used for building the model 
   in \code{\link{CoreModel}} and prediction with the model in \code{\link{predict.CoreModel}}.
   If no costs are supplied, uniform costs are assumed. 
   The format of the matrix is \code{costMatrix(true_class, predicted_class)}. 
   By default a uniform costs are assumed, i.e.,  \code{costMatrix(i, i) = 0}, and \code{costMatrix(i, j) = 1}, 
   for \code{i} not equal to \code{j}. See the example below.
   
   If a non-CORElearn model is evaluated, one should set \code{model=NULL}, and  a vector of prior of class 
   probabilities \code{priorClProb} shall be provided in case of classification,
   and in case of regression \code{avgTrainPrediction} shall be the mean of prediction values 
   (estimated on a e.g., training set).
   
}
\value{
  For classification problem function returns list with the components
  \item{accuracy}{classification accuracy, for two class problems this would equal 
       \deqn{\rm{accuracy}=\frac{TP+TN}{TP+FN+FP+TN}}{accuracy= (TP+TN) / (TP+FN+FP+TN)} }
  \item{averageCost}{average classification cost}
  \item{informationScore}{information score statistics measuring information contents in the predicted probabilities}
  \item{AUC}{Area under the ROC curve}
  \item{predictionMatrix}{matrix of miss-classifications also confusion matrix}
  \item{sensitivity}{sensitivity for two class problems (also called accuracy of the positive class, i.e., acc+, or true positive rate),
       \deqn{rm{sensitivity} = \frac{TP}{TP+FN}}{sensitivity=TP/(TP+FN)}}
  \item{specificity}{specificity for two class problems (also called accuracy of the negative class, i.e., acc-, or true negative rate),
      \deqn{\rm{specificity} = \frac{TN}{TN+FP}}{specificity=TN/(TN+FP)}}
  \item{brierScore}{Brier score of predicted probabilities (the original Brier's definition which scores all the classes not only the correct one)}
  \item{kappa}{Cohen's kappa statistics measuring randomness of the predictions; for perfect predictions kappa=1, for completely random predictions kappa=0 }
  \item{precision}{precision for two class problems
     \deqn{\rm{precision} = \frac{TP}{TP+FP}}{precision=TP/(TP+FP)}}
  \item{recall}{recall for two class problems (the same as sensitivity) }
  \item{F-measure}{F-measure giving a weighted score of precision and recall for two class problems
       \deqn{F= \frac{(1+\beta^2)\cdot \rm{recall} \cdot \rm{precision}}{\beta^2 \cdot \rm{recall} + \rm{precision}}}{F = (1+beta^2)*recall*precision / (beta^2 * recall + precision)} }
  \item{G-mean}{geometric mean of positive and negative accuracy,
       \deqn{G=\sqrt{\rm{senstivity} \cdot \rm{specificity}} }{G=sqrt(sensitivity * specificity) }}
  \item{KS}{Kolmogorov-Smirnov statistics defined for binary classification problems, reports the distance between the probability distributions of positive class
  for positive and negative instances, see (Hand, 2005), value 0 means no separation, and value 1 means perfect separation,
   \deqn{KS = \max_t |TPR(t)-FPR(t)|}{KS = max_t |TPR(t)-FPR(t)|,} 
   see definitions of TPR and FPR below}
  \item{TPR}{true positive rate \eqn{TPR = \frac{TP}{TP+FN}}{TPR = TP / (TP+FN)} at maximal value of \code{KS} statistics}   
  \item{FPR}{false positive rate \eqn{FPR = \frac{FP}{FP+TN}}{FPR = FP / (FP+TN)} at maximal value of \code{KS} statistics}   
 For regression problem the returned list has components
 \item{MSE}{square root of Mean Squared Error}
 \item{RMSE}{Relative Mean Squared Error} 
 \item{MAE}{Mean Absolute Error}
 \item{RMAE}{Relative Mean Absolute Error} 
 }
  
\author{ Marko Robnik-Sikonja}

\seealso{
\code{\link{CORElearn}},
\code{\link{CoreModel}},
\code{\link{predict.CoreModel}}.
}
\references{ 
Igor Kononenko, Matjaz Kukar: \emph{Machine Learning and Data Mining: Introduction to Principles and Algorithms. }
Horwood, 2007

David J.Hand: Good practice in retail credit scorecard assesment. \emph{Journal of Operational Research Society}, 56:1109-1117, 2005)
} 
\examples{
# use iris data

# build random forests model with certain parameters
model <- CoreModel(Species ~ ., iris, model="rf", 
              selectionEstimator="MDL",minNodeWeightRF=5,
              rfNoTrees=100, maxThreads=1)

# prediction with node distribution
pred <- predict(model, iris, rfPredictClass=FALSE)

# Model evaluation
mEval <- modelEval(model, iris[["Species"]], pred$class, pred$prob)
print(mEval)

# use nonuniform cost matrix
noClasses <- length(levels(iris[["Species"]]))
costMatrix <- 1 - diag(noClasses)
costMatrix[3,1] <- costMatrix[3,2] <- 5 # assume class 3 is more valuable  
mEvalCost <- modelEval(model, iris[["Species"]], pred$class, pred$prob, 
                       costMatrix=costMatrix)
print(mEvalCost)

destroyModels(model) # clean up

}
\keyword{ models }
\keyword{ regression }
\keyword{ nonlinear }
\keyword{ tree }
\keyword{ multivariate }
\keyword{ loess }
\keyword{ classif }
