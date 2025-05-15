\name{ALL.Regression.Config-class}
\Rdversion{1.1}
\docType{class}
\alias{KNN.Regression.Config-class}
\alias{NNET.Regression.Config-class}
\alias{RF.Regression.Config-class}
\alias{SVM.Regression.Config-class}
\alias{GBM.Regression.Config-class}
\alias{PENREG.Regression.Config-class}
\alias{BART.Regression.Config-class}

\title{Classes \code{"KNN.Regression.Config"}, \code{"NNET.Regression.Config"}, \code{"RF.Regression.Config"}, \code{"SVM.Regression.Config"}, \code{"GBM.Regression.Config"}, \code{"PENREG.Regression.Config"}, \code{"BART.Regression.Config"}}
\description{
These base learner configuration objects contain tuning parameters needed for training base learner algorithms. Names are identical to those used in implementation packages. See documentation for those packages for detailed definitions.
}
\section{Objects from the Class}{
These objects are typically constructed via calls to \code{\link{make.configs}} and \code{\link{make.instances}}.
}
\section{Slots}{
  \describe{
    For \code{KNN.Regression.Config}:
    \item{\code{kernel}:}{Object of class \code{"character"}, defining the weighting function applied to neighbors as a function of distance from target point. Options include "rectangular", "epanechnikov", "triweight", and "gaussian".}
    \item{\code{k}:}{Object of class \code{"numeric"}, defining the number of nearest neighbors to include in prediction for each target point.}
    For \code{NNET.Regression.Config}:
    \item{\code{decay}:}{Object of class \code{"numeric"}, defining the weight decay parameter.}
    \item{\code{size}:}{Object of class \code{"numeric"}, defining the number of hidden-layer neurons.}
    \item{\code{maxit}:}{Object of class \code{"numeric"}, defining the maximum number of iterations in the training.}
    For \code{RF.Regression.Config}:
    \item{\code{ntree}:}{Object of class \code{"numeric"}, defining the number of trees in the random forest.}
    \item{\code{nodesize}:}{Object of class \code{"numeric"}, defining the minimum size of terminal nodes.}
    \item{\code{mtry.mult}:}{Object of class \code{"numeric"}, defining the multiplier of the default value for \code{mtry} parameter in the \code{randomForest} function call.}
    For \code{SVM.Regression.Config}:
    \item{\code{cost}:}{Object of class \code{"numeric"}, defining the cost of constraint violation.}
    \item{\code{epsilon}:}{Object of class \code{"numeric"}, the parameter of insensitive-loss function.}
    \item{\code{kernel}:}{Object of class \code{"character"}, the kernel used in SVM training and prediction. Options include "linear", "polynomial", "radial", and "sigmoid".}
    For \code{GBM.Regression.Config}:
    \item{\code{n.trees}:}{Object of class \code{"numeric"}, defining the number of trees to fit.}
    \item{\code{interaction.depth}:}{Object of class \code{"numeric"}, defining th maximum depth of variable interactions.}
    \item{\code{shrinkage}:}{Object of class \code{"numeric"}, defining the shrinkage parameter applied to each tree in expansion.}
    \item{\code{bag.fraction}:}{Object of class \code{"numeric"}, defining the fraction of training set observations randomly selected to propose the next tree in the expansion.}
    For \code{PENREG.Regression.Config}:
    \item{\code{alpha}:}{Object of class \code{"numeric"}, defining the mix of L1 and L2 penalty. Must be between 0.0 and 1.0.}
    \item{\code{lambda}:}{Object of class \code{"numeric"}, defining the shrinkage parameter. Must be non-negative.}
    For \code{BART.Regression.Config}:
    \item{\code{num_trees}:}{Object of class \code{"numeric"}, defining the number of trees to be grown in the sum-of-trees model. Must be a positive integer.}
    \item{\code{k}:}{Object of class \code{"numeric"}, controlling the degree of shrinkage and hence conservativeness of the fit. Must be positive.}
    \item{\code{q}:}{Object of class \code{"numeric"}, defining quantile of the prior on the error variance at which the data-based estimate is placed. Higher values of this parameter lead to a more aggressive fit.}
    \item{\code{nu}:}{Object of class \code{"numeric"}, defining degrees of freedom for the inverse chi-squared prior. Must be a positive integer.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{Regression.Config}"}, directly.
Class \code{"\linkS4class{BaseLearner.Config}"}, by class "Regression.Config", distance 2.
}
\section{Methods}{
  \describe{
    \item{BaseLearner.Fit}{\code{signature(object = "KNN.Regression.Config")}: ... }
    \item{BaseLearner.Fit}{\code{signature(object = "NNET.Regression.Config")}: ... }
    \item{BaseLearner.Fit}{\code{signature(object = "RF.Regression.Config")}: ... }
    \item{BaseLearner.Fit}{\code{signature(object = "SVM.Regression.Config")}: ... }
    \item{BaseLearner.Fit}{\code{signature(object = "GBM.Regression.Config")}: ... }
    \item{BaseLearner.Fit}{\code{signature(object = "PENREG.Regression.Config")}: ... }
    \item{BaseLearner.Fit}{\code{signature(object = "BART.Regression.Config")}: ... }
	 }
}

\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{make.configs}}, \code{\link{make.instances}}, \code{\link{make.configs.knn.regression}}, \code{\link{make.configs.nnet.regression}}, \code{\link{make.configs.rf.regression}}, \code{\link{make.configs.svm.regression}}, \code{\link{make.configs.gbm.regression}}, \code{"\linkS4class{Regression.Config}"}, \code{"\linkS4class{BaseLearner.Config}"}
}
\keyword{classes}
