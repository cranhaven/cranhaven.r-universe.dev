\name{ecv.regression.baselearner.control}
\alias{ecv.regression.baselearner.control}
\alias{ecv.regression.integrator.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utility Functions for Configuring Regression Base Learners and Integrator in \pkg{EnsembleCV} Package
}
\description{
Function \code{ecv.regression.baselearner.control} sets up the base learners used in the \code{\link{ecv.regression}} call.
}
\usage{
ecv.regression.baselearner.control(
  baselearners = c("nnet", "rf", "svm", "gbm", "knn", "penreg")
  , baselearner.configs = make.configs(baselearners, type = "regression")
  , npart = 1, nfold = 5
)
ecv.regression.integrator.control(errfun=rmse.error, method=c("default"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{baselearners}{Names of base learners used. Currently, regression options available are Neural Network ("nnet"), Random Forest ("rf"), Support Vector Machine ("svm"), Gradient Boosting Machine ("gbm"), and K-Nearest Neighbors ("knn"), Penalized Rergession ("penreg") and Bayesian Additive Regression Trees ("bart"). The last learner is not included by default, due to significantly longer training time needed by it ("bart") compared to other learners.}
  \item{baselearner.configs}{List of base learner configurations. Default is to call \code{\link[EnsembleBase]{make.configs}} from package \pkg{EnsembleBase}.}
  \item{npart}{Number of partitions to train each base learner configuration in a CV scheme.}
  \item{nfold}{Number of folds within each data partition.}
  \item{errfun}{Error function used to compare performance of base learner configurations. Default is to use \code{\link[EnsembleBase]{rmse.error}} from package \pkg{EnsembleBase}.}
  \item{method}{Integrator method. Currently, only option is "default", which uses average error for each base learner configuration across repeated CV runs to chose the best configuration.}
}

\value{
Both functions return lists with same element names as function arguments.
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[EnsembleBase]{make.configs}}, \code{\link[EnsembleBase]{rmse.error}}
}

