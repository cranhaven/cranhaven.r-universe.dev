\name{epenreg.baselearner.control}
\alias{epenreg.baselearner.control}
\alias{epenreg.integrator.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utility Functions for Configuring Regression Base Learners and Integrator in \pkg{EnsemblePenReg} Package
}
\description{
Function \code{epenreg.baselearner.control} sets up the base learners used in the \code{\link{epenreg}} call. Function \code{epenreg.integrator.control} sets up the PCR integrator.
}
\usage{
epenreg.baselearner.control(
  baselearners = c("nnet","rf","svm","gbm","knn")
  , baselearner.configs = make.configs(baselearners, type = "regression")
  , npart = 1, nfold = 5
)
epenreg.integrator.control(errfun=rmse.error, alpha=1.0
  , n=100, nfold=5, method=c("default")
)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{baselearners}{Names of base learners used. Currently, regression options available are Neural Network ("nnet"), Random Forest ("rf"), Support Vector Machine ("svm"), Gradient Boosting Machine ("gbm"), K-Nearest Neighbors ("knn"), Penalized Rergession ("penreg") and Bayesian Additive Regression Trees ("bart"). The last two learners are not include in the default list: "penreg" tends to produce highly correlated, and generally imprecise, predictions and skews the integration stage towards itself. "bart", on the other hand, is quite time- and memory-consuming to train, depsite generally having superior predictive performance. Users with more CPU and memory resources can add "bart" to achieve higher predictive accuracy.}
  \item{baselearner.configs}{List of base learner configurations. Default is to call \code{\link[EnsembleBase]{make.configs}} from package \pkg{EnsembleBase}.}
  \item{npart}{Number of partitions to train each base learner configuration in a CV scheme.}
  \item{nfold}{Number of folds within each data partition.}
  \item{errfun}{Error function used to compare performance of base learner configurations. Default is to use \code{\link[EnsembleBase]{rmse.error}} from package \pkg{EnsembleBase}.}
  \item{alpha}{Determining L1 vs L2 penalty. \code{alpha=1} leads to Lasso (L1) shrinkage, while \code{alpha=0.0} leads to Ridge (L2) shrinkage. See \code{glmnet} help files for more.}
  \item{n}{Suggested number of \code{lambda}'s in Penalized Regression. Actual number may be smaller than \code{n}, and is determined by the \code{glmnet} package.}
  \item{method}{Integrator method. Currently, only option is "default", where PenReg is performed on all base learner instances, and CV error is used to find the optimal shrinkage parameter. Same CV-based PenReg output is used to make final prediction.}
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

