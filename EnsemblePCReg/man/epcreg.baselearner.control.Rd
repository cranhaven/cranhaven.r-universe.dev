\name{epcreg.baselearner.control}
\alias{epcreg.baselearner.control}
\alias{epcreg.integrator.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Utility Functions for Configuring Regression Base Learners and Integrator in \pkg{EnsemblePCReg} Package
}
\description{
Function \code{epcreg.baselearner.control} sets up the base learners used in the \code{\link{epcreg}} call. Function \code{epcreg.integrator.control} sets up the PCR integrator.
}
\usage{
epcreg.baselearner.control(
  baselearners = c("nnet","rf","svm","gbm","knn")
  , baselearner.configs = make.configs(baselearners, type = "regression")
  , npart = 1, nfold = 5
)
epcreg.integrator.control(errfun=rmse.error, nfold=5, method=c("default"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{baselearners}{Names of base learners used. Currently, regression options available are Neural Network ("nnet"), Random Forest ("rf"), Support Vector Machine ("svm"), Gradient Boosting Machine ("gbm"), and K-Nearest Neighbors ("knn").}
  \item{baselearner.configs}{List of base learner configurations. Default is to call \code{\link[EnsembleBase]{make.configs}} from package \pkg{EnsembleBase}.}
  \item{npart}{Number of partitions to train each base learner configuration in a CV scheme.}
  \item{nfold}{Number of folds within each data partition.}
  \item{errfun}{Error function used to compare performance of base learner configurations. Default is to use \code{\link{rmse.error}} from package \pkg{EnsembleBase}.}
  \item{method}{Integrator method. Currently, only option is "default", where PCR is performed on all base learner instances, and CV error is used to find the optimal number of PC's. Same CV-based PCR output is used to make final prediction.}
}

\value{
Both functions return lists with same element names as function arguments.
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[EnsembleBase]{make.configs}}, \code{\link{rmse.error}}
}

