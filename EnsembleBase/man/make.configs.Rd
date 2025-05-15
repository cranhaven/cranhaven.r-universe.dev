\name{make.configs}
\alias{make.configs}
\alias{make.configs.knn.regression}
\alias{make.configs.gbm.regression}
\alias{make.configs.svm.regression}
\alias{make.configs.rf.regression}
\alias{make.configs.nnet.regression}
\alias{make.configs.penreg.regression}
\alias{make.configs.bart.regression}
\alias{make.instances}
\alias{extract.baselearner.name}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Helper Functions for Manipulating Base Learner Configurations
}
\description{
Helper Functions for Manipulating Base Learner Configurations
}
\usage{
make.configs(baselearner=c("nnet","rf","svm","gbm","knn","penreg")
  , config.df, type = "regression")
make.configs.knn.regression(df=expand.grid(
  kernel=c("rectangular","epanechnikov","triweight","gaussian")
  , k=c(5,10,20,40)))
make.configs.gbm.regression(df=expand.grid(
  n.trees=c(1000,2000)
  , interaction.depth=c(3,4)
  , shrinkage=c(0.001,0.01,0.1,0.5)
  , bag.fraction=0.5))
make.configs.svm.regression(df=expand.grid(
  cost=c(0.1,0.5,1.0,5.0,10,50,75,100)
  , epsilon=c(0.1,0.25)
  , kernel="radial"))
make.configs.rf.regression(df=expand.grid(
  ntree=c(100,500)
  , mtry.mult=c(1,2)
  , nodesize=c(2,5,25,100)))
make.configs.nnet.regression(df=expand.grid(
  decay=c(1e-4,1e-2,1,100)
  , size=c(5,10,20,40)
  , maxit=2000))
make.configs.penreg.regression(df = expand.grid(
  alpha = 0.0
  , lambda = 10^(-8:+7)))
make.configs.bart.regression(df = rbind(cbind(expand.grid(
  num_trees = c(50, 100), k = c(2,3,4,5)), q = 0.9, nu = 3)
  , cbind(expand.grid(
  num_trees = c(50, 100), k = c(2,3,4,5)), q = 0.75, nu = 10)
  ))
make.instances(baselearner.configs, partitions)
extract.baselearner.name(config, type="regression")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{baselearner}{Name of base learner algorithm. Currently, seven base learners are included: 1) Neural Network (\code{nnet} using package \code{nnet}), 2) Random Forest (\code{rf} using package \code{randomForest}), 3) Support Vector Machine (\code{svm} using package \code{e1071}), 4) Gradient Boosting Machine (\code{gbm} using package \code{gbm}), 5) K-Nearest-Neighbors (\code{knn} using package \code{kknn}), 6) Penalized Regression (\code{penreg} using package \code{glmnet}), and Bayesian Additive Regression Trees (\code{bart}) using package \code{bartMachine}.}
  \item{df,config.df}{Data frame, with columns named after tuning parameters belonging to the base learner, and each row indicating a tuning-parameter combination to include in the configuration list.}
  \item{type}{Type of base learner. Currently, only "regression" is supported.}
  \item{baselearner.configs}{Base learner configuration list to use in generating instances.}
  \item{partitions}{A matrix whose columns define data partitions, usually the output of \link{generate.partitions}.}
  \item{config}{Base learner configuration object.}
}
\value{
The \code{make.configs} family of functions return a list of objects of various base learner \code{config} classes, such as \code{\linkS4class{KNN.Regression.Config}}. Function \code{make.instances} returns an object of class \code{\linkS4class{Instance.List}}. Function \code{extract.baselearner.name} returns a character object representing the name of the base learner associated with the passed-in config object. For example, for a \code{KNN.Regression.Config} object, we get back "KNN". This utility function can be used in printing base learner names based on class of a config object.
}

\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
