\name{Regression.CV.Fit}
\alias{Regression.CV.Fit}
\alias{predict.Regression.CV.FitObj}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Cross-Validated Training and Prediction of Regression Base Learners
}
\description{
This function trains the base learner indicated in the configuration object in a cross-validation scheme using the \code{partition} argument. The cross-validated predictions are assembled and returned in the \code{pred} slot of the \code{\linkS4class{Regression.CV.FitObj}} object. Individual trained base learners are also assembled and returned in the return object, and used in the \code{predict} method.
}
\usage{
Regression.CV.Fit(regression.config, formula, data
  , partition, tmpfiles = NULL, print.level = 1)
\method{predict}{Regression.CV.FitObj}(object, newdata=NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{regression.config}{An object of class \code{\linkS4class{Regression.Config}} (must be a concrete implementation of the base class, such as \code{\linkS4class{KNN.Regression.Config}}).}
  \item{formula}{Formula object expressing response and covariates.}
  \item{data}{Data frame containing response and covariates.}
  \item{partition}{Data partition, typically the output of \code{generate.partition} function.}
  \item{tmpfiles}{List of temporary files to save the \code{est} field of the output \code{\linkS4class{Regression.FitObj}}.}
  \item{print.level}{Integer setting verbosity level of command-line output during training.}
  \item{object}{An object of class \code{Regression.FitObj}.}
  \item{newdata}{Data frame containing new observations.}
  \item{...}{Arguments passed to/from other methods.}
}
\value{
Function \code{Regression.CV.Fit} returns an object of class \code{\linkS4class{Regression.CV.FitObj}}. Function \code{predict.Regression.CV.FitObj} returns a numeric vector of length \code{nrow(newdata)}.
}

\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Regression.CV.FitObj}}
}
\examples{
data(servo)
myformula <- class~motor+screw+pgain+vgain
myconfig <- make.configs("knn", config.df=data.frame(kernel="rectangular", k=10))
perc.train <- 0.7
index.train <- sample(1:nrow(servo), size = round(perc.train*nrow(servo)))
data.train <- servo[index.train,]
data.predict <- servo[-index.train,]
mypartition <- generate.partition(nrow(data.train),nfold=3)
ret <- Regression.CV.Fit(myconfig[[1]], myformula, data.train, mypartition)
newpred <- predict(ret, data.predict)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
