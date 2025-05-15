\name{Regression.Batch.Fit}
\alias{Regression.Batch.Fit}
\alias{predict.Regression.Batch.FitObj}
\alias{plot.Regression.Batch.FitObj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Batch Training, Prediction and Diagnostics of Regression Base Learners
}
\description{
Batch Training, Prediction and Diagnostics of Regression Base Learners.
}
\usage{
Regression.Batch.Fit(config.list, formula, data, ncores = 1
  , filemethod = FALSE, print.level = 1)
\method{predict}{Regression.Batch.FitObj}(object, ..., ncores=1)
\method{plot}{Regression.Batch.FitObj}(x, errfun=rmse.error, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{config.list}{List of configuration objects for batch of base learners to be trained.}
  \item{formula}{Formula objects expressing response and covariates.}
  \item{data}{Data frame containing response and covariates.}
  \item{ncores}{Number of cores to use during parallel training.}
  \item{filemethod}{Boolean indicator of whether to save estimation objects to disk or not.}
  \item{print.level}{Determining level of command-line output verbosity during training.}
  \item{object}{Object of class \code{\linkS4class{Regression.Batch.FitObj}} to make predictions for.}
  \item{...}{Arguments to be passed from/to other functions.}
  \item{x}{Object of class \code{\linkS4class{Regression.Batch.FitObj}} to plot.}
  \item{errfun}{Error function to use for calculating errors plotted.}
}

\value{
Function \code{Regression.Batch.Fit} returns an object of class \code{\linkS4class{Regression.Batch.FitObj}}. Function \code{predict.Regression.Batch.FitObj} returns a matrix of predictions, each column corresponding to one base learner in the trained batch. Function \code{plot.Regression.Batch.FitObj} creates a plot of base learner errors over the training set, grouped by type of base learner (all configurations within a given base learner using the same symbol).
}

\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{Regression.Batch.FitObj}}
}
\examples{
data(servo)
myformula <- class~motor+screw+pgain+vgain
myconfigs <- make.configs("knn")
perc.train <- 0.7
index.train <- sample(1:nrow(servo), size = round(perc.train*nrow(servo)))
data.train <- servo[index.train,]
data.predict <- servo[-index.train,]
ret <- Regression.Batch.Fit(myconfigs, myformula, data.train, ncores=2)
newpred <- predict(ret, data.predict)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
