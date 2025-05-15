\name{BaseLearner.Fit-methods}
\docType{methods}
\alias{BaseLearner.Fit-methods}
\alias{BaseLearner.Fit}
\alias{BaseLearner.Fit,GBM.Regression.Config-method}
\alias{BaseLearner.Fit,KNN.Regression.Config-method}
\alias{BaseLearner.Fit,NNET.Regression.Config-method}
\alias{BaseLearner.Fit,RF.Regression.Config-method}
\alias{BaseLearner.Fit,SVM.Regression.Config-method}
\alias{BaseLearner.Fit,PENREG.Regression.Config-method}
\alias{BaseLearner.Fit,BART.Regression.Config-method}
\title{Generic S4 Method for Fitting Base Learners}
\description{
 Each base learner must provide its concrete implementation of this generic method.
}

\usage{
BaseLearner.Fit(object, formula, data, tmpfile=NULL, print.level=1, ...)
}

\arguments{
  \item{object}{An object of class \code{\linkS4class{BaseLearner.Config}} (must be a concrete implementation such as \code{\linkS4class{KNN.Regression.Config}}).}
  \item{formula}{Formula object expressing response and covariates.}
  \item{data}{Data frame containing response and covariates.}
  \item{tmpfile}{Filepath to save the estimation object to. If \code{NULL}, estimation object will not be saved to a file.}
  \item{print.level}{Controlling verbosity level during fitting.}
  \item{...}{Arguments to be passed to/from other methods.}
}

\section{Methods}{
\describe{

\item{\code{signature(object = "GBM.Regression.Config")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "KNN.Regression.Config")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "NNET.Regression.Config")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "RF.Regression.Config")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(object = "SVM.Regression.Config")}}{
%%  ~~describe this method here~~
}
\item{\code{signature(object = "PENREG.Regression.Config")}}{
%%  ~~describe this method here~~
}
\item{\code{signature(object = "BART.Regression.Config")}}{
%%  ~~describe this method here~~
}
}}
\keyword{methods}
