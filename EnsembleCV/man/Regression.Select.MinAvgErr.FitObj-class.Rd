\name{Regression.Select.MinAvgErr.FitObj-class}
\Rdversion{1.1}
\docType{class}
\alias{Regression.Select.MinAvgErr.FitObj-class}

\title{Class \code{"Regression.Select.MinAvgErr.FitObj"}}
\description{
Class containing the fit object from the "MinAvgErr" specialization of the "Regression.Select.Fit" operation in \pkg{EnsembleBase} package.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Regression.Select.MinAvgErr.FitObj", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{config}:}{Object of class \code{"\link[EnsembleBase]{Regression.Select.Config-class}"}, containing the configuration supplied to the fit operation.}
    \item{\code{est}:}{Object of class \code{"ANY"}, containing the estimation object needed for prediction. This is a list with elements \code{config.opt} (optimal base learner configuration), \code{error.opt} (error associated with optimal configuration), and \code{errors} (vector of errors for all base learner configurations).}
    \item{\code{pred}:}{Object of class \code{"\link[EnsembleBase]{RegressionSelectPred-class}"}, containing the within-sample prediction, in this case the average prediction across all partitions. Note that this prediction is not used in the \code{\link{ecv.regression}} function as the ultimate training-set prediction. Instead, base learners trained on full training set (not CV style) are used for that purpose.}
  }
}
\section{Extends}{
Class \code{"\link[EnsembleBase]{Regression.Select.FitObj-class}"}, directly.
}
%\section{Methods}{
%No methods defined with class "Regression.Select.MinAvgErr.FitObj" in the signature.
%}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

\keyword{classes}
