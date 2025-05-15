\name{Regression.Select.MinAvgErr.Config-class}
\Rdversion{1.1}
\docType{class}
\alias{Regression.Select.MinAvgErr.Config-class}

\title{Class \code{"Regression.Select.MinAvgErr.Config"}}
\description{
Configuration class for the "MinAvgErr" specialization of the "Regression.Select.Fit" operation in \pkg{EnsembleBase} package. This operation selects the base learner configuration with minimum average error across repeated cross-validation runs.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Regression.Select.MinAvgErr.Config", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{instance.list}:}{Object of class \code{\link[EnsembleBase]{Instance.List-class}}, containing a list of base learners to train.}
    \item{\code{errfun}:}{Object of class \code{"function"}, the error metric to use for ranking base learner performances. }
  }
}
\section{Extends}{
Class \code{"\link[EnsembleBase]{Regression.Select.Config-class}"}, directly.
}
\section{Methods}{
  \describe{
    \item{Regression.Select.Fit}{\code{signature(object = "Regression.Select.MinAvgErr.Config")}: ... }
   }
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

\keyword{classes}
