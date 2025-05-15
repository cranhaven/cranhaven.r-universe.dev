\name{Regression.Sweep.CV.FitObj-class}
\Rdversion{1.1}
\docType{class}
\alias{Regression.Sweep.CV.FitObj-class}

\title{Class \code{"Regression.Sweep.CV.FitObj"}}
\description{
Class containing output of \code{\link{Regression.Sweep.CV.Fit}} function.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Regression.Sweep.CV.FitObj", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{sweep.list}:}{Object of class \code{"list"}, list of length equal to number of folds in \code{partition}. Each element of list is contains the output of \code{Regression.Sweep.Fit} and has class \code{Regression.Sweep.FitObj}.}
    \item{\code{pred}:}{Object of class \code{"matrix"}, containing the matrix of predictions from this operation.}
    \item{\code{partition}:}{Object of class \code{"OptionalInteger"}, data partition used to perform CV sweep.}
  }
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

\seealso{
\code{\link{Regression.Sweep.CV.Fit}}
}

\keyword{classes}
