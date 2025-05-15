\name{Regression.Sweep.PenReg.Config-class}
\Rdversion{1.1}
\docType{class}
\alias{Regression.Sweep.PenReg.Config-class}

\title{Class \code{"Regression.Sweep.PenReg.Config"}}
\description{
Configuration class for PenReg sweep operation
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Regression.Sweep.PenReg.Config", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{n}:}{Object of class \code{"OptionalNumeric"}, indicating, in this derived class, the maximum number of values of \code{lambda}'s to produce predictions for.}
    \item{\code{alpha}:}{Object of class \code{"numeric"}, indicating the relative strength of L1 (\code{alpha=1.0}) vs. L2 (\code{alpha=0.0}) penalty in penalized regression.}
    \item{\code{lambda}:}{Object of class \code{"numeric"}, containing the values of shrinkage parameter to generate predictions for. During CV sweep, this parameter is determined in the first fold and passed on to the remaining folds.}
  }
}
\section{Extends}{
Class \code{"Regression.Sweep.Config"}, directly.
}
\section{Methods}{
  \describe{
    \item{Regression.Sweep.Fit}{\code{signature(object = "Regression.Sweep.PenReg.Config")}: ... }
   }
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

\keyword{classes}
