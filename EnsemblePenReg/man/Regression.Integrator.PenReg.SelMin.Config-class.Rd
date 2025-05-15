\name{Regression.Integrator.PenReg.SelMin.Config-class}
\Rdversion{1.1}
\docType{class}
\alias{Regression.Integrator.PenReg.SelMin.Config-class}

\title{Class \code{"Regression.Integrator.PenReg.SelMin.Config"}}
\description{
Configuration class for PenReg-base integration, where optimal shrinkage parameter is selected to minimize the cross-validation error of the integrator.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Regression.Integrator.PenReg.SelMin.Config", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{partition}:}{Object of class \code{"integer"}, data partition to use for cross-validation selection of optimal PC's in PCR integration. This can be the output of \code{\link[EnsembleBase]{generate.partition}}.}
    \item{\code{n}:}{Object of class \code{"OptionalNumeric"}, indicating, in this derived class, the maximum number of values of \code{lambda}'s to produce predictions for.}
    \item{\code{alpha}:}{Object of class \code{"numeric"}, indicating the relative strength of L1 (\code{alpha=1.0}) vs. L2 (\code{alpha=0.0}) penalty in penalized regression.}
    \item{\code{errfun}:}{Object of class \code{"function"}, error function to use for selecting best number of PC's.}
  }
}
\section{Extends}{
Class \code{"\link[EnsembleBase]{Regression.Integrator.Config-class}"}, directly.
}
\section{Methods}{
  \describe{
    \item{Regression.Integrator.Fit}{\code{signature(object = "Regression.Integrator.PenReg.SelMin.Config")}: ... }
   }
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[EnsembleBase]{generate.partition}}
}

\keyword{classes}
