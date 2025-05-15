\name{Regression.Integrator.PenReg.SelMin.FitObj-class}
\Rdversion{1.1}
\docType{class}
\alias{Regression.Integrator.PenReg.SelMin.FitObj-class}

\title{Class \code{"Regression.Integrator.PenReg.SelMin.FitObj"}}
\description{
Class containing the output of fitting a PenReg-based integrator with CV-error minimization method for selecting the optimal shrinkage parameter.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Regression.Integrator.PenReg.SelMin.FitObj", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{config}:}{Object of class \code{"Regression.Integrator.Config"}, containing the error function and the partition to use for training the PenReg integrator.}
    \item{\code{est}:}{Object of class \code{"ANY"}, estimation object that is used for prediction.}
    \item{\code{pred}:}{Object of class \code{"numeric"}, prediction for training set.}
  }
}
\section{Extends}{
Class \code{"\link[EnsembleBase]{Regression.Integrator.FitObj-class}"}, directly.
}
\section{Methods}{
No methods defined with class "Regression.Integrator.PenReg.SelMin.FitObj" in the signature.
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{"\link[EnsembleBase]{Regression.Integrator.FitObj-class}"}
}

\keyword{classes}
