\name{Regression.Integrator.PCR.SelMin.FitObj-class}
\Rdversion{1.1}
\docType{class}
\alias{Regression.Integrator.PCR.SelMin.FitObj-class}

\title{Class \code{"Regression.Integrator.PCR.SelMin.FitObj"}}
\description{
Class containing the output of fitting a PCR-based integrator with CV-error minimization method for selecting the number of PC's.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Regression.Integrator.PCR.SelMin.FitObj", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{config}:}{Object of class \code{"Regression.Integrator.Config"}, containing the error function and the partition to use for training the PCR integrator.}
    \item{\code{est}:}{Object of class \code{"ANY"}, estimation object that is used for prediction.}
    \item{\code{pred}:}{Object of class \code{"numeric"}, prediction for training set.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{Regression.Integrator.FitObj}"}, directly.
}
\section{Methods}{
No methods defined with class "Regression.Integrator.PCR.SelMin.FitObj" in the signature.
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{"\linkS4class{Regression.Integrator.FitObj}"}
}

\keyword{classes}
