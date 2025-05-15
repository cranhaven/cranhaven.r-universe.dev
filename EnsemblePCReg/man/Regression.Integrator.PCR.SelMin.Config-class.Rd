\name{Regression.Integrator.PCR.SelMin.Config-class}
\Rdversion{1.1}
\docType{class}
\alias{Regression.Integrator.PCR.SelMin.Config-class}

\title{Class \code{"Regression.Integrator.PCR.SelMin.Config"}}
\description{
Configuration class for PCR-base integration, where the number of PC's is selected to minimize the cross-validation error of the integrator.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Regression.Integrator.PCR.SelMin.Config", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{partition}:}{Object of class \code{"integer"}, data partition to use for cross-validation selection of optimal PC's in PCR integration. This can be the output of \code{\link[EnsembleBase]{generate.partition}}.}
    \item{\code{errfun}:}{Object of class \code{"function"}, error function to use for selecting best number of PC's.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{Regression.Integrator.Config}"}, directly.
}
\section{Methods}{
  \describe{
    \item{Regression.Integrator.Fit}{\code{signature(object = "Regression.Integrator.PCR.SelMin.Config")}: ... }
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
