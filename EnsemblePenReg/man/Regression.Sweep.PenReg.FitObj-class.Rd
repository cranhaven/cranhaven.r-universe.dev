\name{Regression.Sweep.PenReg.FitObj-class}
\Rdversion{1.1}
\docType{class}
\alias{Regression.Sweep.PenReg.FitObj-class}

\title{Class \code{"Regression.Sweep.PenReg.FitObj"}}
\description{
Class containing the output of performing - or fitting - of PenReg sweep operation.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("Regression.Sweep.PenReg.FitObj", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{config}:}{Object of class \code{"Regression.Sweep.Config"} ~~ }
    \item{\code{est}:}{Object of class \code{"ANY"}, the estimation object needed for prediction.}
    \item{\code{pred}:}{Object of class \code{"matrix"}, matrix of predictions for training data. Column \code{n} corresponds to the prediction using PC's from 1 to \code{n}.}
  }
}
\section{Extends}{
Class \code{"Regression.Sweep.FitObj"}, directly.
}
\section{Methods}{
No methods defined with class "Regression.Sweep.PenReg.FitObj" in the signature.
}

\author{
Mansour T.A. Sharabiani, Alireza S. Mahani
}

\seealso{
\code{"Regression.Sweep.FitObj"}
}

\keyword{classes}
