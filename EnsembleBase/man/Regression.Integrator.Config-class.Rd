\name{Regression.Integrator.Config-class}
\Rdversion{1.1}
\docType{class}
\alias{Regression.Integrator.Config-class}
\alias{Regression.Select.Config-class}
\alias{Regression.Integrator.FitObj-class}
\alias{Regression.Select.FitObj-class}

\title{Classes \code{"Regression.Integrator.Config"}, \code{"Regression.Select.Config"}, \code{"Regression.Integrator.FitObj"}, \code{"Regression.Select.FitObj"}}
\description{
Virtual base classes to contain configuration and fit objects for integrator operations.
}
\section{Objects from the Class}{All virtual classes; therefore, no objects may be created from them.}
\section{Slots}{
  For config classes:
  \describe{
    \item{\code{errfun}:}{Object of class \code{"function"} ~~ }
    For FitObj classes:
      \item{\code{config}:}{Object of class \code{"Regression.Integrator.Config"} or \code{"Regression.Select.Config"} for the Integrator and Select classes.}
      \item{\code{est}:}{Object of class \code{ANY}, containing estimation objects for concrete extensions of the virtual classes.}
      \item{\code{pred}:}{Object of class \code{"numeric"}, containing the prediction of integrator operations.}
  }
}
\section{Methods}{
No methods defined with class "Regression.Integrator.Config" in the signature.
}

\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Regression.Integrator.Fit}}, \code{\link{Regression.Select.Fit}}
}

\keyword{classes}
