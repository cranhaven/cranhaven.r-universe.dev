\name{RegressionSelectPred-class}
\Rdversion{1.1}
\docType{class}
\alias{RegressionSelectPred-class}

\title{Class \code{"RegressionSelectPred"}}
\description{
Union of classes \code{"NULL"}, \code{"numeric"} and \code{"matrix"} to hold prediction output of \code{Select} operations based on generic function \code{\link{Regression.Select.Fit}}. Class \code{NULL} is included to allow methods to save memory by not returning the prediction, espeically when a highe-level wrapper takes responsibility for holding a global copy of all prediction results. The "numeric" and "matrix" classes allow for a single predictor or multiple predictors to be produced by a \code{Select} operation.
}
\section{Objects from the Class}{A virtual Class: No objects may be created from it.}
\section{Methods}{
No methods defined with class "RegressionSelectPred" in the signature.
}

\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

\seealso{
\code{\link{Regression.Select.Fit}}
}

\keyword{classes}
