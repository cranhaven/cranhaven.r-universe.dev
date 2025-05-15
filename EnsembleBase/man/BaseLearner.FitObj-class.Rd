\name{BaseLearner.FitObj-class}
\Rdversion{1.1}
\docType{class}
\alias{BaseLearner.FitObj-class}
\alias{Regression.FitObj-class}

\title{Classes \code{"BaseLearner.FitObj"} and \code{"Regression.FitObj"}}
\description{
Base class templates for containing base learner training output.
}
\section{Objects from the Class}{\code{"BaseLearner.FitObj"} is a virtual class: No objects may be created from it. \code{"Regression.FitObj"} is a base class for objects representing trained models for individual base learners.}
\section{Slots}{
  \describe{
    \item{\code{config}:}{Object of class \code{"BaseLearner.Config"}; often one of the derived configuration classes belonging to a particular base learner.}
    For \code{Regression.FitObj}, we have the following additional fields:
    \item{\code{est}:}{Object of class \code{"RegressionEstObj"}, typically containing the low-level list coming out of the training algorithm. If \code{filemethod=TRUE} during the fit, this object will be of class "character", containing the filepath to where the estimation object is stored.}
    \item{\code{pred}:}{Object of class \code{"OptionalNumeric"}, fitted values of the model for the training data. It is allowed to be "NULL" in order to reduce memory footrpint during cross-validated ensemble methods.}
  }
}
\section{Methods}{
No methods defined with these classes in their signature.
}
\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{"\linkS4class{KNN.Regression.FitObj}"}, \code{"\linkS4class{RF.Regression.FitObj}"}, \code{"\linkS4class{SVM.Regression.FitObj}"}, \code{"\linkS4class{GBM.Regression.FitObj}"}, \code{"\linkS4class{NNET.Regression.FitObj}"}
}

\keyword{classes}
