\name{BaseLearner.CV.FitObj-class}
\Rdversion{1.1}
\docType{class}
\alias{BaseLearner.CV.FitObj-class}
\alias{Regression.CV.FitObj-class}

\title{Classes \code{"BaseLearner.CV.FitObj"} and \code{"Regression.CV.FitObj"}}
\description{
Classes for containing base learner CV training output.
}
\section{Objects from the Class}{\code{"BaseLearner.CV.FitObj"} is a virtual class: No objects may be created from it. \code{"Regression.CV.FitObj"} is the output of \code{Regression.CV.Fit} function.}
\section{Slots}{
  \describe{
    \item{\code{fitobj.list}:}{Object of class \code{"list"}, contains a list of objects of class \code{BaseLearner.Fit}, one per partition fold.}
    \item{\code{partition}:}{Object of class \code{"OptionalInteger"}, representing how data must be split across folds during cross-validation. This is typically the output of \code{\link{generate.partition}} function.}
    \item{\code{filemethod}:}{Object of class \code{"logical"}, determining whether to save individual estimation objects to file or not.}
    In addition, \code{Regression.CV.FitObj} contains the following slot:
    \item{pred}{Object of class \code{"OptionalNumeric"}, containing the prediction from the CV fit object for training data. This slot is allowed to take on a "NULL" value to reduce excess memory use by large ensemble models.}
  }
}
\section{Methods}{
No methods defined with class "BaseLearner.CV.FitObj" in the signature.
}

\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\link{Regression.CV.Fit}
}

\keyword{classes}
