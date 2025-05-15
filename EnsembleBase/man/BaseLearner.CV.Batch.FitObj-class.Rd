\name{BaseLearner.CV.Batch.FitObj-class}
\Rdversion{1.1}
\docType{class}
\alias{BaseLearner.CV.Batch.FitObj-class}
\alias{Regression.CV.Batch.FitObj-class}

\title{Classes \code{"BaseLearner.CV.Batch.FitObj"} and \code{"Regression.CV.Batch.FitObj"}}
\description{
Classes for containing base learner batch CV training output.
}
\section{Objects from the Class}{\code{BaseLearner.CV.Batch.FitObj} is virtual Class; therefore, no objects may be created from it. Class \code{Regression.CV.Batch.FitObj} is the output of \code{\link{Regression.CV.Batch.Fit}} function.}
\section{Slots}{
  \describe{
    \item{\code{fitobj.list}:}{Object of class \code{"list"}, contains a list of objects of class \code{BaseLearner.CV.FitObj}, one per base learner instance.}
    \item{\code{instance.list}:}{Object of class \code{"Instance.List"}, the list of base learner instance passed to the function \code{Regression.CV.Batch.Fit} that produces the object.}
    \item{\code{filemethod}:}{Object of class \code{"logical"}, the boolean flag indicating whether estimation objects are saved to files or help in memory.}
    \item{\code{tmpfiles}:}{Object of class \code{"OptionalCharacter"}, list of temporary files used for storing estimation objects (if any).}
    \item{\code{tmpfiles.index.list}:}{Object of class \code{"list"}, with elements \code{start} and \code{end}, holding the start and end indexes into the tempfiles vector for each of the base learner instances trained.}
    \item{\code{tvec}:}{Execution times for each base learner in the batch. Note: Currently implemented for serial execution only.}
    In addition, \code{Regression.CV.Batch.FitObj} contains the following slots:
    \item{\code{pred}:}{Object of class \code{"matrix"}, with each column being the training-set prediction for one of base learner instances.}
    \item{\code{y}:}{Object of class \code{"OptionalNumeric"}, holding the response variable values for training set. This slot can be \code{NULL} for memory efficiency purposes.}
  }
}
\section{Methods}{
No methods defined with class "BaseLearner.CV.Batch.FitObj" in the signature.
}

\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{Regression.CV.Batch.Fit}
}

\keyword{classes}
