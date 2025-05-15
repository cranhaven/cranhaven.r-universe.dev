\name{BaseLearner.Batch.FitObj-class}
\Rdversion{1.1}
\docType{class}
\alias{BaseLearner.Batch.FitObj-class}
\alias{Regression.Batch.FitObj-class}

\title{Classes \code{"BaseLearner.Batch.FitObj"} and \code{"Regression.Batch.FitObj"}}
\description{
Classes for containing base learner batch training output.
}
\section{Objects from the Class}{Class \code{"BaseLearner.Batch.FitObj"} is virtual; therefore No objects may be created from it. Class \code{"Regression.Batch.FitObj"} extends \code{"BaseLearner.Batch.FitObj"} and is the output of function \code{Regression.Batch.Fit}.}
\section{Slots}{
  \describe{
    \item{\code{fitobj.list}:}{Object of class \code{"list"}, containing the \code{\linkS4class{BaseLearner.FitObj}} outputs of lower-level \code{\link{BaseLearner.Fit}} function calls.}
    \item{\code{config.list}:}{Object of class \code{"list"}, containing the list of configuration objects for each base learner fit. This list is typically the output of \link{make.configs} function call.}
    \item{\code{filemethod}:}{Object of class \code{"logical"}, indicating whether file method is used for storing the estimation objects.}
    \item{\code{tmpfiles}:}{Object of class \code{"OptionalCharacter"}, containing (if applicable) the vector of filepaths used for storing estimation objects, if \code{filemethod==TRUE}.}
    For \code{Regression.Batch.FitObj} (in addition to above slots):
    \item{\code{pred}:}{Object of class \code{"matrix"}, with each column containing the predictions of one base learner.}
    \item{\code{y}:}{Object of class \code{"numeric"}, containing the response variable corresponding to the training set.}
  }
}
\section{Methods}{
No methods defined with class "BaseLearner.Batch.FitObj" in the signature.
}

\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{Regression.Batch.Fit}}
}
\examples{
showClass("BaseLearner.Batch.FitObj")
}
\keyword{classes}
