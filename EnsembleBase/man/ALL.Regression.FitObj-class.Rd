\name{ALL.Regression.FitObj-class}
\Rdversion{1.1}
\docType{class}
\alias{KNN.Regression.FitObj-class}
\alias{NNET.Regression.FitObj-class}
\alias{RF.Regression.FitObj-class}
\alias{SVM.Regression.FitObj-class}
\alias{GBM.Regression.FitObj-class}
\alias{PENREG.Regression.FitObj-class}
\alias{BART.Regression.FitObj-class}

\title{Classes \code{"KNN.Regression.FitObj"}, \code{"NNET.Regression.FitObj"}, \code{"RF.Regression.FitObj"}, \code{"SVM.Regression.FitObj"}, \code{"GBM.Regression.FitObj"}, \code{"PENREG.Regression.FitObj"}, \code{"BART.Regression.FitObj"}}
\description{
Objects returned by base learner training functions.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("KNN.Regression.FitObj", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  All classes inherit slots \code{config}, \code{est}, and \code{pred} from \code{"\linkS4class{Regression.FitObj}"}. Some base learners may have additional slots as described below.
  
  \describe{
    For \code{KNN.Regression.FitObj}:
    \item{\code{formula}:}{Object of class \code{"formula"}, copy of same argument from training call \code{\link{BaseLearner.Fit}}.}
    \item{\code{data}:}{Object of class \code{"data.frame"}, copy of same argument from training call \code{\link{BaseLearner.Fit}}.}
    For \code{NNET.Regession.FitObj}:
    \item{\code{y.range}:}{Object of class \code{"numeric"}, range of response variable in training data. This is used for scaling of data during prediction so that it falls between 0 and 1 for regression tasks.}
    \item{\code{y.min}:}{Object of class \code{"numeric"}, minimum of response variable in training data. This is used for scaling of data during prediction so that it falls between 0 and 1 for regression tasks.}
    For \code{PENREG.Regression.FitObj} and \code{BART.Regression.FitObj}:
    \item{\code{mm}:}{A list containing data structures needed for creating the matrix of predictors and the response variable from the formula and data frame.}
  }
}
\section{Extends}{
Class \code{"\linkS4class{Regression.FitObj}"}, directly.
Class \code{"\linkS4class{BaseLearner.FitObj}"}, by class "Regression.FitObj", distance 2.
}
\section{Methods}{
None.
}
\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{"\linkS4class{BaseLearner.FitObj}"}, \code{"\linkS4class{Regression.FitObj}"}
}
\examples{
showClass("KNN.Regression.FitObj")
}
\keyword{classes}
