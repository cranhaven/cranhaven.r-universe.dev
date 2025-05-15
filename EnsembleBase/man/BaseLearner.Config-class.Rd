\name{BaseLearner.Config-class}
\Rdversion{1.1}
\docType{class}
\alias{BaseLearner.Config-class}
\alias{Regression.Config-class}

\title{Classes \code{"BaseLearner.Config"}, \code{"Regression.Config"}}
\description{
Base classes in the configuration class hierarchy.
}
\section{Objects from the Class}{\code{"BaseLearner.Config"} is a virtual Class: No objects may be created from it. \code{"Regression.Config"} is a base class for configuration classes of specific base learners, such as \code{\linkS4class{SVM.Regression.Config}}; therefore, there is typically no need to generate objects from this base class directly.}
\section{Extends}{
Class \code{"\linkS4class{Regression.Config}"} extends class \code{"\linkS4class{BaseLearner.Config}"}, directly.
}
\section{Methods}{
No methods defined with class "BaseLearner.Config" or "Regression.Config" in the signature.
}

\author{
Alireza S. Mahani, Mansour T.A. Sharabiani
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\linkS4class{KNN.Regression.Config}}, \code{\linkS4class{RF.Regression.Config}}, \code{\linkS4class{NNET.Regression.Config}}, \code{\linkS4class{GBM.Regression.Config}}, \code{\linkS4class{SVM.Regression.Config}}
}
\examples{
showClass("BaseLearner.Config")
}
\keyword{classes}
