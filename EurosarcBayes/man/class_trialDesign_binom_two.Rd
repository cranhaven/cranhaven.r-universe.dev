\name{trialDesign_binom_two-class}
\Rdversion{1.1}
\docType{class}
\alias{trialDesign_binom_two}
\alias{trialDesign_binom_two-class}
\alias{plot,trialDesign_binom_two,ANY-method}
\alias{print,trialDesign_binom_two-method}
\alias{show,trialDesign_binom_two-method}
\title{Class \code{"trialDesign_binom_two"}}
\description{
This is the s4 class for Binomial two endpoint designs with frequentist and Bayesian properties.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("trialDesign_binom_two", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{reviews}:}{Object of class \code{"numeric"}, a vector of the number of patients each interim analysis will occur at }
    \item{\code{data}:}{Object of class \code{"data.frame"}, exact simulation values for each scenario}
    \item{\code{cutpoints}:}{Object of class \code{"data.frame"}, the most extreme cutpoints when cause the trial to stop on their own (ignoring interaction with the other endpoint)}
    \item{\code{precision}:}{Object of class \code{"numeric"}, should be a vector of ones confirming probability is all accounted for}
    \item{\code{decision}:}{Object of class \code{"list"}, a list of matrices for the decisions to be made at each interim analysis}
    \item{\code{post.futility}, \code{post.efficacy}, \code{post.toxicity}, \code{post.no_toxicity}:}{Object of class \code{"numeric"}, The posterior probabilities of the design}
    \item{\code{graph}:}{Object of class \code{"list"}, an optional argument to pass to plot for the ploting of the first graph}
	\item{data}{Exact simulation values for each scenario}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "trialDesign_binom_two", y = "ANY")}: ... }
    \item{print}{\code{signature(x = "trialDesign_binom_two")}: ... }
    \item{show}{\code{signature(object = "trialDesign_binom_two")}: ... }
	 }
}
\examples{
showClass("trialDesign_binom_two")
}
\keyword{classes}
