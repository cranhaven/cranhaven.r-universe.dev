\name{trialDesign_binom_one-class}
\Rdversion{1.1}
\docType{class}
\alias{trialDesign_binom_one}
\alias{trialDesign_binom_one-class}
\alias{plot,trialDesign_binom_one,ANY-method}
\alias{print,trialDesign_binom_one-method}
\alias{show,trialDesign_binom_one-method}

\title{Class \code{"trialDesign_binom_one"}}
\description{
This is the s4 class for Binomial one endpoint designs with frequentist and Bayesian properties.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("trialDesign_binom_one", ...)}.
%%  ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{reviews}:}{Object of class \code{"numeric"}, numeric vector of the number of patients an interim analysis will occur at}
    \item{\code{success}:}{Object of class \code{"numeric"}, number of successed needed to stop for efficacy}
    \item{\code{failure}:}{Object of class \code{"numeric"}, number of failures needed to stop for futility}
    \item{\code{eta}, \code{zeta}:}{Object of class \code{"numeric"}, bayesiand properties of the design}
    \item{\code{alpha}, \code{power}, \code{exp.p0}, \code{exp.p1}:}{Object of class \code{"numeric"}, frequentist properties of the design including the expected sampel size under H0 and H1}
    \item{\code{p0}, \code{p1}:}{Object of class \code{"numeric"}, the probabilities used for H0 and H1 respectively}
  }
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "trialDesign_binom_one", y = "ANY")}: ... }
    \item{print}{\code{signature(x = "trialDesign_binom_one")}: ... }
    \item{show}{\code{signature(object = "trialDesign_binom_one")}: ... }
	 }
}
\examples{
showClass("trialDesign_binom_one")
}
\keyword{classes}
