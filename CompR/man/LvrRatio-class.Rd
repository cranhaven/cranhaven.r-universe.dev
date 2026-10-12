\name{LvrRatio-class}
\Rdversion{1.1}
\docType{class}
\alias{LvrRatio-class}
\alias{getSimu,LvrRatio-method}
\alias{getTest,LvrRatio-method}

\title{Class \code{"LvrRatio"}}
\description{
A class for Lilkelihood Ration Test results
}
\section{Objects from the Class}{
Objects can be created by \code{ResSimulLvrRatio().
}
}
\section{Slots}{
  \describe{
    \item{\code{Simu}:}{Object of class \code{"matrix"} with the number of classes under H0, Loglikelihoods under H0 and H1, difference between these Loglikelihoods.}
    \item{\code{Test}:}{Object of class \code{"matrix"} with the level and the associated quantile after performing Likelihood Ratio test.}
  }
}

\section{Methods}{
  \describe{
    \item{getSimu}{\code{signature(object = "LvrRatio")}}
    \item{getTest}{\code{signature(object = "LvrRatio")}}
	 }
}
\examples{
showClass("LvrRatio")
}