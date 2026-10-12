\name{BradleyEstim-class}
\Rdversion{1.1}
\docType{class}
\alias{BradleyEstim-class}
\alias{getIc,BradleyEstim-method}
\alias{getLambda,BradleyEstim-method}
\alias{getLvr,BradleyEstim-method}
\alias{getLvriter,BradleyEstim-method}
\alias{getPi,BradleyEstim-method}
\alias{getRestestglob,BradleyEstim-method}
\alias{getRestestprod,BradleyEstim-method}
\alias{getVarcov,BradleyEstim-method}
\alias{getZh,BradleyEstim-method}
\alias{show,BradleyEstim-method}

\title{Class \code{"BradleyEstim"}}
\description{
A class for Bradley's scores estimation results
}
\section{Objects from the Class}{
Objects can be created by the function \code{EstimBradley()}.
}
\section{Slots}{
  \describe{
    \item{\code{Lvriter}:}{Object of class \code{"matrix"} corresponding to the number of iterations of the EM algorithm, LogLikelihoods at the previous step and the current step, and the differences between these 2 LogLikelihoods }
    \item{\code{Lvr}:}{Object of class \code{"numeric"} final value of the LogLikelihood}
    \item{\code{Lambda}:}{Object of class \code{"matrix"} weights of the different classes }
    \item{\code{Pi}:}{Object of class \code{"list"} Bradley's scores for each class and each criteria }
    \item{\code{Zh}:}{Object of class \code{"matrix"} with the posterior probabilities for each individual to belong to the different classes and the class with the higher probability }
    \item{\code{Ic}:}{Object of class \code{"matrix"} value of the different Information criterion (AIC, BIC, CAIC) }
    \item{\code{Restestglob}:}{Object of class \code{"list"} result of testing the whole Bradley's scores equality for each class and each criteria}
    \item{\code{Restestprod}:}{Object of class \code{"list"} result of multiple comparison tests for Bradley's scores in each class and for each criteria }
    \item{\code{Varcov}:}{Object of class \code{"list"} of covaraince matrices of Bradley's scores in each class and for each criteria }
  }
}
\section{Methods}{
  \describe{
    \item{getIc}{\code{signature(object = "BradleyEstim")} }
    \item{getLambda}{\code{signature(object = "BradleyEstim")}}
    \item{getLvr}{\code{signature(object = "BradleyEstim")}}
    \item{getLvriter}{\code{signature(object = "BradleyEstim")}}
    \item{getPi}{\code{signature(object = "BradleyEstim")}}
    \item{getRestestglob}{\code{signature(object = "BradleyEstim")}}
    \item{getRestestprod}{\code{signature(object = "BradleyEstim")}}
    \item{getVarcov}{\code{signature(object = "BradleyEstim")}}
    \item{getZh}{\code{signature(object = "BradleyEstim")}}
    \item{show}{\code{signature(object = "BradleyEstim")}}
	 }
}
\examples{
data(ResCocktail1)
show(ResCocktail1)
}

