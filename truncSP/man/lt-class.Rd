\name{lt-class}
\Rdversion{1.1}
\docType{class}
\alias{lt-class}

\title{Class \code{"lt"}}
\description{
Documentation on S4 class \code{"lt"}.
}
\section{Objects from the Class}{
Objects from the class are usually obtained by a call to the function \code{\link{lt}}.
 
}
\section{Slots}{
\describe{
  \item{\code{call}:}{Object of class \code{"call"} the function call }
    \item{\code{coefficients}:}{Object of class \code{"matrix"} the estimated coefficients from fitting a model for truncated regression using the Quadratic Mode Estimator (QME) }
    \item{\code{startcoef}:}{Object of class \code{"matrix"} the starting coefficients used when fitting the model }
    \item{\code{cvalues}:}{Object of class \code{"data.frame"} containing information about the thresholds used }
   \item{\code{value}:}{Object of class \code{"numeric"} the value of the objective function corresponding to \code{coefficients} }
    \item{\code{counts}:}{Object of class \code{"integer"} number of iterations until convergence}
    \item{\code{convergence}:}{Object of class \code{"integer"} indicating whether convergence was achieved }
    \item{\code{message}:}{Object of class \code{"character"} a character string giving any additional information returned by the optimizer }
    \item{\code{residuals}:}{Object of class \code{"matrix"} the residuals of the model }
    \item{\code{fitted.values}:}{Object of class \code{"matrix"} the fitted values }
    \item{\code{df.residual}:}{Object of class \code{"integer"} the residual degrees of freedom }
    \item{\code{covariance}:}{Object of class \code{"matrix"} the estimated covariance matrix }
    \item{\code{bootrepl}:}{Object of class \code{"matrix"} bootstrap replicates used to estimate the covariance matrix }
  }
}
\section{Methods}{
  \describe{
   \item{coef}{\code{signature(object = "lt")}: extracts the coefficients of the model fitted using \code{\link{lt}}}
     \item{fitted}{\code{signature(object = "lt")}: extracts the fitted values of the model fitted using \code{\link{lt}} }
    \item{print}{\code{signature(x = "lt")}: print method }
    \item{residuals}{\code{signature(object = "lt")}: extracts the residuals of the model fitted using \code{\link{lt}} }
    \item{summary}{\code{signature(object = "lt")}: summary method }
    \item{vcov}{\code{signature(object = "lt")}: extracts the covariance matrix of the model fitted using \code{\link{lt}} }
	 }
}

\author{
Anita Lindmark and Maria Karlsson
}


\seealso{
Function \code{\link{lt}} and class \code{"\linkS4class{summary.lt}"}
}

\examples{
showClass("lt")
}
\keyword{classes}
