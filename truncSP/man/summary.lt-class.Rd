\name{summary.lt-class}
\Rdversion{1.1}
\docType{class}
\alias{summary.lt-class}

\title{Class \code{"summary.lt"}}
\description{
 Documentation on S4 class \code{"summary.lt"}
}
\section{Objects from the Class}{
Objects from the class are usually obtained by a calling \code{summary} on an object of class \code{"\linkS4class{lt}"}.
}
\section{Slots}{
  \describe{
     \item{\code{level}:}{Object of class \code{"numeric"} the level of confidence for confidence intervals }
    \item{\code{confint}:}{Object of class \code{"matrix"} confidence intervals for regression coefficients }
    \item{\code{bootconfint}:}{Object of class \code{"matrix"} bootstrap confidence intervals for regression coefficients }
    \item{\code{call}:}{Object of class \code{"call"} the function call }
    \item{\code{coefficients}:}{Object of class \code{"matrix"} the estimated coefficients from fitting a model for truncated regression using the Quadratic Mode Estimator (QME) }
    \item{\code{startcoef}:}{Object of class \code{"matrix"} the starting coefficients used when fitting the model }
    \item{\code{cvalues}:}{Object of class \code{"data.frame"} containing information about the threshold values used }
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
\section{Extends}{
Class \code{"\linkS4class{lt}"}, directly.
}
\section{Methods}{
  \describe{
    \item{print}{\code{signature(x = "summary.lt")}: print method }
   }
}
\author{
 Anita Lindmark and Maria Karlsson
}
\seealso{
Function \code{\link{lt}} and class \code{"\linkS4class{lt}"}
}
\examples{
showClass("summary.lt")
}
\keyword{classes}
