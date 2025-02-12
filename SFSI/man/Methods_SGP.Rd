
\name{Regression coefficients, predicted values and summary in SGP problems}
\alias{summary.SGP}
\alias{coef.SGP}
\alias{predict.SGP}
\title{SGP methods}
\usage{
\method{coef}{SGP}(object, ...)

\method{predict}{SGP}(object, ...)

\method{summary}{SGP}(object, ...)

}
\arguments{
\item{object}{An object of the class 'SGP'}

\item{...}{Other arguments to be passed to \code{coef} method: 
\itemize{
 \item \code{nsup}: (numeric) Average (across testing individuals) number of non-zero regression coefficients. Only the coefficients for the lambda associated to \code{nsup} are returned as a 'matrix' with testing individuals in rows
 \item \code{iy}: (integer vector) Index testing elements (stored in \code{object$tst}) to be considered. Only coefficients corresponding to the testing individuals \code{object$tst[iy]} are returned
}
For \code{predict} and \code{summary} methods:
\itemize{
 \item \code{y}: (numeric vector) An optional response vector
}
}
}
\value{
Method \code{predict} returns a matrix with the predicted values for each individual in the testing set (in rows) for each value of lambda (in columns).

Method \code{coef} (list of matrices) returns the regression coefficients for each testing set individual (elements of the list). Each matrix contains the coefficients for each value of lambda (in rows) associated to each training set individual (in columns). 

Method \code{summary} returns a list object containing:
\itemize{
  \item \code{lambda}: (vector) sequence of values of lambda used in the coefficients' estimation.
  \item \code{nsup}: (vector) Number of non-zero coefficients (across testing individuals) at each solution associated to each value of lambda.
  \item \code{accuracy}: (vector) correlation between observed and predicted values associated to each value of lambda.
  \item \code{MSE}: (vector) mean squared error associated to each value of lambda.
  \item \code{optCOR}: (vector) summary of the optimal SGP with maximum accuracy.
  \item \code{optMSE}: (vector) summary of the optimal SGP with minimum MSE.
}
 
}
\description{
Useful methods for retrieving and summarizing important results from the 'SGP' function's output
}
\examples{
  # See examples in
  # help(SGP, package="SFSI")
}

