\name{coef.TSmodelEstEval}
\alias{coef.TSmodelEstEval}
\alias{coef.TSestModelEstEval}
\title{Specific Methods for coef}
\description{See the generic function description.}
\usage{
    \method{coef}{TSmodelEstEval}(object, criterion.args=NULL, ...)
    \method{coef}{TSestModelEstEval}(object, criterion.args=NULL, ...)
}
\arguments{
    \item{object}{an object (model) from which to extract
       coefficients(parameters).}
    \item{criterion.args}{arguments to be passed to this method when it is
         called by \code{EstEval}.}
   \item{...}{(further arguments, currently disregarded).}
}
\details{
    The methods ***.ee are intended mainly to be called from \code{EstEval} as
    criterion for evaluating an estimation method. See \code{coef}. }
\seealso{
    \code{\link{EstEval}} 
    \code{\link{coef}} }
\concept{DSE}
\keyword{ts}


