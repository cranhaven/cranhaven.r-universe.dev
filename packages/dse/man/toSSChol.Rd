\name{toSSChol}
\alias{toSSChol}
\alias{toSSChol.TSmodel}
\alias{toSSChol.TSestModel}

\title{Convert to Non-Innovation State Space Model}
\description{
This function may not be working properly.

Convert to a non-innovations state space representation using 
the given matrix (Om) as the measurement noise covariance. 
Om would typically be an estimate of the output noise, such as returned 
in \code{$estimates$cov} of the function \code{l} (\code{l.SS} or \code{l.ARMA}).
This assumes that the noise processes in the arbitrary SS representation 
are white and uncorrelated.
}
\usage{
    toSSChol(model, ...)
    \method{toSSChol}{TSmodel}(model, Om=diag(1,nseriesOutput(model)), ...)
    \method{toSSChol}{TSestModel}(model, Om=NULL, ...)
}
\arguments{
    \item{model}{An object of class TSmodel.}
    \item{Om}{
      a matrix to be used as the measurement noise covariance. If Om is
      not supplied and model is of class TSestModel then 
      \code{model$estimates$cov} is used. Otherwise, Om is set to the 
      identity matrix.}
    \item{...}{arguments to be passed to other methods.}
}
\value{
An object of class 'SS' 'TSmodel' containing a state space model which is
not in innovations form.
}
\details{
Convert to a non-innovations SS  representation using a Cholesky 
decomposition of Om as the coefficient matrix of the output noise.
}
\seealso{
\code{\link{toSSinnov}}
}
\examples{
    data("eg1.DSE.data.diff", package="dse")
    model <- estVARXls(eg1.DSE.data.diff)
    model <- toSSChol(model)
}
\concept{DSE}
\keyword{ts}

