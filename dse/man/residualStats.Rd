\name{residualStats}
\alias{residualStats}
\title{Calculate Residuals Statistics and Likelihood}
\description{Calculate the residuals statistics and likelihood of a residual.}
\usage{
    residualStats(pred, data, sampleT=nrow(pred), warn=TRUE)}
\arguments{
    \item{pred}{A matrix with columns representing time series.}
    \item{data}{A matrix with columns representing time series.}
    \item{sampleT}{An integer indicating the sample to use.}
    \item{warn}{If FALSE certain warnings are suppressed.}
}
\details{
    Residuals are calculated as
    pred[1:sampleT,,drop=FALSE] - data[1:sampleT,,drop=FALSE]
    and then statistics are calculated based on these residuals. If pred
    or data are NULL they are treated as zero.
}
\value{
    A list with elements like, cov, pred, and sampleT. pred and sampleT are
    as supplied (and are returned as this is a utility function called by
    other functions and it is convenient to pass them along). cov is the
    covariance of the residual and like is a vector of four elements 
    representing the total, constant, determinant and covariance 
    terms of the negative log likelihood function.
}
\seealso{
\code{\link{l}}
}
\examples{
    residualStats(matrix(rnorm(200), 100,2), NULL) # but typically used for a residual
}
\concept{DSE}
\keyword{ts}

