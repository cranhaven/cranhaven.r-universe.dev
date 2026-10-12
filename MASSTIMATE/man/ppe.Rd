\name{ppe}
\alias{ppe}
\title{Percent Prediction Error}
\description{This function calculates the percent prediction error of a sample based on the predicted and true values}
\usage{ppe(true, pred, abs = TRUE)}
\arguments{
  \item{true}{a numeric vector representing the true value on which a prediction (\code{pred}) will be compared}
  \item{pred}{a numeric vector with the predicted values, must be equal length to \code{true}}
  \item{abs}{a logical value indicating whether to return the absolute values for ppe, default is \code{abs = TRUE}}
}
\details{
This function calculates the percent prediction error (ppe) as a scaled residual (Smith 1980), generally based on the absolute residual, so:
  \deqn{
    ppe=\frac{\left ( \left | true-pred \right | \right )}{pred}\times 100
  }{ppe=(( | true-pred | ) / pred) x 100}
}
\value{
Function returns a list of results, including the a list of all the precent prediction errors, along with the mean, 95 percent confidence intervals, range, and standard deviation.
}
\author{Nicolas E. Campione}
\references{
  Smith, R. J. (1980) Rethinking allometry. Journal of Theoretical Biology, 87, 97-111.
}
\seealso{
  \code{\link{see}}
}