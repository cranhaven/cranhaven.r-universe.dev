\name{fitted.TRMF}
\alias{fitted.TRMF}

\title{
Extract TRMF fitted values.
}
\description{
A function to extract fitted values from a trained TRMF object.
}
\usage{
\method{fitted}{TRMF}(object,impute = FALSE,...)
}

\arguments{
  \item{object}{a trained TRMF object.}
  \item{impute}{logical, should imputed values be returned?}
  \item{...}{other arguments.}
  }

\value{
Fitted values extracted from object. If \code{impute} is TRUE then entire fitted (unscaled and uncentered) matrix is returned, otherwise
there are NAs in the same locations as the time series matrix.
}

\author{
Chad Hammerquist
}

\seealso{
\code{\link{create_TRMF}}, \code{\link{TRMF_columns}}, \code{\link{TRMF_trend}}}

\examples{
xm = poly(x = (-10:10)/10,degree=4)
fm = matrix(runif(40),4,10)
Am = xm\%*\%fm+rnorm(210,0,.2)

# create model
obj = create_TRMF(Am)
out = train(obj)
fitted(out)
}

