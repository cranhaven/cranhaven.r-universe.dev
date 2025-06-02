\name{residuals.TRMF}
\alias{residuals.TRMF}

\title{
Extract TRMF residuals
}
\description{
A function to extract residuals from a trained TRMF object.
}
\usage{
\method{residuals}{TRMF}(object, ...)
}

\arguments{
  \item{object}{a trained TRMF object.}
  \item{...}{ignored}
  }

\value{
residuals extracted from TRMF object
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
resid(out)
}
