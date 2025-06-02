\name{coef.TRMF}
\alias{coef.TRMF}

\title{
Extract TRMF Coefficients (Fm)
}
\description{
Returns the Fm (transposed) matrix from the matrix factorization Xm*Z*Fm.
}
\usage{
\method{coef}{TRMF}(object, ...)
}

\arguments{
  \item{object}{a trained TRMF object.}
  \item{...}{other arguments.}
  }

\value{
the coefficient matrix, t(Fm)
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
coef(out)
}
