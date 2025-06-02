\name{summary.TRMF}
\alias{summary.TRMF}

\title{
Summarize TRMF
}
\description{
\code{summary} method for class "TRMF"
}
\usage{
\method{summary}{TRMF}(object, ...)
}

\arguments{
  \item{object}{TRMF object.}
  \item{...}{other arguments.}
  }

\value{
NULL
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
summary(obj)
summary(out)
}
