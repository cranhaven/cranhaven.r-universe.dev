\name{plot.TRMF}
\alias{plot.TRMF}

\title{
Plot Latent Time Series for a TRMF Object
}
\description{
Plots all the time series in Xm from a trained TRMF object.
}
\usage{
\method{plot}{TRMF}(x, ...)
}

\arguments{
  \item{x}{a trained TRMF object.}
  \item{...}{ignored.}
  }

\value{No return value, called for side effects} 


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
plot(out)
}