\name{TRMF_trend}
\alias{TRMF_trend}


\title{Add Trend Model to a TRMF Object}
\description{Creates a regularization scheme that favors trend-like solutions and adds it to a TRMF object. In matrix optimization form, it adds the following term to the TRMF cost function:  \eqn{R(x) = lambdaD^2||w(DX_s)||^2 + lambdaA^2||X_s||^2}  where \eqn{X_s} is sub-set of the Xm matrix controlled by this model and D is a finite difference matrix.
}
\usage{
TRMF_trend(obj,numTS = 1,order = 1,lambdaD=1,lambdaA=0.0001,weight=1)
}

\arguments{
  \item{obj}{A TRMF object}
  \item{numTS}{number of latent time series in this model}
   \item{order}{The order of derivative for finite difference constraint matrix. Fractionally and negative values allowed.}
  \item{lambdaD}{regularization parameter for temporal constraint matrix}
  \item{lambdaA}{regularization parameter to apply simple L2 regularization to this time series model}
  \item{weight}{optional vector of weights to weight constraints, i.e. R(x) = lambdaD^2*||w*(D\%*\%X)||^2}
}
\details{
An arbitrary number of time series models can be added. \code{TRMF_trend(order = 1)} fits a random walk. \code{TRMF_trend(order = 2)} fits a cubic smoothing spline. For a single time series, \code{TRMF_trend(order = 2)} is basically equivalent to the Hodge-Prescot filter. A fractional value for order minimizes a squared fractional derivative. A negative value minimizes a (possibly fractional order) squared integral of time-series. Using a fractional or negative order for \code{TRMF_trend} could drastically reduce the sparsity of constraint matrix and slow down training. Fractional or negative order has only been lightly tested, so use with care. 
}
\note{Unlike \code{TRMF_columns()}, these lambdas are squared in the code.}
\value{
Returns an updated object of class TRMF.
}
\references{
Yu, Hsiang-Fu, Nikhil Rao, and Inderjit S. Dhillon. "High-dimensional time series prediction with missing values." arXiv preprint arXiv:1509.08333 (2015).

}
\author{
Chad Hammerquist
}

\seealso{
\code{\link{create_TRMF}}, \code{\link{TRMF_columns}}, \code{\link{TRMF_simple}}, \code{\link{TRMF_seasonal}}}

\examples{

# create test data
xm = poly(x = (-10:10)/10,degree=4)
fm = matrix(runif(40),4,10)
Am = xm\%*\%fm+rnorm(210,0,.1)

# create model
obj = create_TRMF(Am)
obj = TRMF_columns(obj,reg_type ="interval")
obj = TRMF_trend(obj,numTS=4,order=2,lambdaD=2)
out = train(obj)
plot(out)

# more complex model
require(magrittr) # for pipes

obj = create_TRMF(Am)\%>\%
    TRMF_columns(reg_type ="interval")\%>\%
    TRMF_trend(numTS=2,order=1,lambdaD=4)\%>\%
    TRMF_trend(numTS=2,order=2,lambdaD=4)\%>\%
    TRMF_trend(numTS=1,order=1.5)

out = train(obj)
plot(out)
}
