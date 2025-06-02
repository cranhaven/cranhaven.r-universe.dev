\name{TRMF_simple}
\alias{TRMF_simple}

\title{Add L2 regularization model to a TRMF object}
\description{Creates an L2 regularization and adds it to a TRMF object. In matrix optimization form, it adds the following term to the TRMF cost function:  \eqn{R(x) = lambdaA^2||w(X_s)||^2}  where \eqn{X_s} is sub-set of the Xm matrix controlled by this model.}

\usage{
TRMF_simple(obj,numTS = 1,lambdaA=0.0001,weight=1)
}
\arguments{
  \item{obj}{A TRMF object}
  \item{numTS}{number of latent time series in this model}
  \item{lambdaA}{regularization parameter to apply simple L2 regularization to this time series model}
  \item{weight}{optional vector of weights to weight constraints, i.e. R(x) = lambdaA^2*||w*X||^2}
}
\details{
This is called by \code{train_TRMF} if the TRMF object doesn't have any time series models.
}
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
\code{\link{create_TRMF}}, \code{\link{TRMF_columns}},\code{\link{TRMF_seasonal}}, \code{\link{TRMF_trend}}}

\examples{
# create test data
xm = matrix(rnorm(160),40,4)
fm = matrix(runif(40),4,10)
Am = xm\%*\%fm+rnorm(400,0,.1)

# create model
obj = create_TRMF(Am)
obj = TRMF_simple(obj,numTS=4,lambdaA=0.1)
out = train(obj)
plot(out)
}