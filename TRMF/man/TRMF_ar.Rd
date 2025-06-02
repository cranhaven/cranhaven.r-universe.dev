\name{TRMF_ar}
\alias{TRMF_ar}


\title{Add an Auto-Regressive Regularization Model to a TRMF Object.}
\description{Creates a regularization scheme that constrains latent time-series based on auto-regressive parameters and adds it to a TRMF object.
In matrix optimization form, it adds the following term to the TRMF cost function:  \eqn{R(x) = lambdaD^2||w(DX_s)||^2 + lambdaA^2||X_s||^2}  where \eqn{X_s} is sub-set of the Xm matrix controlled by this model and D is a matrix that corresponds to an auto-regressive model.}

\usage{
TRMF_ar(obj,numTS = 1,AR,lambdaD=1,lambdaA=0.0001,weight=1)
}

\arguments{
  \item{obj}{A TRMF object}
  \item{numTS}{number of latent time series in this model}
  \item{lambdaD}{regularization parameter for temporal constraint matrix}
  \item{lambdaA}{regularization parameter to apply simple L2 regularization to this time series model}
  \item{weight}{optional vector of weights to weight constraints, i.e. R(x) = lambdaD^2*||w*(D\%*\%X)||^2}
  \item{AR}{vector of autoregressive parameters. No checks are performed}

}
\details{
Setting AR = c(1) gives a random walk model, same as \code{TRMF_trend(..., order=1)}.
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
\code{\link{create_TRMF}}, \code{\link{TRMF_columns}}, \code{\link{TRMF_trend}}}

\examples{

# create test data
xm = matrix(rnorm(80),20,4)
fm = matrix(rnorm(40),4,10)+1
Am = xm\%*\%fm+rnorm(200,0,.1)

# create model
obj = create_TRMF(Am)
obj = TRMF_columns(obj,reg_type ="interval")
obj = TRMF_ar(obj,numTS=2,AR=c(0.5),lambdaD=4)
out = train(obj)
plot(out)

}
