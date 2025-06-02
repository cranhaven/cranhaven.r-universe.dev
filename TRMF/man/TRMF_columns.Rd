\name{TRMF_columns}
\alias{TRMF_columns}
%- also NEED an '\alias' for EACH other topic documented here.
\title{
Add a column regularization model to TRMF object
}
\description{
Adds a regularization model to TRMF object created by \code{create_TRMF()} to constrain the fitting process of the coefficient matrix. 

}
\usage{
TRMF_columns(obj,
    reg_type = c("l2", "nnls", "constrain", "interval", "none"), lambda = 0.0001,mu0=NULL)
}

\arguments{
  \item{obj}{TRMF object created by \code{create_TRMF()}}
  \item{reg_type}{regularization type to apply when fitting TRMF model. \code{l2} regularizes using an L2 function (see details), \code{nnls}
  forces coefficients to be non-negative. \code{constrain} constrains coefficients to be non-negative and to sum to 1. \code{interval} constrains coefficients
  to the interval [0-1]
}
  \item{lambda}{L2 regularization parameter used for all regularization types. Can be a single value, a vector, or a matrix. If not a scalar, the dimension must match the number of rows of \code{Fm}. In a Bayesian framework, this is the inverse prior covariance for matrix coefficients. Note this value is not squared in the code.}
   \item{mu0}{The prior value for matrix coefficient. Can be a single value or vector. If not a scalar, the dimension must match the number of rows of \code{Fm}. If NULL it is set to zero. If external regressors are used these are included in Fm, so if lambda and mu0 are not scalars, they need to be arranged as: [column_xreg_parameters,global_xreg_parameters,Fm_parameters].}
}
\details{
This function doesn't do any computations, it just sets up regularization parameters for the coefficient matrix. This function should only be called once on a TRMF object. If called twice, it will overwrite previous model with a warning. In addition to the possible constraints, a possible L2 regularization term is added to the fit. The regularization term for each column \code{i} is \eqn{(Fm_i - \mu_0)^TP(Fm_i - \mu_0)} where \eqn{P = \lambda I} or \eqn{P = diag(\lambda)} or \eqn{P = \lambda} depending on the size of provided lambda. A nonzero value for \code{lambda} is recommended to ensure stability of fit.
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
\code{\link{train.TRMF}}, \code{\link{create_TRMF}}, \code{\link{TRMF_trend}}
}
\examples{

# create test data
xm = poly(x = (-10:10)/10,degree=4)
fm = matrix(abs(rnorm(40)),4,10)
Am = xm\%*\%fm+rnorm(210,0,.2)

# create model
obj = create_TRMF(Am)
obj = TRMF_columns(obj,reg_type ="nnls")
out = train(obj)
plot(out)


}
