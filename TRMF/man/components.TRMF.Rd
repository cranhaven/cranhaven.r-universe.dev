\name{components.TRMF}
\alias{components.TRMF}


\title{
 Access TRMF factors
}
\description{
This function returns the factors (Xm, Fm,Z) from a trained TRMF object
}
\usage{
\method{components}{TRMF}(object, XorF = c("Xm","Fm","Z","Fm_each"), ...)
}

\arguments{
  \item{object}{trained TRMF object}
  \item{XorF}{ which factor to return}
  \item{...}{ignored}
}
\details{
Returns the matrix factors.  If matrix normalization was used in \code{\link{create_TRMF}}, \code{Xm\%*\%diag(Z)\%*\%Fm} could look much different than the input data matrix. If external regressor were included in the model and \code{XorF = "F_each"} then a returns a list with Fm split up by parts.
}
\value{
A matrix or vector, or a possibly a list if \code{XorF = "F_each"}.
}
\author{
Chad Hammerquist
}



\seealso{
\code{\link{create_TRMF}}, \code{\link{TRMF_columns}}, \code{\link{TRMF_trend}}}

\examples{
# create test data
xm = poly(x = (-10:10)/10,degree=4)
fm = matrix(rnorm(40),4,10)
Am = xm\%*\%fm+rnorm(210,0,.2)

# create model
obj = create_TRMF(Am)
out = train(obj)
plot(out)
components(out,"Xm")

}


