\name{predict.TRMF}
\alias{predict.TRMF}


\title{
Predict method for TRMF model fit
}
\description{
Predict values based on the TRMF fit
}
\usage{
\method{predict}{TRMF}(object,newdata=NULL, ...)
}
\arguments{
  \item{object}{A trained TRMF object}
  \item{newdata}{A list with slot \code{Xm} and possibly with slots \code{cXreg} and \code{gXreg}}
  \item{\dots}{other arguments, ignored.}
  }
\details{
If newdata is NULL, returns fitted model. If newdata doesn't have the term \code{Xm} or if it has a different 
number of columns than the number of latent time series, it will throw an error. If the object also contains
a global regression, \code{gXreg} must be present and appropriately sized. If the object also contains
a column-wise regression, \code{cXreg} must be present and appropriately sized.
}

\value{
Returns a matrix of predictions.
}


\author{
Chad Hammerquist
}

\seealso{
\code{\link{create_TRMF}}, \code{\link{TRMF_columns}}, \code{\link{TRMF_trend}},\code{\link{train.TRMF}}}

\examples{
xm = poly(x = (-10:10)/10,degree=4)
fm = matrix(runif(40),4,10)
Am = xm\%*\%fm+rnorm(210,0,.2)

# create model
obj = create_TRMF(Am)
out = train(obj)
fitted(out)
newXm = 1:5
predict(out,newdata=list(Xm=newXm))

}

