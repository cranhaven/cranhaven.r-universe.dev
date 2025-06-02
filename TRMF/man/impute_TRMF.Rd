\name{impute_TRMF}
\alias{impute_TRMF}
\title{
Impute missing values in a matrix
}
\description{
Impute missing values in matrix from a pre-trained TRMF object.
}
\usage{
impute_TRMF(obj)
}

\arguments{
  \item{obj}{a trained TRMF object}
}
\details{
Essentially an accessor function. Replaces the missing values in data matrix with values from the fitted TRMF object.
}
\value{
data matrix with missing values imputed
}

\references{
Yu, Hsiang-Fu, Nikhil Rao, and Inderjit S. Dhillon. "High-dimensional time series prediction with missing values." arXiv preprint arXiv:1509.08333 (2015).
}
\author{
Chad Hammerquist
}



\seealso{
\code{\link{train.TRMF}}, \code{\link{create_TRMF}}, \code{\link{TRMF_trend}}}

\examples{


# create test data
xm = poly(x = (-10:10)/10,degree=4)
fm = matrix(rnorm(40),4,10)
Am = xm\%*\%fm+rnorm(210,0,.2)
Am[sample.int(210,20)] = NA

# create model
obj = create_TRMF(Am)
obj = TRMF_trend(obj,numTS=4,order=2)
out = train(obj)
impute_TRMF(out)

}
