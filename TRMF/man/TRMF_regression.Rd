\name{TRMF_regression}
\alias{TRMF_regression}
\title{Add external regressors to TRMF object}
\description{A function to add external regressors to a TRMF object.}
\usage{
TRMF_regression(obj, Xreg, type = c("global", "columnwise"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{TRMF object created by \code{create_TRMF()}}
  \item{Xreg}{Vector or matrix of external regressors. If \code{type = "columnwise"}, \code{Xreg} can be a matrix or array, but the first two dimensions must match those of the data matrix.}
  \item{type}{how are the regressors added to the model. If \code{type = "global"} the matrix factorization includes all the regressors. If \code{type = "columnwise"} each column in the data matrix is regressed of the corresponding column of \code{Xreg}.
}
}
\details{
The coefficients model for the regressors are subject to the same regularization as the rest of the matrix factorization. Only one columnwise and one global model should be used in the same model. Both types can be include in the same model though.
}
\value{
Returns an updated object of class TRMF.
}

\author{
Chad Hammerquist
}

\seealso{
\code{\link{create_TRMF}}, \code{\link{TRMF_columns}}, \code{\link{TRMF_trend}}}

\examples{
# ~ Global regression example ~
# create test data 
bb = (-10:10)/10
xReg = 10*cos(bb*10)
xm = poly(x = bb,degree=3)
fm = matrix(rnorm(40),4,10)
Am = cbind(xReg,xm)\%*\%fm+rnorm(210,0,.2)

# creat model and fit
obj = create_TRMF(Am)
obj = TRMF_trend(obj,numTS=3,order=2)
obj = TRMF_regression(obj,Xreg=xReg,type="global")
out = train(obj)
plot(out)

# ~ columnwise regression example ~
# create test data 
bb = (-10:10)/10
xm = poly(x = bb,degree=4)
fm = matrix(rnorm(84),4,21)
Am = xm\%*\%fm+rnorm(441,0,.2)

layers = array(0,dim=c(21,21,2))
layers[,,1] = 2*cos(2*bb)\%o\%sin(4*bb)
layers[,,2] = 2*sqrt(abs(bb\%o\%bb))
nAm = Am+layers[,,1]+layers[,,2]

# creat model and fit
obj = create_TRMF(nAm)
obj = TRMF_trend(obj,numTS=4,order=2)
obj = TRMF_regression(obj,Xreg=layers,type="columnwise")
out = train(obj)
plot(out)


}
