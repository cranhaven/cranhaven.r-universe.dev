\name{retrain}
\alias{retrain}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Retrain TRMF objects.
}
\description{
Continue training on a pre-trained TRMF object.
}
\usage{
retrain(obj, numit, fit_xm_first = TRUE,Xm=NULL,Fm=NULL,Z=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{Pre-trained TRMF object}
  \item{numit}{Number of training iterations}
  \item{fit_xm_first}{Fit the Xm factor first? This could be useful it modifications are made to one of the factors that we don't want to be overwritten.}
  \item{Xm}{Optional update to Xm matrix. See details}
  \item{Fm}{Optional update to Fm matrix. See details}
  \item{Z}{Optional update to Z vector. See details}
}
\details{
This is basically the same function as \code{train()} but it doesn't create any of the constraint matrices and doesn't do any initialization. train() must be called on obj before this function. If external regressors are in the model and Fm is provided, the number of rows of columns must equal number of regressors plus columns of Xm. The format of Fm in this case is: [column_xreg_parameters,global_xreg_parameters,Fm_parameters]^T.
}
\value{
A trained TRMF object.
}


\seealso{
 \code{\link{train.TRMF}}
}
\examples{
# create test data
tm = 3*poly(x = (-20:20)/10,degree=3)
sm = diffinv(rnorm(29,0,.1),lag=12,xi=(-5:6)/6)
xm = cbind(sm,tm)
fm = matrix(runif(40),4,10)
Am = xm\%*\%fm+rnorm(410,0,.1)

# create model
obj = create_TRMF(Am)
obj = TRMF_columns(obj,reg_type ="interval")
obj = TRMF_trend(obj,numTS=3,order=2)
obj = TRMF_seasonal(obj,numTS=1,freq=12,lambdaD=5)

# train 
out = train(obj,numit=0) # intialize
plot(out)
new_out = retrain(out,numit=10)
plot(new_out)
}

