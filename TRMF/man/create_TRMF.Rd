\name{create_TRMF}
\alias{create_TRMF}

\title{Create a TRMF object}
\description{Creates a TRMF object from a data matrix. This function is always needed to initialize a TRMF model.}
\usage{
create_TRMF(dataM, weight = 1, 
      normalize = c("none", "standard", "robust", "range"),
      normalize.type = c("global", "columnwise", "rowwise"),
      na.action = c("impute", "fail"),
      scaleXm = c("no","project","track"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataM}{The data matrix, each column represents a time series.
}
  \item{weight}{An optional matrix of weights to be used in the fitting process. If used, \code{sum(w^2*e^2)} is minimized.}
  \item{normalize}{Type of scaling/centering for the data. Recommended to reduce bias when using regularization. \code{none} does nothing,
  \code{standard} centers with mean, and scales by \code{sd()}, \code{robust} centers with the median and scales by \code{mad(,constant=1)},
  \code{range} maps to \code{[0-1]} interval}
  \item{normalize.type}{how should normalization be applied. \code{global} scales and centers matrix by one value. \code{columnwise} and \code{rowwise}
  normalize each column or row separately.}
  \item{na.action}{what action to take when data contains NAs}
  \item{scaleXm}{Should the columns of Xm be rescaled at each iteration. See details}
}
\details{
This function doesn't do any computation, it is the entry point for creating a TRMF model. To train the model or add additional details, see examples. Normalization is recommended in general. Regularization biases the factorization toward zero a little bit, centering changes that to bias towards the mean. Scaling makes the choosing of regularization parameters easier. If the factorization is to be used for forward forecasting, rowwise normalization is not recommended as it could remove some temporal information.
If \code{scaleXm} = 'project' then the columns of Xm will be rescaled to have sum squared value = 1. If set to 'track' then the scaling factors will be stored and updated and used scale Fm before fitting Xm at each iteration.These options were added to allow for more stable behavior of temporal regularization.
}
\value{
\code{create_TRMF} returns an object of \code{\link{class}} "\code{TRMF}" to be passed to other TRMF functions.
}
\references{
Yu, Hsiang-Fu, Nikhil Rao, and Inderjit S. Dhillon. "High-dimensional time series prediction with missing values." arXiv preprint arXiv:1509.08333 (2015).
}
\author{
Chad Hammerquist
}



\seealso{
\code{\link{train.TRMF}}, \code{\link{TRMF_columns}}, \code{\link{TRMF_trend}}
}
\examples{

# create test data
xm = poly(x = (-10:10)/10,degree=4)
fm = matrix(runif(40),4,10)
Am = xm\%*\%fm+rnorm(210,0,.2)

# create model
obj = create_TRMF(Am)
obj = TRMF_columns(obj,reg_type ="interval")
obj = TRMF_trend(obj,numTS=4,order=2)
out = train(obj)
plot(out)

}

