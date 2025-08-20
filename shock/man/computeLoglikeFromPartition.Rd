\name{computeLoglikeFromPartition}
\alias{computeLoglikeFromPartition}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute the log-likelihood of the model
}
\description{

This function computes the log-likelihood of a multivariate Gaussian model with a block-diagonal covariance matrix.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
computeLoglikeFromPartition(labels, expdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labels}{
  vector of block labels for each variable

%%     ~~Describe \code{labels} here~~
}
  \item{expdata}{
%%     ~~Describe \code{expdata} here~~
matrix of data 
}
}
\details{
This function computes the log-likelihood of a multivariate Gaussian model with a block-diagonal covariance matrix described in the labels vector.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
 \item{loglike}{loglikehood of the model}
 \item{df}{degree of freedom of the model}
  \item{labels}{labels provided as input}

}

\examples{
## load data to test
 data(dataTest)
 
## threshold of absS matrix
myLABELS <- thresholdAbsSPath(dataTest)$partitionList

## compute loglikelihood 
logLikePath <- lapply(myLABELS, function(x) computeLoglikeFromPartition(x,dataTest))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~loglikelihood }
%\keyword{ ~block-diagonal covariance }% __ONLY ONE__ keyword per line
