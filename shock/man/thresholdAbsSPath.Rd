\name{thresholdAbsSPath}
\alias{thresholdAbsSPath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Detect partitions of variables into blocks.
%%  ~~function to do ... ~~
}
\description{
This function returns a list of partitions of variables based on the sample covariance matrix for several levels of threshold.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
thresholdAbsSPath(expdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expdata}{
    matrix of data 
%%     ~~Describe \code{expdata} here~~
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{partitionList}{list of partitions of variables (vectors) deduced by thresholding the sample covariance matrix }
  \item{lambdaPath}{ list of threshold parameters}

}

\examples{

## load data to test 
data(dataTest)

## detect partitions of variables into blocks based on the sample covariance matrix
partitions <- thresholdAbsSPath(dataTest)$partitionList


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
