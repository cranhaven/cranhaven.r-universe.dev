\name{shockSelect}
\alias{shockSelect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Shock selection
}
\description{

This function performs block-diagonal covariance selection for high-dimensional Gaussian graphical models.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
shockSelect(expdata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{expdata}{
%%     ~~Describe \code{expdata} here~~
matrix of data 
}
}
%\details{
%This function decomposes the initial network inference problem into sub-problems based on the slope heuristic.
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use

 \item{SHDJlabels}{Vector of partition labels based on the slope heuristic dimension jump}
 \item{SHRRlabels}{Vector of partition labels based on the slope heuristic robust regression}
 \item{capusheOutput}{output of the kappa coefficient calibration capushe function}

}

\examples{

## load data to test 
data(dataTest)

## dimension of the dataset expdata
n <- dim(dataTest)[1]
p <- dim(dataTest)[2]

## perform partition of variables selection
## based on the slope heuristic
resShock <- shockSelect(dataTest)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~loglikelihood }
%\keyword{ ~block-diagonal covariance }% __ONLY ONE__ keyword per line
