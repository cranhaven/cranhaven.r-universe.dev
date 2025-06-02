\name{Get_R_new}
\alias{Get_R_new}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Cholesky decomposition of the correlation matrix in GaSP.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function computes the Cholesky decomposition of the correlation matrix in GaSP.
}
\usage{
Get_R_new(beta_delta,  eta_delta,  R0, kernel_type,  alpha, inv_output_weights)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta_delta}{
%%     ~~Describe \code{param} here~~
Inverse range parameters.
}
  \item{eta_delta}{
%%     ~~Describe \code{param} here~~
nugget parameters.
}
  \item{R0}{
%%     ~~Describe \code{param} here~~
A List of matrices where the j-th matrix is an absolute difference matrix of the j-th input vector.
}
  \item{kernel_type}{
%%     ~~Describe \code{kernel_type} here~~
Type of kernel. \code{matern_3_2} and \code{matern_5_2} are \code{Matern kernel} with roughness parameter 3/2 and 5/2 respectively. \code{pow_exp} is power exponential kernel with roughness parameter alpha. If \code{pow_exp} is to be used, one needs to specify its roughness parameter alpha.
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
Roughness parameters in the kernel functions. It is only useful if the power exponential correlation function is used.
}
  \item{inv_output_weights}{
%%     ~~Describe \code{alpha} here~~
The inverse of output weights.
}


}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
lower triangular matrix of Cholesky decomposition of the correlation matrix in GaSP.}
\author{
%%  ~~who you are~~
\packageAuthor{RobustCalibration}

Maintainer: \packageMaintainer{RobustCalibration}
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
% \code{\link{log_marginal_lik}}.
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")

\keyword{internal}