\name{Get_R_z_new}
\alias{Get_R_z_new}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Cholesky decomposition of the covariance matrix in S-GaSP.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function computes the Cholesky decomposition of the covariance matrix in GaSP.
}
\usage{
Get_R_z_new(beta_delta,  eta_delta, lambda_z,  R0, kernel_type,  alpha, 
            inv_output_weights)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta_delta}{
%%     ~~Describe \code{param} here~~
a vector of inverse range parameters.
}
  \item{eta_delta}{
%%     ~~Describe \code{param} here~~
a scalar of nugget parameters.
}
  \item{lambda_z}{
%%     ~~Describe \code{param} here~~
a scalar parameter controling how close the math model to the reality in squared distance.
}

  \item{R0}{
%%     ~~Describe \code{param} here~~
a list of matrices where the j-th matrix is an absolute difference matrix of the j-th input vector.
}
  \item{kernel_type}{
%%     ~~Describe \code{kernel_type} here~~
type of kernel. \code{matern_3_2} and \code{matern_5_2} are \code{Matern kernel} with roughness parameter 3/2 and 5/2 respectively. \code{pow_exp} is power exponential kernel with roughness parameter alpha. If \code{pow_exp} is to be used, one needs to specify its roughness parameter alpha.
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
roughness parameters in the kernel functions. It is only useful if the power exponential correlation function is used.
}
  \item{inv_output_weights}{
%%     ~~Describe \code{alpha} here~~
the inverse of output weights.
}


}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
lower triangular matrix of Cholesky decomposition of the covariance matrix in S-GaSP.}
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