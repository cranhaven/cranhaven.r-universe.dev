\name{Get_inv_all}
\alias{Get_inv_all}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Produce the inversion of the covariances of the model discrepancy and the measurement bias.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function computes inversion of the covariances of the model discrepancy and the measurement bias. This is applicable to model calibration with multiple sources of data and measurement bias. 
}
\usage{
Get_inv_all(param,  lambda_z, is_SGaSP, R0, kernel_type,  alpha_list,
p_x, num_sources)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{
%%     ~~Describe \code{param} here~~
a list of the current parameters values in MCMC.
}
  \item{lambda_z}{
%%     ~~Describe \code{param} here~~
the value of lambda_z 
}
  \item{is_SGaSP}{
%%     ~~Describe \code{param} here~~
a vector of integer values to indicate whether it is S-GaSP model or not. 0 means the model is GaSP and 1 means the model is S-GaSP.
}

  \item{R0}{
%%     ~~Describe \code{param} here~~
A List of matrices where the j-th matrix is an absolute difference matrix of the j-th input vector.
}
  \item{kernel_type}{
%%     ~~Describe \code{kernel_type} here~~
Type of kernel. \code{matern_3_2} and \code{matern_5_2} are \code{Matern kernel} with roughness parameter 3/2 and 5/2 respectively. \code{pow_exp} is power exponential kernel with roughness parameter alpha. If \code{pow_exp} is to be used, one needs to specify its roughness parameter alpha.
}
  \item{alpha_list}{
%%     ~~Describe \code{alpha} here~~
A list of roughness parameters in the kernel functions. It is only useful if the power exponential correlation function is used.
}
  \item{p_x}{
a list of dimensions of the observable inputs.
}

  \item{num_sources}{
%%     ~~Describe \code{alpha} here~~
a integer value of the number of sources.
}


}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a list of inverse covariances of discrepancy and measurement bias.}
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