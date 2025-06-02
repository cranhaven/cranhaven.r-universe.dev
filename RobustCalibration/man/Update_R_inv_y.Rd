\name{Update_R_inv_y}
\alias{Update_R_inv_y}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Update the inverse of covariance multiplied by the outputs in the S-GaSP model. }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function update the inverse of R_z multiple the outputs in the S-GaSP model for prediction.
}
\usage{
Update_R_inv_y(R_inv_y,  R0,  beta_delta,   kernel_type,  alpha,  lambda_z,  num_obs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{R_inv_y}{
%%     ~~Describe \code{param} here~~
A vector of inverse of covariance  multiplied by the outputs.
}
  \item{R0}{
%%     ~~Describe \code{param} here~~
A List of matrix where the j-th matrix is an absolute difference matrix of the j-th input vector.
}
  \item{beta_delta}{
%%     ~~Describe \code{param} here~~
Inverse range parameters.
}

  \item{kernel_type}{
%%     ~~Describe \code{kernel_type} here~~
Type of kernel. \code{matern_3_2} and \code{matern_5_2} are \code{Matern kernel} with roughness parameter 3/2 and 5/2 respectively. \code{pow_exp} is power exponential kernel with roughness parameter alpha. If \code{pow_exp} is to be used, one needs to specify its roughness parameter alpha.
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
Roughness parameters in the kernel functions. It is only useful if the power exponential correlation function is used.
}

  \item{lambda_z}{
%%     ~~Describe \code{param} here~~
A parameter controling how close the math model to the reality in squared distance.
}

  \item{num_obs}{
%%     ~~Describe \code{alpha} here~~
Number of observations.
}



}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector of the inverse of covariance multiplied by the outputs in the S-GaSP model. }
\references{
A. O'Hagan and M. C. Kennedy (2001), \emph{Bayesian calibration of computer models}, \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology}, \bold{63}, 425-464.

Mengyang Gu. (2016). Robust Uncertainty Quantification and Scalable Computation for Computer Models with Massive Output. Ph.D. thesis. Duke University.

M. Gu and L. Wang (2017) \emph{Scaled Gaussian Stochastic Process for Computer Model Calibration and Prediction}. arXiv preprint arXiv:1707.08215.


}
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