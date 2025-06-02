\name{Sample_sigma_2_theta_m}
\alias{Sample_sigma_2_theta_m}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Sample the variance and mean parameters. }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function Samples the variance and mean parameters assuming the GaSP or S-GaSP models for the discrepancy function.
}
\usage{
Sample_sigma_2_theta_m(param,  L_cur,  output,   p_theta,  p_x,  X,  have_mean, cm_obs
,S_2_f,num_obs_all)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{
%%     ~~Describe \code{param} here~~
Current parameters in the MCMC. 
}
  \item{L_cur}{
%%     ~~Describe \code{param} here~~
Cholesky decomposition of the covariance matrix. 
}
  \item{output}{
Experimental observations.
}

  \item{p_theta}{
Number of calibration parameters.
}

  \item{p_x}{
Number of range parameters.
}
  \item{X}{
Number of mean discrepancy parameters.
}
  \item{have_mean}{
Whether the mean discrepancy is zero or not.
}


  \item{cm_obs}{
outputs from the mathematical model.
}

  \item{S_2_f}{
Variance of the data. This term is useful when there are repeated experiments. 
}

  \item{num_obs_all}{
Total number of observations. If there is no repeated experiment, this is equal to the number of observable inputs. 
}


}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector of samples of the variance and mean discrepancy parameters. }
\references{
A. O'Hagan and M. C. Kennedy (2001), \emph{Bayesian calibration of computer models}, \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology}, \bold{63}, 425-464.

Mengyang Gu. (2016). Robust Uncertainty Quantification and Scalable Computation for Computer Models with Massive Output. Ph.D. thesis. Duke University.

M. Gu and L. Wang (2017) \emph{Scaled Gaussian Stochastic Process for Computer Model Calibration and Prediction}. arXiv preprint arXiv:1707.08215.

M. Gu (2018) \emph{Jointly Robust Prior for Gaussian Stochastic Process in Emulation, Calibration and Variable Selection
}. arXiv preprint arXiv:1804.09329.

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