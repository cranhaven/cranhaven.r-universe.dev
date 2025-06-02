\name{post_sample_with_discrepancy}
\alias{post_sample_with_discrepancy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Posterior sampling for the model with a discrepancy function }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function performs the posterior sampling for calibration parameters and other parameters in the model, assuming the GaSP or S-GaSP model for the discrepancy function.
}
\usage{
post_sample_with_discrepancy(input, output, R0_list, kernel_type, p_theta, 
                             output_weights, par_cur, lambda_z,prior_par,theta_range,
                             S, thinning,X, have_trend, alpha,sd_proposal, 
                             discrepancy_type, simul_type,emulator,
                             emulator_type, loc_index_emulator,math_model,
                             S_2_f,num_obs_all)
                       
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
%%     ~~Describe \code{param} here~~
a matrix of observed inputs/design points of the experimental data. 
}
  \item{output}{
%%     ~~Describe \code{param} here~~
a vector of experimental data.
}
  \item{R0_list}{
%%     ~~Describe \code{param} here~~
a List of matrices where the j-th matrix is an absolute difference matrix of the j-th input vector.
}
  \item{kernel_type}{
%%     ~~Describe \code{kernel_type} here~~
type of kernel. \code{matern_3_2} and \code{matern_5_2} are \code{Matern kernel} with roughness parameter 3/2 and 5/2 respectively. \code{pow_exp} is power exponential kernel with roughness parameter alpha. If \code{pow_exp} is to be used, one needs to specify its roughness parameter alpha.
}
  \item{p_theta}{
number of calibration parameters.
}
  \item{output_weights}{
a vector of the weights of the output.
}


  \item{par_cur}{
%%     ~~Describe \code{kernel_type} here~~
current value of the posterior samples.
}

  \item{lambda_z}{
%%     ~~Describe \code{param} here~~
a scalar parameter controling how close the math model to the reality in squared distance.
}
  \item{prior_par}{
a vector of prior parameters in the prior.
}

  \item{theta_range}{
a matrix for the range of the calibration parameters. The first column is the lower bound and the second column is the upper bound of the calibration parameters.
}
  \item{S}{
number of MCMC to run.
}

  \item{thinning}{the ratio between the number of posterior samples and the number of
  recorded samples.}

  \item{X}{
a matrix for the basis of the mean discrepancy. 
}



  \item{have_trend}{
a bool value. It means the mean discrepancy is zero or not.
}

  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
a vector of roughness parameters in the kernel functions. It is only useful if the power exponential correlation function is used.
}


  \item{sd_proposal}{
a vector for the standard deviation of the proposal distribution. 
}

  \item{discrepancy_type}{
%%     ~~Describe \code{param} here~~
A string for type of discrepancy funcation. It can be chosen from 'no-discrepancy', 'GaSP' or 'S-GaSP'.
}

  \item{simul_type}{
  tpye of math model. If the simul_type is 0, it means we use the RobustGaSP R package to emulate the math model. If the simul_type is 1, it means the function of the math model is given by the user. When simul_type is 2 or 3, the mathematical model is the geophyiscal model for Kilauea Volcano.  If the simul_type is 2, it means it is for the ascending mode InSAR data; if the simul_type is 3, it means it is for the descending mode InSAR data.
  }

  
  \item{emulator}{
   an S4 class of rgasp object from the RobustGaSP R Package.
}

  \item{emulator_type}{
   a character to specify the type of emulator.  'rgasp' is for computer models with scalar-valued output and 'ppgasp'  for computer models with vectorized output.
}


  \item{loc_index_emulator}{
   a vector of the location index from the ppgasp emulator to output. Only useful for vectorized output computer model emulated by the ppgasp emulator.
}

  \item{math_model}{
  a function for the math model to be calibrated.
  }


  \item{S_2_f}{
Variance of the data. This term is useful when there are repeated experiments. 
}

  \item{num_obs_all}{
Total number of observations. If there is no repeated experiment, this is equal to the number of observable inputs.}


}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
a \code{list}. The first element is a matrix of the posterior samples after burn-in. The second element is a vector of posterior values after burn-in. The third element is the number of times the proposed samples are accepted. The first value of the vector is the number of times that the proposed calibration parameters are accepted and the second value is the number of times of the proposed log inverse range parameter and the log nugget parameters are accepted. The fourth element is the number of times the proposed samples of the calibration parameters are outside the  range of the calibration parameters.
 }
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