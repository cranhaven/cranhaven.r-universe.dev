\name{post_sample_MS}
\alias{post_sample_MS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Posterior sampling. }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function performs the posterior sampling for calibration parameters and other parameters in the model.
}
\usage{
post_sample_MS(model,par_cur_theta, par_cur_individual, emulator,math_model_MS)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
%%     ~~Describe \code{param} here~~
a S4 object of rcalibration_MS. 
}

  \item{par_cur_theta}{
%%     ~~Describe \code{kernel_type} here~~
a list of current value of the posterior sample of calibration parameters.
}

  \item{par_cur_individual}{
%%     ~~Describe \code{param} here~~
a list of the current values of the posterior sample of the individual parameter of multiple sources.
}
  \item{emulator}{
a list of emulators if specified of multiple sources.
}

  \item{math_model_MS}{
a list of mathematical models of multiple sources. 
}


}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list. The record_post is a vector of posterior values after burn-in samples. The record_theta is a matrix of of the posterior samples of theta after burn-in samples. The individual_par is a list where each element is a matrix of posterior samples of the range and nugget parameters for each source. The accept_S_theta is a vector where each element is the number of accepted posterior samples of calibration parameters. The accept_S_beta is a vector where each element is the number of accepted posterior samples of range and nugget parameters. The count_dec_record is vector where each element is the number of times the proposed samples of the calibration parameters are outside the  range of the calibration parameters for each source.

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