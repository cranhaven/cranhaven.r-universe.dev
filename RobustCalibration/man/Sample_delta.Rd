\name{Sample_delta}
\alias{Sample_delta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Sample the model discrepancy. }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function samples a vector of the model discrepancy for the scenario with 
 multiple sources and measurement bias. 
}
\usage{
Sample_delta(cov_inv_all,  tilde_output_cur,   param,  p_x,   
num_sources, num_obs, rand_norm)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cov_inv_all}{
%%     ~~Describe \code{param} here~~
a list of inverse covariances of discrepancy and measurement bias. 
}
  \item{tilde_output_cur}{
a list of transformed observations.
}

  \item{param}{
a list of the current parameters values in MCMC.
}

  \item{p_x}{
a list of dimensions of the observable inputs.
}

  \item{num_sources}{
the number of sources.
}

  \item{num_obs}{
the number of observations. 
}

  \item{rand_norm}{
the vector of i.i.d. standard normal samples. 
}


}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A vector of samples of model discrepancy. }
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