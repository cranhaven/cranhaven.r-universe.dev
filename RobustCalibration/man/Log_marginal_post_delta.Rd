\name{Log_marginal_post_delta}
\alias{Log_marginal_post_delta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Natural Logorithm of the posterior of the discrepancy in model calibration with multiple sources with measurement bias. }
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function compute the natural Logorithm of the posterior assuming the GaSP or S-GaSP models for the discrepancy function.
}
\usage{
Log_marginal_post_delta(param, L, delta,  p_x, CL, a, b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{param}{
%%     ~~Describe \code{param} here~~
current parameters in the MCMC. 
}
  \item{L}{
%%     ~~Describe \code{param} here~~
Cholesky decomposition of the covariance matrix. 
}
  \item{delta}{
a vector of the discrepancy.
}

  \item{p_x}{
dimension of observable inputs.
}


  \item{CL}{
Prior parameter in the jointly robust prior.
}
  \item{a}{
Prior parameter in the jointly robust prior.
}

  \item{b}{
Prior parameter in the jointly robust prior.
}

}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
Natural logorithm of the posterior of the discrepancy function. }
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