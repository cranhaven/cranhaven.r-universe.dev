\name{predict_separable_2dim}
\alias{predict_separable_2dim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Fast prediction when the test points lie on a 2D lattice.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function computes fast computation when the test points lie on a 2D lattice.
}
\usage{
predict_separable_2dim(object, testing_input_separable,
X_testing=matrix(0,length(testing_input_separable[[1]])*
length(testing_input_separable[[2]]),1), n_thinning=10,  
interval_est = NULL,math_model=NULL,test_loc_index_emulator=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of  class \code{rcalibration}.}
  \item{testing_input_separable}{
a list. The first element is a vector of the coordinate of the latitue 
and the second element is a vector of the coordinate of the longitude.
}
  \item{X_testing}{a matrix of mean/trend for prediction.}
  \item{n_thinning}{number of points thinning the MCMC posterior samples.}
  \item{math_model}{
  a function for the math model to be calibrated.
  }
    \item{test_loc_index_emulator}{
   a vector of the location index from the ppgasp emulator to output. Only useful for vectorized output computer model emulated by the ppgasp emulator.
  }




}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The returned value is a S4 CLass \code{predictobj.rcalibration}. 

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