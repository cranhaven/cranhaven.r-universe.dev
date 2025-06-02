\name{predict_separable_2dim_MS}
\alias{predict_separable_2dim_MS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Fast prediction when the test points lie on a 2D lattice for multiple sources of observations.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function computes fast computation when the test points lie on a 2D lattice for multiple sources of observations.
}
\usage{
predict_separable_2dim_MS(object, testing_input_separable,
X_testing=NULL, math_model=NULL,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of  class \code{rcalibration}.}
  \item{testing_input_separable}{
a list of two. In the first (outer) list, each list is a source of test input. Then in the second (interior) list,  The first element is a vector of the coordinate of the latitue 
and the second element is a vector of the coordinate of the longitude.
}
  \item{X_testing}{mean/trend for prediction where the defaul value is NULL.}
  \item{math_model}{
  a list of functions of math models to be calibrated.
  }
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The returned value is a S4 CLass \code{predictobj.rcalibration_MS}. 

}
\references{
A. O'Hagan and M. C. Kennedy (2001), \emph{Bayesian calibration of computer models}, \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology}, \bold{63}, 425-464.

Mengyang Gu. (2016). Robust Uncertainty Quantification and Scalable Computation for Computer Models with Massive Output. Ph.D. thesis. Duke University.

M. Gu and L. Wang (2018) \emph{Scaled Gaussian Stochastic Process for Computer Model Calibration and Prediction}. SIAM/ASA Journal on Uncertainty Quantification, \bold{6}, 1555-1583.

M. Gu (2019) \emph{Jointly Robust Prior for Gaussian Stochastic Process in Emulation, Calibration and Variable Selection
}. Bayesian Analysis,  \bold{14}, 857-885.

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