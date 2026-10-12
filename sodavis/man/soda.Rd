\name{soda}
\alias{soda}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
SODA algorithm for variable and interaction selection
}
\description{
SODA is a forward-backward variable and interaction selection algorithm under logistic regression model with second-order terms.  In the forward stage, a stepwise procedure is conducted to screen for important predictors with both main and interaction effects, and in the backward stage SODA remove insignificant terms so as to optimize the extended BIC (EBIC) criterion.  SODA is applicable for variable selection for logistic regression, linear/quadratic discriminant analysis and other discriminant analysis with generative model being in exponential family.
}
\usage{
soda(xx, yy, norm = F, debug = F, gam = 0, minF = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xx}{
The design matrix, of dimensions n * p, without an intercept. Each row is an observation vector.
}
  \item{yy}{
The response vector of dimension n * 1.
}
  \item{norm}{
Logical flag for xx variable quantile normalization to standard normal, prior to performing
SODA algorithm.  Default is norm=FALSE.  Quantile-normalization is suggested if the data contains
obvious outliers.
}
  \item{debug}{
Logical flag for printing debug information.
}
  \item{gam}{
Tuning paramter gamma in extended BIC criterion.

EBIC for selected set S:

  EBIC = -2 * log-likelihood + |S| * log(n) + 2 * |S| * gamma * log(p)
}
  \item{minF}{
Minimum number of steps in forward interaction screening. Default is minF=3.
}
}
\value{
  \item{EBIC}{
Trace of extended Bayesian information criterion (EBIC) score.
}
  \item{Type}{
Trace of step type ("Forward (Main)", "Forward (Int)", "Backward").
}
  \item{Var}{
Trace of selected variables.
}
  \item{Term}{
Trace of selected main and interaction terms.
}
  \item{final_EBIC}{
Final selected term set EBIC score.
}
  \item{final_Var}{
Final selected variables.
}
  \item{final_Term}{
Final selected main and interaction terms.
}
}
\references{
Li Y, Liu JS. (2017). Robust Variable and Interaction Selection for Logistic Regression and Multiple Index Models. \emph{Technical Report}.
}
\author{
Yang Li, Jun S. Liu
}
\examples{
# # (uncomment the code to run)
# # simulation study with 1 main effect and 2 interactions
# N = 250;
# p = 1000;
# r = 0.5;
# s = 1;
# H = abs(outer(1:p, 1:p, "-"))
# S = s * r^H;
# S[cbind(1:p, 1:p)] = S[cbind(1:p, 1:p)] * s

# xx = as.matrix(data.frame(mvrnorm(N, rep(0,p), S)));
# zz = 1 + xx[,1] - xx[,10]^2 + xx[,10]*xx[,20];
# yy = as.numeric(runif(N) < exp(zz) / (1+exp(zz)))

# res_SODA = soda(xx, yy, gam=0.5);
# cv_SODA  = soda_trace_CV(xx, yy, res_SODA)
# cv_SODA

# # Michigan lung cancer dataset
# data(mich_lung);
# res_SODA = soda(mich_lung_xx, mich_lung_yy, gam=0.5);
# cv_SODA  = soda_trace_CV(mich_lung_xx, mich_lung_yy, res_SODA)
# cv_SODA
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ SODA }
\keyword{ logistic_regression }
\keyword{ quadratic_discriminant_analysis }
\keyword{ interaction_selection }
