\name{s_soda}
\alias{s_soda}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
S-SODA algorithm for general index model variable selection
}
\description{
S-SODA is an extension of SODA to conduct variable selection for general index models with continuous response. S-SODA first evenly discretizes the continuous response into H slices, and then apply SODA on the discretized response. Compared with existing variable selection methods based on the Sliced Inverse Regression (SIR), SODA requires neither the linearity nor the constant variance condition and is much more robust.
}
\usage{
s_soda(x, y, H = 5, gam = 0, minF = 3, norm = F, debug = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
The design matrix, of dimensions n * p, without an intercept. Each row is an observation vector.
}
  \item{y}{
The response vector of dimension n * 1.
}
  \item{H}{
The number of slices.
}
  \item{gam}{
EBIC penalization coefficient parameter for SODA.
}
  \item{minF}{
Minimum number of steps in forward interaction screening. Default is minF=3.
}
  \item{norm}{
If set as True, S-SODA first marginally quantile-normalize each predictor to the standard normal distribution.
}
  \item{debug}{
If print debug information.
}
}
\value{
  \item{BIC}{
Trace of extended Bayesian information criterion (EBIC) score.
}
  \item{Var}{
Trace of selected variables.
}
  \item{Term}{
Trace of selected main and interaction terms.
}
  \item{best_BIC}{
Final selected term set EBIC score.
}
  \item{best_Var}{
Final selected variables.
}
  \item{best_Term}{
Final selected main and interaction terms.
}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# # (uncomment the code to run)
# # Simulation:  x1 / (1 + x2^2) example 
# N = 500
# x1 = runif(N, -3, +3)
# x2 = runif(N, -3, +3)
# x3 = x1 / exp(x2^2) + rnorm(N, 0, 0.2)
# ss = s_soda_model(cbind(x1,x2), x3, H=25)
# 
# # true surface in grid
# MM = 50
# xx1 = seq(-3, +3, length.out = MM)
# xx2 = seq(-3, +3, length.out = MM)
# yyy = matrix(0, MM, MM)
# for(i in 1:MM)
#   for(j in 1:MM)
#     yyy[i,j] = xx1[i] / exp(xx2[j]^2)
# 
# # predicted surface
# ppp = s_soda_pred_grid(xx1, xx2, ss, po=1)
# 
# par(mfrow=c(1, 2), mar=c(1.75, 3, 1.25, 1.5))
# persp(xx1, xx2, yyy, theta=-45, xlab="X1", ylab="X2", zlab="Y")
# persp(xx1, xx2, ppp, theta=-45, xlab="X1", ylab="X2", zlab="Pred")
# 
# # Pumadyn dataset
# #data(pumadyn);
# #s_soda(pumadyn_isample_x, pumadyn_isample_y, H=25, gam=0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ S-SODA }
\keyword{ general index model }
\keyword{ interaction_selection }