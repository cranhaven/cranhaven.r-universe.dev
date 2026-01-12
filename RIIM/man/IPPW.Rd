\name{IPPW}
\alias{IPPW}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Randomization-based inference using inverse post-matching probability weighting (IPPW)}
\description{
This function implements the inverse post-matching probability weighting (IPPW) method proposed in Zhu, Zhang, Guo and Heng (2024). It can be used for conducting randomization-based inference for the sample average treatment effect in potentially inexactly matched observational studies. By default, the matching design implemented in the package is optimal full matching, and the estimated propensity scores used in our method are obtained by the XGBoost method. 
}

\usage{
IPPW(Y, Z, X, min.controls = 0.0001,max.controls = 10000, caliper = TRUE, 
     calipersd = 0.2, dim = FALSE, gamma = 0.1, alpha = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{The observed outcome vector.}
  \item{Z}{The treatment indicator vector.}
  \item{X}{The covariates matrix. Each row is an individual.}
  \item{min.controls}{The minimum ratio of controls to treatments permitted within a matched set. The default is 0.0001.}
  \item{max.controls}{The maximum ratio of controls to treatments permitted within a matched set. The default is 10000.}
  \item{caliper}{Whether adding a propensity score caliper or not. The default is TRUE.}
  \item{calipersd}{The standard deviation of the logit propensity score for caliper. The default is 0.2.}
  \item{dim}{Whether using difference-in-means estimator to estimate the average treatment effect. The default is FALSE.}
  \item{gamma}{The regularization threshold. The default is 0.1.}
  \item{alpha}{The prespecified level alpha for the 1-alpha condifence interval.}

}

\value{
  \item{estimate}{The estimate for the sample average treatment effect using the IPPW estimator.}
  \item{var}{The variance for the sample average treatment effect using the IPPW estimator.}
  \item{low}{The lower bound for the sample average treatment effect using the IPPW estimator.}
  \item{up}{The upper bound for the sample average treatment effect using the IPPW estimator.}
  \item{CI}{The confidence interval for the sample average treatment effect using the IPPW estimator.}
  \item{balance}{The pre- and post-matching covariate balance table.}

}
\references{
Fogarty, C. B. (2018). On mitigating the analytical limitations of finely stratified experiments. Journal of the Royal Statistical Society Series B: Statistical Methodology, 80(5), 1035-1056.

Hansen, B. B. (2004). Full matching in an observational study of coaching for the SAT. Journal of the American Statistical Association, 99(467), 609-618.

Hansen, B. B. and Klopfer, S. O. (2006). Optimal full matching and related designs via network flows. Journal of Computational and Graphical Statistics, 15(3), 609-627.

Kang, H., Kreuels, B., May, J., and Small, D. S. (2016). Full matching approach to instrumental variables estimation with application to the effect of malaria on stunting. The Annals of Applied Statistics, 10(1), 335-364.

Rosenbaum, P. R. (1991). A characterization of optimal designs for observational studies. Journal of the Royal Statistical Society: Series B (Methodological), 53(3), 597-610.

Zhu, J., Zhang, J., Guo, Z., and Heng, S. (2024). Randomization-Based Inference for Average Treatment Effect in Inexactly Matched Observational Studies. arXiv preprint, arXiv:2308.02005.
}
\author{
Jianan Zhu (maintainer), Jeffrey Zhang, Zijian Guo and Siyu Heng.
}

\examples{
library(MASS)
library(xgboost)
library(optmatch)

# Generate data
set.seed(1)
d = 3
n = 30
sigma = diag(d)

# Generate X
X_d = mvtnorm::rmvnorm(n, mean = rep(0,d), sigma = sigma)

# Generate Z
C = -2.5 
fx = 0.1*(X_d[,1])^3 + 0.3*(X_d[,2]) + 0.2*log((X_d[,3])^2) + 
     abs(X_d[,1]*X_d[,2]) + rnorm(n,0,1) + C
p = exp(fx)/(1+exp(fx)) # the probability of receiving the treatment
Z = rep(0,length(p))
for(i in seq_along(p)){
  Z[i] = rbinom(1,1,p[i])
}

# Generate Y 
Y_0 = 0.2*(X_d[,1])^3 + 0.2*abs(X_d[,2]) + 0.5*abs(X_d[,3]) + rnorm(n,0,1)
Y_1 = Y_0 + 1 + 0.3*X_d[,1] + 0.2*X_d[,3]^3
Y = (1-Z)*Y_0 + Z*Y_1

# The output
est = IPPW(Y,Z,X_d,min.controls = 0.01,max.controls = 100,caliper=FALSE,  
calipersd = 0.2,dim=FALSE,gamma=0.1,alpha=0.05)$estimate
est

}
