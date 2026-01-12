\name{IPPW_IV}
\alias{IPPW_IV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{The bias-corrected Wald estimator for the complier average treatment effect}
\description{
This function implements the bias-corrected Wald estimator for randomization-based estimation and inference for the complier average treatment effect under inexact matching, proposed in Zhu, Zhang, Guo and Heng (2024). By default, the matching design implemented in the package is optimal full matching, and the estimated propensity scores used in our method are obtained by the XGBoost method. 
}
\usage{
IPPW_IV(Y, Z, X, D, min.controls = 0.0001, max.controls = 10000, 
        caliper = TRUE, calipersd = 0.2, classical = FALSE, gamma = 0.1, 
        lower.bound, upper.bound, by,alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{The observed outcome vector.}
  \item{Z}{The binary instrument vector.}
  \item{X}{The covariates matrix. Each row is an individual.}
  \item{D}{The binary treatment indicator vector.}
  \item{min.controls}{The minimum ratio of the unencouraged to the encouraged permitted within a matched set. The default is 0.0001.}
  \item{max.controls}{The maximum ratio of the unencouraged to the encouraged permitted within a matched set. The default is 10000.}
  \item{caliper}{Whether adding a propensity score caliper or not. The default is TRUE.}
  \item{calipersd}{The standard deviation of the logit propensity score for caliper. The default is 0.2.}
  \item{classical}{Whether using the classical estimator (proposed by Kang et al. (2016)) to estimate the complier average treatment effect. The default is FALSE.}
  \item{gamma}{The regularization threshold. The default is 0.1.}
  \item{lower.bound}{The starting value of the search region for the point estimate.}
  \item{upper.bound}{The end value of the search region for the point estimate.}
  \item{by}{The increment of the search region for the point estimate.}
  \item{alpha}{The prespecified level alpha for the 1-alpha confidence interval.}

}

\value{
  \item{estimate}{The estimate for the complier average treatment effect using the bias-corrected Wald estimator.}
  \item{CI}{The confidence interval for the complier average treatment effect using the bias-corrected Wald estimator.}
  \item{value}{The corresponding z-scores for the hypothetical values of the complier average treatment effect.}
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

# generate X
X_d = mvtnorm::rmvnorm(n, mean = rep(0,d), sigma = sigma)

# generate Z
C = -2.5 
fx = 0.1*(X_d[,1])^3 + 0.3*(X_d[,2]) + 0.2*log((X_d[,3])^2) + 
     abs(X_d[,1]*X_d[,2]) + rnorm(n,0,1) + C
p = exp(fx)/(1+exp(fx)) # the probability of receiving the treatment
Z = rep(0,length(p))
for(i in seq_along(p)){
  Z[i] = rbinom(1,1,p[i])
}

#joint distribution
matrix = matrix(c(1,0.8,0.8,1),2,2)
sigma = mvrnorm(n,c(0,0),matrix)
  
# generate the treatment effect D:
fx_D0 = 0.1*X_d[,3] + 0.4*sin(X_d[,2]) + 0.4*abs(X_d[,3])- 1 + sigma[,1]  
ibu <- rnorm(n,0,1)
D_0 = ifelse(fx_D0>ibu,1,0)
  
fx_D1 = fx_D0 + 2 + 0.8*X_d[,2]^2
D_1 = ifelse(fx_D1>ibu,1,0)
D = (1-Z)*D_0 + Z*D_1
  
# generate continuous outcome Y:
Y_0 = 0.4*(X_d[,1])^2 + 0.1*abs(X_d[,2]) + 0.2*cos(X_d[,3]) + sigma[,2]   
Y_1 = Y_0 + 1 + 0.1*X_d[,1] + 0.3*X_d[,3]^2
Y = (1-D)*Y_0 + D*Y_1

# The output
est = IPPW_IV(Y,Z,X_d,D,min.controls = 0.01, max.controls = 100,caliper=FALSE,  
      calipersd = 0.2, classical = FALSE,gamma = 0.1,lower.bound=0,upper.bound=3,
      by=0.01,alpha=0.05)$estimate
est


}
