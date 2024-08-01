\name{AROC.bnp}
\alias{AROC.bnp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Nonparametric Bayesian inference of the covariate-adjusted ROC curve (AROC).
}
\description{
Estimates the covariate-adjusted ROC curve (AROC) using the nonparametric Bayesian approach proposed by Inacio de Carvalho and Rodriguez-Alvarez (2018).
}
\usage{
AROC.bnp(formula.healthy, group, tag.healthy, data, scale = TRUE, 
  p = seq(0, 1, l = 101), paauc = paauccontrol(), 
  compute.lpml = FALSE, compute.WAIC = FALSE, 
  m0, S0, nu, Psi, alpha = 1, a = 2, b = 0.5, L = 10, nsim = 10000, nburn = 2000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula.healthy}{A \code{\link[stats]{formula}} object specifying the B-splines dependent Dirichlet process mixture model for the estimation of the conditional distribution function for the diagnostic test outcome in the healthy population (see Note).}
  \item{group}{A character string with the name of the variable that distinguishes healthy from diseased individuals.}
  \item{tag.healthy}{The value codifying the healthy individuals in the variable \code{group}.}
  \item{data}{Data frame representing the data and containing all needed variables.}
  \item{scale}{A logical value. If TRUE the test outcomes are scaled, i.e., are divided by the standard deviation. The default is TRUE.}
  \item{p}{Set of false positive fractions (FPF) at which to estimate the covariate-adjusted ROC curve.}
  \item{paauc}{A list of control values to replace the default values returned by the function \code{\link{paauccontrol}}. This argument is used to indicate whether the partial area under the covariate-adjusted ROC curve (pAAUC) should be computed and at which FPF.}
  \item{compute.lpml}{A logical value. If TRUE, the log pseudo marginal likelihood (LPML, Geisser and Eddy, 1979) and the conditional predictive ordinates (CPO) are computed.}
  \item{compute.WAIC}{A logical value. If TRUE, the widely applicable information criterion (WAIC, Gelman et al., 2014; Watanabe, 2010) is computed.}
  \item{m0}{A numeric vector. Hyperparameter; mean vector of the (multivariate) normal prior distribution for the mean of the normal component of the centering distribution. If missing, it is set to a vector of zeros of length \code{Q} (see Details).}
  \item{S0}{A numeric matrix. Hyperparameter; covariance matrix of the (multivariate) normal prior distribution for the mean of the normal component of the centering distribution. If missing, it is set to a diagonal matrix of dimension \code{Q}x\code{Q} with 100 in the diagonal (see Details).}
  \item{nu}{A numeric value. Hyperparameter; degrees of freedom of the Wishart prior distribution for the precision matrix of the the normal component of the centering distribution. If missing, it is set to \code{Q + 2} (see Details)}
  \item{Psi}{A numeric matrix. Hyperparameter; scale matrix of the Wishart distribution for the precision matrix of the the normal component of the centering distribution. If missing, it is set to an identity matrix of dimension \code{Q}x\code{Q} (see Details).}
  \item{alpha}{A numeric value. Precision parameter of the Dirichlet Process. The default is 1 (see Details).}
  \item{a}{A numeric value. Hyperparameter; shape parameter of the gamma prior distribution for the precision (inverse variance). The default is 2 (scaled data) (see Details).}
  \item{b}{A numeric value. Hyperparameter; rate parameter of the gamma prior distribution for the precision (inverse variance). The default is 0.5 (scaled data) (see Details).}
  \item{L}{A numeric value. Maximum number of mixture components for the B-splines dependent Dirichlet process mixture model. The default is 10 (see Details)}
  \item{nsim}{A numeric value. Total number of Gibbs sampler iterates (including the burn-in). The default is 10000.}
  \item{nburn}{A numeric value. Number of burn-in iterations. The default is 2000.}
  }
\details{
Estimates the covariate-adjusted ROC curve (AROC) defined as

\deqn{AROC\left(t\right) = Pr\{1 - F_{\bar{D}}(Y_D | \mathbf{X}_{D}) \leq t\},}

where \eqn{F_{\bar{D}}(\cdot|\mathbf{X}_{\bar{D}})} denotes the conditional distribution function for \eqn{Y_{\bar{D}}} conditional on the vector of covariates \eqn{\mathbf{X}_{\bar{D}}}. In particular, the method implemented in this function combines a B-splines dependent Dirichlet process mixture model to estimate \eqn{F_{\bar{D}}(\cdot|\mathbf{X}_{\bar{D}})} and the Bayesian bootstrap (Rubin, 1981) to estimate the outside probability. More precisely, and letting \eqn{\{(\mathbf{x}_{\bar{D}i},y_{\bar{D}i})\}_{i=1}^{n_{\bar{D}}}} be a random sample from the nondiseased population

\deqn{F_{\bar{D}}(y_{\bar{D}i}|\mathbf{X}_{\bar{D}}=\mathbf{x}_{\bar{D}i}) = \sum_{l=1}^{L}\omega_l\Phi(y_{\bar{D}i}\mid\mu_{l}(\mathbf{x}_{\bar{D}i}),\sigma_l^2),}

where \eqn{\mu_{l}(\mathbf{x}_{\bar{D}i}) = \mathbf{z}_{\bar{D}i}^{T}\mathbf{\beta}_l} and \eqn{L} is pre-specified (maximum number of mixture components). The \eqn{\omega_l}'s result from a truncated version of the stick-breaking construction (\eqn{\omega_1=v_1}; \eqn{\omega_l=v_l\prod_{r<l}(1-v_r)}, \eqn{l=2,\ldots,L}; \eqn{v_1,\ldots,v_{L-1}\sim} Beta \eqn{(1,\alpha)}; \eqn{v_L=1}), \eqn{\mathbf{\beta}_l\sim N_{Q}(\mathbf{m},\mathbf{S})}, and \eqn{\sigma_l^{-2}\sim\Gamma(a,b)}. It is assumed that \eqn{\mathbf{m} \sim N_{Q}(\mathbf{m}_0,\mathbf{S}_0)} and \eqn{\mathbf{S}^{-1}\sim W(\nu,(\nu\Psi)^{-1})}. Here \eqn{W(\nu,(\nu\Psi)^{-1})} denotes a Wishart distribution with \eqn{\nu} degrees of freedom and expectation \eqn{\Psi^{-1}}, and \eqn{Q} denotes the dimension of vector \eqn{\mathbf{z}_{\bar{D}i}}. For a detailed description, we refer to Inacio de Carvalho and Rodriguez-Alvarez (2018).
}
\value{
As a result, the function provides a list with the following components:
\item{call}{The matched call.}
\item{p}{Set of false positive fractions (FPF) at which the pooled ROC curve has been estimated.}
\item{ROC}{Estimated covariate-adjusted ROC curve (AROC) (posterior mean), and 95\% pointwise posterior credible band.}
\item{AUC}{Estimated area under the covariate-adjusted ROC curve (AAUC) (posterior mean), and 95\% pointwise posterior credible band.}
\item{pAUC}{If required, estimated partial area under the covariate-adjusted ROC curve (pAAUC) (posterior mean), and 95\% pointwise posterior credible band.}
\item{lpml}{If required, list with two components: the log pseudo marginal likelihood (LPML) and the conditional predictive ordinates (CPO).}
\item{WAIC}{If required, widely applicable information criterion (WAIC).}
\item{fit}{Results of the fitting process. It is a list with the following components: (1) \code{mm}: information needed to construct the model matrix associated with the B-splines dependent Dirichlet process mixture model. (2) \code{beta}: array of dimension \code{N}x\code{L}x\code{Q} with the sampled regression coefficients. Here, \code{N} is the number of Gibbs sampler iterates after burn-in, \code{L} is the maximum number of mixture components, and \code{Q} is the dimension of vector \eqn{\mathbf{Z}_{\bar{D}}} (see also Details). (3) \code{sd}: matrix of dimension \code{N}x\code{L} with the sampled variances. Here, \code{N} is the number of Gibbs sampler iterates after burn-in, and \code{L} is the maximum number of mixture components (see also Details). (4) \code{probs}: matrix of dimension \code{N}x\code{L} with the sampled components' weights. Here, \code{N} is the number of Gibbs sampler iterates after burn-in and \code{L} is the maximum number of mixture components (see also Details).}
\item{data_model}{List with the data used in the fit: observed diagnostic test outcome and B-spline design matrices, separately for the healthy and diseased groups.}
}
\references{
Inacio de Carvalho, V., and Rodriguez-Alvarez, M. X. (2018). Bayesian nonparametric inference for the covariate-adjusted ROC curve. arXiv preprint arXiv:1806.00473.

Rubin, D. B. (1981). The Bayesian bootstrap. The Annals of Statistics, 9(1), 130-134.
}
%\author{
%%  ~~who you are~~
%}
\note{
The input argument \code{formula.healthy} is similar to that used for the \code{\link{glm}} function, except that flexible specifications can be added by means of function \code{f()}. For instance, specification \eqn{y \sim x1 + f(x2, K = 3)} would assume a linear effect of \code{x1} and the effect of \code{x2} would be modeled using B-splines basis functions. The argument \code{K = 3} indicates that \code{3} internal knots will be used, with the quantiles of \code{x2} used for their location. Categorical variables (factors) can be also incorporated, as well as factor-by-curve interaction terms. For example, to include the interaction between \code{age} and \code{gender} we need to specify \eqn{y \sim gender + f(age, by = gender, K = 3)}.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{AROC.bnp}}, \code{\link{AROC.bsp}}, \code{\link{AROC.sp}}, \code{\link{AROC.kernel}}, \code{\link{pooledROC.BB}} or \code{\link{pooledROC.emp}}.
}
\examples{
library(AROC)
data(psa)
# Select the last measurement
newpsa <- psa[!duplicated(psa$id, fromLast = TRUE),]

# Log-transform the biomarker
newpsa$l_marker1 <- log(newpsa$marker1)
\donttest{
m0 <- AROC.bnp(formula.healthy = l_marker1 ~ f(age, K = 0),
group = "status", tag.healthy = 0, data = newpsa, scale = TRUE,
p = seq(0,1,l=101), compute.lpml = TRUE, compute.WAIC = TRUE,
a = 2, b = 0.5, L = 10, nsim = 5000, nburn = 1000)

summary(m0)

plot(m0)
}
\dontshow{
m0 <- AROC.bnp(formula.healthy = l_marker1 ~ f(age, K = 0),
group = "status", tag.healthy = 0, data = newpsa, scale = TRUE,
p = seq(0,1,l=101), compute.lpml = TRUE, compute.WAIC = TRUE,
a = 2, b = 0.5, L = 10, nsim = 500, nburn = 100)

summary(m0)

plot(m0)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
