\name{nlps}

\alias{nlpsLM}
\alias{nlpsGLM}
\alias{nlpsAFTM}
 
\title{Non-local prior based single-step variable selection for high-dimensional data}

\description{\code{nlpsLM}, \code{nlpsGLM}, \code{nlpsAFTM} perform variable selection in a single iteration respectively for continuous, binary and survival outcomes, combining the computational efficiency of the 'structured screen-and-select' variable selection strategy based on some association learning and the parsimonious uncertainty quantification provided by the use of non-local priors (see the References).
}

\usage{
nlpsLM(y, x, cor_xy, prior = c("mom", "imom", "emom", "zellner", 
  "horseshoe"), tau, priorDelta = modelbbprior(1,1), 
  k0, rxx, niter = 2000, verbose = F, 
  tau.hs.method = "halfCauchy", sigma.hs.method = "Jeffreys" )

nlpsGLM(y, x, mmle_xy, prior = c("mom", "imom", "zellner"), 
  tau, priorDelta = modelbbprior(1,1), 
  k0, rxx, niter = 2000, verbose = F )

nlpsAFTM(y, event, x, mu_xy, prior = c("mom", "imom", "emom", 
  "zellner"), tau, priorDelta = modelbbprior(1,1), 
  k0, rxx, niter = 2000, verbose = F )
}

\arguments{
  \item{y}{The vector of continuous response (phenotype) for linear models (LM), or binary response (phenotype) for generalized linear models (GLM), or survival times for accelerated failure time models (AFTM). Binary response values must be 0 or 1.}
  \item{event}{Only for AFTM. The vector of status indicator for the survival data.}
  \item{x}{The design matrix with subjects in rows and independent variables (SNPs) in columns. Missing values are not accepted currently.}
  \item{cor_xy}{Only for LM. Vector of (Pearson) correlations of \code{y} with the columns of \code{x}.}
  \item{mmle_xy}{Only for GLM. Vector of maximum marginal likelihood estimates of the regression parameters for the variables (SNPs) in \code{x}. These may be obtained from individual GLM fits of \code{y} with the columns of \code{x}.}
  \item{mu_xy}{Only for AFTM. Vector of marginal utility estimates of the variables (SNPs) in \code{x}. These may be obtained by fitting AFT model to \code{y} with individual columns of \code{x} using the \code{survreg} function of the package \code{survival}. }
  \item{prior}{\code{"mom"} for pMOM prior, \code{"imom"} for piMOM prior, \code{"emom"} for peMOM prior, \code{"zellner"} for Zellner's g-prior, \code{"horseshoe"} for horseshoe prior. For GLM, \code{"zellner"} considers group Zellner prior and \code{"emom"} and \code{"horseshoe"} are not available. For AFTM, \code{"horseshoe"} is not available. }
  \item{tau}{the value of the scale parameter tau of the non-local prior. }
  \item{priorDelta}{Prior for model space. Defaults to \code{modelbbprior(1,1)}, which is beta-binomial(1,1) prior.}
  \item{k0}{GWASinlps tuning parameter denoting the number of leading SNPs (see Details).}
  \item{rxx}{GWASinlps tuning parameter denoting the correlation threshold to determine leading sets (see References).}
  \item{niter}{Number of MCMC iterations for non-local prior based Bayesian variable selection. Defaults to 2000.}
  \item{verbose}{If TRUE, prints result from the iterations progressively. FALSE by default.}
  \item{tau.hs.method}{Necessary only when \code{prior="horseshoe"}. See \code{horseshoe} function reference.}
  \item{sigma.hs.method}{Necessary only when \code{prior="horseshoe"}. See \code{horseshoe} function reference.}  
}

\details{The \code{nlpsLM}, \code{nlpsGLM} and \code{nlpsAFTM} functions perform SNP selection in one iteration for continuous data, binary data, and survival data, respectively. The \code{GWASinlps} function repeatedly calls these functions. For details of the procedure, see the reference for the \code{GWASinlps} function.
}

\value{
  A list with elements
    \item{hppm }{The names of variables (SNPs) appearing in the highest posterior probability model (HPPM) of at least one leading set. }
    \item{not.selected }{The names of variables (SNPs) appearing in at least one leading set but in none of the HPPMs.}
}

\references{
Sanyal et al. (2019), "GWASinlps: Non-local prior based iterative SNP selection tool for genome-wide association studies". Bioinformatics, 35(1), 1-11.

Sanyal, N. (2022). "Iterative variable selection for high-dimensional data with binary outcomes". arXiv preprint arXiv:2211.03190.
}

\author{Nilotpal Sanyal <nilotpal.sanyal@gmail.com>
}

\seealso{
\code{\link{GWASinlps}}, \code{\link[mombf]{modelSelection}}, \code{\link[horseshoe]{horseshoe}}
}

\examples{
n = 100
p = 1000
m = 10

# Generate design matrix (genotype matrix)
set.seed(1) 
f = runif( p, .1, .2 ) # simulate minor allele frequency
x = matrix( nrow = n, ncol = p )
colnames(x) = 1:p
for(j in 1:p)
  x[,j] = rbinom( n, 2, f[j] )

# Generate true effect sizes
causal_snps = sample( 1:p, m )
beta = rep( 0, p )
set.seed(1)
beta[causal_snps] = rnorm(m, mean = 0, sd = 2 )

# Generate continuous (phenotype) data
y.cont = x \%*\% beta + rnorm(n, 0, 1) 

# Generate binary (phenotype) data
prob = exp(x \%*\% beta)/(1 + exp(x \%*\% beta))
y.bin = sapply(1:n, function(i)rbinom(1,1,prob[i]) )

# Fix scale parameter tau 
tau = 0.022

# GWASinlps analysis
cor_xy = c(cor(x,y.cont))
names(cor_xy) = colnames(x)
nlps_cont = nlpsLM(y.cont, x, cor_xy=cor_xy, prior="mom", 
  tau=tau, k0=2, rxx=0.3, niter=10000, verbose=TRUE) 
nlps_cont

library(fastglm)
mode(x) = "double"  #needed for fastglm() function below
mmle_xy = apply( x, 2, function(z) coef( fastglm(y=y.bin, 
x=cbind(1,matrix(z)), family = binomial(link = "logit")) )[2] )
nlps_bin = nlpsGLM(y.bin, x, mmle_xy=mmle_xy, prior="mom", 
  tau=tau, k0=2, rxx=0.3, niter=10000, verbose=TRUE) 
nlps_bin
}



