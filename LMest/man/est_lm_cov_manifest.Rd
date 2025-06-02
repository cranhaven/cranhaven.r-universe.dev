\name{est_lm_cov_manifest}

\alias{est_lm_cov_manifest}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimate LM model with covariates in the measurement model}

\description{Main function for estimating LM model with covariates in the measurement model based on a global logit parameterization.  \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{lmest}} \bold{function}.}

\usage{
est_lm_cov_manifest(S, X, yv = rep(1,nrow(S)), k, q = NULL, mod = c("LM", "FM"),
                    tol = 10^-8, maxit = 1000, start = 0, mu = NULL, al = NULL,
                    be = NULL, si = NULL, rho = NULL, la = NULL, PI = NULL,
                    output = FALSE, out_se = FALSE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{array of available configurations (n x TT) with categories starting from 0}
  \item{X}{array (n x TT x nc) of covariates with eventually includes lagged response (nc = number of covariates)}
  \item{yv}{vector of frequencies of the available configurations}
  \item{k}{number of latent states}
  \item{q}{number of support points for the AR(1) process}
  \item{mod}{model ("LM" = Latent Markov with stationary transition, "FM" = finite mixture)}
  \item{tol}{tolerance for the convergence (optional) and tolerance of conditional probability if tol>1 then return}
  \item{maxit}{maximum number of iterations of the algorithm}
  \item{start}{type of starting values (0 = deterministic, 1 = random, 2 = initial values in input)}
 \item{mu}{starting value for mu (optional)}
 \item{al}{starting value for al (optional)}
 \item{be}{starting value for be (optional)}
 \item{si}{starting value for si when mod="FM" (optional)}
 \item{rho}{starting value for rho when mod="FM" (optional)}
 \item{la}{starting value for la (optional)}
 \item{PI}{starting value for PI (optional)}
 \item{output}{to return additional output (PRED0, PRED1)}
 \item{out_se}{TRUE for computing information matrix and standard errors}
}

\value{
	\item{mu }{vector of cutpoints}
	\item{al }{support points for the latent states}
	\item{be }{estimate of the vector of regression parameters}
	\item{si }{sigma of the AR(1) process (mod = "FM")}
	\item{rho }{parameter vector for AR(1) process (mod = "FM")}
	\item{la }{vector of initial probabilities}
	\item{PI }{transition matrix}
 	\item{lk }{maximum log-likelihood}
 	\item{np }{number of parameters}
 	\item{aic }{value of AIC index}
 	\item{bic }{value of BIC index}
 	\item{PRED0 }{prediction of latent state}
 	\item{PRED1 }{prediction of the overall latent effect}
 	\item{sebe }{standard errors for the regression parameters be}
 	\item{selrho }{standard errors for logit type transformation of rho}
 	\item{J1 }{information matrix}
	\item{call}{command used to call the function}
}

\references{Bartolucci, F., Farcomeni, A. and Pennoni, F. (2013) \emph{Latent Markov Models for Longitudinal Data}, Chapman and Hall/CRC press.

Bartolucci, F., Bacci, S. and Pennoni, F. (2014) Longitudinal analysis of the 	self-reported health status by mixture latent autoregressive models, \emph{Journal of the Royal Statistical Society - series C}, \bold{63}, pp. 267-288}
\author{Francesco Bartolucci, Silvia Pandolfi - University of Perugia (IT)}

\examples{

\dontrun{
# Example based on self-rated health status (SRHS) data

# load SRHS data
data(data_SRHS_long)
dataSRHS <- data_SRHS_long
head(dataSRHS)

res <- long2matrices(dataSRHS$id, X = cbind(dataSRHS$gender-1,
 dataSRHS$race == 2 | dataSRHS$race == 3, dataSRHS$education == 4,
dataSRHS$education == 5, dataSRHS$age-50, (dataSRHS$age-50)^2/100),
Y = dataSRHS$srhs)

X <- res$XX
S <- 5-res$YY

# *** fit stationary LM model
res0 <- vector("list", 10)
tol <- 10^-6;
for(k in 1:10){
  res0[[k]] <- est_lm_cov_manifest(S, X, k, 1, mod = "LM", tol)
   save.image("example_SRHS.RData")
}

# *** fit the mixture latent auto-regressive model
tol <- 0.005
res <- vector("list",4)
k <- 1
q <- 51
res[[k]] <- est_lm_cov_manifest(S, X, k, q, mod = "FM", tol, output = TRUE)
for(k in 2:4) res[[k]] <- est_lm_cov_manifest(S, X, k, q = 61, mod = "FM", tol, output = TRUE)
}
}
