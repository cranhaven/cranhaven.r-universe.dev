\name{est_lm_cov_latent_cont}

\alias{est_lm_cov_latent_cont}

\title{Estimate LM model for continuous outcomes with covariates in the latent model}

\description{Main function for estimating the LM model for continuous outcomes with covariates in the latent model.  \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{lmestCont}} \bold{function}.}

\usage{
est_lm_cov_latent_cont(Y, X1 = NULL, X2 = NULL, yv = rep(1,nrow(Y)), k, start = 0,
                       tol = 10^-8, maxit = 1000, param = "multilogit",
                       Mu = NULL, Si = NULL, Be = NULL, Ga = NULL,
                       output = FALSE, out_se = FALSE)}

\arguments{
  \item{Y}{array of continuous outcomes (n x TT x r)}
  \item{X1}{matrix of covariates affecting the initial probabilities (n x nc1)}
  \item{X2}{array of covariates affecting the transition probabilities (n x TT-1 x nc2)}
  \item{yv}{vector of frequencies of the available configurations}
  \item{k}{number of latent states}
  \item{start}{type of starting values (0 = deterministic, 1 = random, 2 = initial values in input)}
  \item{tol}{tolerance level for checking convergence of the algorithm}
  \item{maxit}{maximum number of iterations of the algorithm}
  \item{param}{type of parametrization for the transition probabilities ("multilogit" = standard multinomial logit for every row of the transition matrix, "difflogit" = multinomial logit based on the difference between two sets of parameters)}
 \item{Mu}{initial value of the conditional means (r x k) (if start=2)}
\item{Si}{initial value of the var-cov matrix common to all states (r x r) (if start=2)}
 \item{Be}{intial value of the parameters affecting the logit for the initial probabilities (if start=2)}
 \item{Ga}{intial value of the parametes affecting the logit for the transition probabilities (if start=2)}
 \item{output}{to return additional output (V,PI,Piv,Ul)}
  \item{out_se}{to compute the information matrix and standard errors}
}

\value{
  \item{lk }{maximum log-likelihood}
  \item{Be }{estimated array of the parameters affecting the logit for the initial probabilities}
  \item{Ga }{estimated array of the parameters affecting the logit for the transition probabilities}
  \item{Mu }{estimate of conditional means of the response variables}
  \item{Si }{estimate of var-cov matrix common to all states}
  \item{np }{number of free parameters}
  \item{aic }{value of AIC for model selection}
  \item{bic }{value of BIC for model selection}
  \item{lkv }{log-likelihood trace at every step}
  \item{Piv }{estimate of initial probability matrix}
  \item{PI }{estimate of transition probability matrices}
  \item{Ul }{matrix containing the predicted sequence of latent states by the local decoding method}
   \item{call}{command used to call the function}
}

\references{Bartolucci, F., Farcomeni, A. and Pennoni, F. (2013) \emph{Latent Markov Models for Longitudinal Data}, Chapman and Hall/CRC press.}
\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia, http://www.stat.unipg.it/bartolucci}

\examples{
\dontrun{
# Example based on multivariate longitudinal continuous data

data(data_long_cont)
TT <- 5
res <- long2matrices(data_long_cont$id, X = cbind(data_long_cont$X1, data_long_cont$X2),
      Y = cbind(data_long_cont$Y1, data_long_cont$Y2, data_long_cont$Y3))
Y <- res$YY
X1 <- res$XX[,1,]
X2 <- res$XX[,2:TT,]

# estimate the model
est <- est_lm_cov_latent_cont(Y, X1, X2, k = 3, output = TRUE)
summary(est)

# average transition probability matrix
PI <- round(apply(est$PI[,,,2:TT], c(1,2), mean), 4)
PI
}
}
