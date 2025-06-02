\name{est_lm_cov_latent}

\alias{est_lm_cov_latent}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Estimate LM model with covariates in the latent model}

\description{Main function for estimating the LM model with covariates in the latent model.  \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{lmest}} \bold{function}.}

\usage{est_lm_cov_latent(S, X1=NULL, X2=NULL, yv = rep(1,nrow(S)), k, start = 0, tol = 10^-8,
                  maxit = 1000, param = "multilogit", Psi, Be, Ga, fort = TRUE,
                  output = FALSE, out_se = FALSE, fixPsi = FALSE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{array of available configurations (n x TT x r) with categories starting from 0 (use NA for missing responses)}
  \item{X1}{matrix of covariates affecting the initial probabilities (n x nc1)}
  \item{X2}{array of covariates affecting the transition probabilities (n x TT-1 x nc2)}
  \item{yv}{vector of frequencies of the available configurations}
  \item{k}{number of latent states}
  \item{start}{type of starting values (0 = deterministic, 1 = random, 2 = initial values in input)}
  \item{tol}{tolerance level for checking convergence of the algorithm}
  \item{maxit}{maximum number of iterations of the algorithm}
  \item{param}{type of parametrization for the transition probabilities ("multilogit" = standard multinomial logit for every row of the transition matrix, "difflogit" = multinomial logit based on the difference between two sets of parameters)}
 \item{Psi}{intial value of the array of the conditional response probabilities (mb x k x r)}
 \item{Be}{intial value of the parameters affecting the logit for the initial probabilities (if start=2)}
 \item{Ga}{intial value of the parametes affecting the logit for the transition probabilities (if start=2)}
 \item{fort}{to use fortran routine when possible (FALSE for not use fortran)}
 \item{output}{to return additional output (V,PI,Piv,Ul)}
 \item{out_se}{to compute the information matrix and standard errors}
 \item{fixPsi}{TRUE if Psi is given in input and is not updated anymore}
}

\value{
  \item{lk }{maximum log-likelihood}
  \item{Be }{estimated array of the parameters affecting the logit for the initial probabilities}
  \item{Ga }{estimated array of the parameters affecting the logit for the transition probabilities}
  \item{Piv }{estimate of initial probability matrix}
  \item{PI }{estimate of transition probability matrices}
  \item{Psi }{estimate of conditional response probabilities}
  \item{np }{number of free parameters}
  \item{aic }{value of AIC for model selection}
  \item{bic }{value of BIC for model selection}
  \item{lkv }{log-likelihood trace at every step}
  \item{V }{array containing the posterior distribution of the latent states for each response configuration and time occasion}
  \item{Ul }{matrix containing the predicted sequence of latent states by the local decoding method}
  \item{sePsi}{standard errors for the conditional response matrix}
  \item{seBe}{standard errors for Be}
  \item{seGa}{standard errors for Ga}
  \item{call}{command used to call the function}
}

\references{Bartolucci, F., Farcomeni, A. and Pennoni, F. (2013) \emph{Latent Markov Models for Longitudinal Data},
	Chapman and Hall/CRC press.}
\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia, http://www.stat.unipg.it/bartolucci}

\examples{

\dontrun{
# Example based on self-rated health status (SRHS) data
# load SRHS data
data(data_SRHS_long)
dataSRHS = data_SRHS_long

TT <- 8
head(dataSRHS)
res <- long2matrices(dataSRHS$id, X = cbind(dataSRHS$gender-1,
dataSRHS$race == 2 | dataSRHS$race == 3, dataSRHS$education == 4,
dataSRHS$education == 5, dataSRHS$age-50, (dataSRHS$age-50)^2/100),
Y = dataSRHS$srhs)

# matrix of responses (with ordered categories from 0 to 4)
S <- 5-res$YY
n <- dim(S)[1]

# matrix of covariates (for the first and the following occasions)
# colums are: gender,race,educational level (2 columns),age,age^2)
X1 <- res$XX[,1,]
X2 <- res$XX[,2:TT,]

# estimate the model
est2f <- est_lm_cov_latent(S, X1, X2, k = 2, output = TRUE, out_se = TRUE)
summary(est2f)

# average transition probability matrix
PI <- round(apply(est2f$PI[,,,2:TT], c(1,2), mean), 4)

# Transition probability matrix for white females with high educational level
ind1 <- X1[,1] == 1 & X1[,2] == 0 & X1[,4] == 1)
PI1 <- round(apply(est2f$PI[,,ind1,2:TT], c(1,2), mean), 4)

# Transition probability matrix for non-white male, low educational level
ind2 <- (X1[,1] == 0 & X1[,2] == 1 & X1[,3] == 0 & X1[,4] == 0)
PI2 <- round(apply(est2f$PI[,,ind2,2:TT], c(1,2), mean), 4)
}
}
