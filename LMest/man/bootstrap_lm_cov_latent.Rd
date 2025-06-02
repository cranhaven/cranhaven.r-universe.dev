\name{bootstrap_lm_cov_latent}

\alias{bootstrap_lm_cov_latent}

\title{Parametric bootstrap for LM models with individual covariates in the latent model}

\description{Function that performs bootstrap parametric resampling to compute standard errors for the parameter estimates. \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{bootstrap}} \bold{function}.}

\usage{
bootstrap_lm_cov_latent(X1, X2, param = "multilogit", Psi, Be, Ga, B = 100,
                        fort = TRUE)}

\arguments{
  \item{X1 }{matrix of covariates affecting the initial probabilities (n x nc1)}
  \item{X2 }{array of covariates affecting the transition probabilities (n x TT-1 x nc2)}
  \item{param }{type of parametrization for the transition probabilities ("multilogit" = standard multinomial logit for every row of the transition matrix, "difflogit" = multinomial logit based on the difference between two sets of parameters)}
  \item{Psi }{array of conditional response probabilities (mb x k x r)}
  \item{Be }{parameters affecting the logit for the initial probabilities}
  \item{Ga }{parametes affecting the logit for the transition probabilities}
  \item{B}{number of bootstrap samples}
  \item{fort}{to use fortran routine when possible (FALSE for not use fortran)}
}

\value{
  \item{mPsi }{average of bootstrap estimates of the conditional response probabilities}
  \item{mBe }{average of bootstrap estimates of the parameters affecting the logit for the initial probabilities}
  \item{mGa }{average of bootstrap estimates of the parameters affecting the logit for the transition probabilities}
  \item{sePsi }{standard errors for the conditional response probabilities }
  \item{seBe }{standard errors for the parameters in Be}
  \item{seGa }{standard errors for the parameters in Ga }
}

\author{Francesco Bartolucci, Silvia Pandolfi - University of Perugia (IT)}

\examples{
\dontrun{
# Example based on self-rated health status (SRHS) data
# load SRHS data
data(data_SRHS_long)
dataSRHS <- data_SRHS_long

TT <- 8
head(dataSRHS)
res <- long2matrices(dataSRHS$id, X = cbind(dataSRHS$gender-1,
dataSRHS$race == 2 | dataSRHS$race == 3, dataSRHS$education == 4,
dataSRHS$education == 5, dataSRHS$age-50, (dataSRHS$age-50)^2/100),
Y = dataSRHS$srhs)

# matrix of responses (with ordered categories from 0 to 4)
S <- 5-res$YY

# matrix of covariates (for the first and the following occasions)
# colums are: gender,race,educational level (2 columns),age,age^2)
X1 <- res$XX[,1,]
X2 <- res$XX[,2:TT,]

# estimate the model
out1 <- est_lm_cov_latent(S, X1, X2, k = 2, output = TRUE, out_se = TRUE)

out2 <- bootstrap_lm_cov_latent(X1, X2, Psi = out1$Psi, Be = out1$Be, Ga = out1$Ga, B = 1000)
}
}
