\name{bootstrap_lm_cov_latent_cont}

\alias{bootstrap_lm_cov_latent_cont}

\title{Parametric bootstrap for LM models for continuous outcomes with individual covariates in the latent model}

\description{Function that performs bootstrap parametric resampling to compute standard errors for the parameter estimates.  \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{bootstrap}} \bold{function}.}

\usage{bootstrap_lm_cov_latent_cont(X1, X2, param = "multilogit", Mu, Si, Be, Ga, B = 100)}

\arguments{
  \item{X1 }{matrix of covariates affecting the initial probabilities (n x nc1)}
  \item{X2 }{array of covariates affecting the transition probabilities (n x TT-1 x nc2)}
  \item{param }{type of parametrization for the transition probabilities ("multilogit" = standard multinomial logit for every row of the transition matrix, "difflogit" = multinomial logit based on the difference between two sets of parameters)}
  \item{Mu }{matrix of conditional means for the response variables (r x k)}
  \item{Si }{var-cov matrix common to all states (r x r)}
  \item{Be }{parameters affecting the logit for the initial probabilities}
  \item{Ga }{parametes affecting the logit for the transition probabilities}
  \item{B}{number of bootstrap samples}
}

\value{
  \item{mMu }{average of bootstrap estimates of the conditional means for the response variables}
  \item{mSi }{average of bootstrap estimates of the var-cov matrix}
  \item{mBe }{average of bootstrap estimates of the parameters affecting the logit for the initial probabilities}
  \item{mGa }{average of bootstrap estimates of the parameters affecting the logit for the transition probabilities}
  \item{seMu }{standard errors for the conditional means }
  \item{seSi}{standard errors for the var-cov matrix}
  \item{seBe }{standard errors for the parameters in Be}
  \item{seGa }{standard errors for the parameters in Ga }
}

\author{Francesco Bartolucci, Silvia Pandolfi - University of Perugia (IT)}

\examples{
\dontrun{
# Example based on multivariate longitudinal continuous data

data(data_long_cont)
TT <- 5
res <- long2matrices(data_long_cont$id, X = cbind(data_long_cont$X1, data_long_cont$X2),
                    Y = cbind(data_long_cont$Y1, data_long_cont$Y2,data_long_cont$Y3))
Y <- res$YY
X1 <- res$XX[,1,]
X2 <- res$XX[,2:TT,]

# estimate the model
est <- est_lm_cov_latent_cont(Y, X1, X2, k = 3, output = TRUE)
out <- bootstrap_lm_cov_latent_cont(X1, X2, Mu = est$Mu, Si = est$Si,
                                    Be = est$Be, Ga = est$Ga, B = 1000)

}
}
