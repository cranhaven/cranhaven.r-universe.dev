\name{bootstrap}
\alias{bootstrap}
\alias{bootstrap.LMbasic}
\alias{bootstrap.LMbasiccont}
\alias{bootstrap.LMlatent}
\alias{bootstrap.LMlatentcont}

\title{Parametric bootstrap}
\description{
Function that performs bootstrap parametric resampling to compute standard errors for the parameter estimates.
}
\usage{
bootstrap(est, ...)
\method{bootstrap}{LMbasic}(est,  B = 100, seed = NULL, ...)
\method{bootstrap}{LMbasiccont}(est, B=100, seed = NULL, ...)
\method{bootstrap}{LMlatent}(est, B = 100, seed = NULL, ...)
\method{bootstrap}{LMlatentcont}(est, B = 100, seed = NULL, ...)
}
\arguments{
  \item{est}{an object obtained from a call to \code{\link{lmest}} and \code{\link{lmestCont}}}
  \item{B}{number of bootstrap samples}
  \item{seed}{an integer value with the random number generator state}
  \item{\dots}{further arguments}
}

\value{
Average of bootstrap estimates and standard errors for the model parameters in \code{est} object.
}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}

\examples{
\dontrun{

# LM model for categorical responses with covariates on the latent model

data("data_SRHS_long")
SRHS <- data_SRHS_long[1:2400,]

# Categories rescaled to vary from 0 (“poor”) to 4 (“excellent”)

SRHS$srhs <- 5 - SRHS$srhs

out1 <- lmest(responsesFormula = srhs ~ NULL,
              index = c("id","t"),
              data = SRHS,
              k = 3,
              tol = 1e-8,
              start = 1,
              modBasic = 1,
              out_se = TRUE,
              seed = 123)

boot1 <- bootstrap(out1)

out2 <- lmest(responsesFormula = srhs ~ NULL,
              latentFormula =  ~
              I(gender - 1) +
              I( 0 + (race == 2) + (race == 3)) +
              I(0 + (education == 4)) +
              I(0 + (education == 5)) +
              I(age - 50) + I((age-50)^2/100),
              index = c("id","t"),
              data = SRHS,
              k = 2,
              paramLatent = "multilogit",
              start = 0)

boot2 <- bootstrap(out2)

# LM model for continous responses without covariates 

data(data_long_cont)

out3 <- lmestCont(responsesFormula = Y1 + Y2 + Y3 ~ NULL,
                  index = c("id", "time"),
                  data = data_long_cont,
                  k =3,
                  modBasic=1,
                  tol=10^-5)

boot3 <- bootstrap(out3)

# LM model for continous responses with covariates 

out4 <- lmestCont(responsesFormula = Y1 + Y2 + Y3 ~ NULL,
                  latentFormula = ~ X1 + X2,
                  index = c("id", "time"),
                  data = data_long_cont,
                  k = 3,
                  output=TRUE)
                  
boot4 <- bootstrap(out4)
}
}
