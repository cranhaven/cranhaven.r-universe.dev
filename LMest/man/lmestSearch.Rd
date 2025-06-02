\name{lmestSearch}
\alias{lmestSearch}

\title{Search for the global maximum of the log-likelihood}
\description{
Function that searches for the global maximum of the log-likelihood of different models and selects the optimal number of states.}

\usage{
lmestSearch(responsesFormula = NULL, latentFormula = NULL,
            data, index, k,
            version = c("categorical", "continuous"),
            weights = NULL, nrep = 2, tol1 = 10^-5,
            tol2 = 10^-10, out_se = FALSE, miss.imp = FALSE, seed = NULL, ...)
}
\arguments{
  \item{responsesFormula}{a symbolic description of the model to fit. A detailed description is given in the ‘Details’ section of \code{\link{lmest}}
}
  \item{latentFormula}{a symbolic description of the model to fit. A detailed description is given in the ‘Details’ section of \code{\link{lmest}}
}
  \item{data}{a \code{data.frame} in long format}
  \item{index}{a character vector with two elements, the first indicating the name of the unit identifier, and the second the time occasions
}
  \item{k}{a vector of integer values for the number of latent states}
  \item{weights}{an optional vector of weights for the available responses}
   \item{version}{type of responses for the LM model: "categorical" and "continuous"}
  \item{nrep}{number of repetitions of each random initialization}
  \item{tol1}{tolerance level for checking convergence of the algorithm in the random initializations}
  \item{tol2}{tolerance level for checking convergence of the algorithm in the last deterministic initialization}
  \item{out_se}{to compute the information matrix and standard errors (FALSE is the default option)}
  \item{miss.imp}{Only for continuous responses: how to deal with missing values (TRUE for imputation through the imp.mix function, FALSE for missing at random assumption)}
  \item{seed}{an integer value with the random number generator}
  \item{\dots}{additional arguments to be passed to functions \code{\link{lmest}} or \code{\link{lmestCont}}}
}
\details{
The function combines deterministic and random initializations strategy to reach the global maximum of the model log-likelihood.
It uses one deterministic initialization (\code{start=0}) and a number of random initializations (\code{start=1}) proportional to the number of latent states. The tolerance level is set equal to 10^-5. Starting from the best solution obtained in this way, a final run is performed (\code{start=2}) with a default tolerance level equal to 10^-10.

Missing responses are allowed according to the model to be estimated.
}
\value{Returns an object of class \code{'LMsearch'} with the following components:
 \item{out.single}{Output of every LM model estimated for each number of latent states given in input}
  \item{Aic}{Values the Akaike Information Criterion for each number of latent states given in input}
  \item{Bic}{Values of the Bayesian Information Criterion for each number of latent states given in input}
  \item{lkv}{Values of log-likelihood for each number of latent states given in input.}

}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}

\references{
%Bacci, S., Pandolfi, S., Pennoni, F. (2014). A comparison of some criteria for states selection in the latent Markov model for longitudinal data, \emph{Advances in Data Analysis and Classification}, \bold{8}, 125-145.
Bartolucci F., Pandolfi S., Pennoni F. (2017) LMest: An R Package for Latent Markov Models for Longitudinal Categorical
Data, \emph{Journal of Statistical Software}, \bold{81}(4), 1-38.

Bartolucci, F., Farcomeni, A. and Pennoni, F. (2013) \emph{Latent Markov Models for Longitudinal Data}, Chapman and Hall/CRC press.}


\examples{

### Example with data on drug use in wide format

data("data_drug")
long <- data_drug[,-6]

# add labels referred to the identifier

long <- data.frame(id = 1:nrow(long),long)

# reshape data from the wide to the long format

long <- reshape(long,direction = "long",
                idvar = "id",
                varying = list(2:ncol(long)))

out <- lmestSearch(data = long,
                   index = c("id","time"),
                   version = "categorical",
                   k = 1:3,
                   weights = data_drug[,6],
                   modBasic = 1,
                   seed = 123)

out
summary(out$out.single[[3]])

\dontrun{

### Example with data on self rated health

# LM model with covariates in the measurement model

data("data_SRHS_long")
SRHS <- data_SRHS_long[1:1000,]

# Categories rescaled to vary from 1 (“poor”) to 5 (“excellent”)

SRHS$srhs <- 5 - SRHS$srhs

out1 <- lmestSearch(data = SRHS,
                    index = c("id","t"),
              version = "categorical",
             responsesFormula = srhs ~ -1 +
             I(gender - 1) +
             I( 0 + (race == 2) + (race == 3)) +
             I(0 + (education == 4)) +
             I(0 + (education == 5)) + I(age - 50) +
             I((age-50)^2/100),
                   k = 1:2,
                   out_se = TRUE,
                   seed = 123)
summary(out1)
summary(out1$out.single[[2]])
                   }
}
