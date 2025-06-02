\name{draw}
\docType{methods}
\alias{draw}
\alias{draw.LMbasic}
\alias{draw.LMbasiccont}
\alias{draw.LMlatent}
\alias{draw.LMlatentcont}
\alias{draw.LMmixed}

\title{Draw simulated sample from a Generalized Latent Markov Model}

\description{Draw a sample for LMest objects of classes: \code{LMbasic}, \code{LMbasiccont}, \code{LMlatent}, \code{LMlatentcont}, and \code{LMmixed}}

\usage{
\method{draw}{LMbasic}(est, n = NULL, TT = NULL, format = c("long","matrices"), seed = NULL, ...)
\method{draw}{LMlatent}(est, n = NULL, TT = NULL, data, index, format = c("long", "matrices"),
                        fort = TRUE, seed = NULL, ...)
\method{draw}{LMbasiccont}(est, n = NULL, TT = NULL, format = c("long","matrices"), seed = NULL, ...)
\method{draw}{LMlatentcont}(est, n = NULL , TT = NULL, data, index, format = c("long", "matrices"),
                            fort = TRUE, seed = NULL, ...)
\method{draw}{LMmixed}(est, n = NULL, TT = NULL, format = c("long", "matrices"), seed = NULL, ...)
}

\arguments{
  \item{est}{object of class \code{LMbasic} (\code{\link{LMbasic-class}}), \code{LMlatent} (\code{\link{LMlatent-class}}), class \code{LMbasiccont} (\code{\link{LMbasiccont-class}}),  \code{LMlatentcont} (\code{\link{LMlatentcont-class}}), or \code{LMmixed} (\code{\link{LMmixed-class}})}
  \item{n}{sample size}
  \item{format}{character string indicating the format of final responses matrix}
  \item{seed}{an integer value with the random number generator state}
  \item{data}{a data frame in long format, with rows corresponding to observations and columns corresponding to covariates, a column corresponding to time occasions and a column containing the unit identifier when est is of class \code{LMlatent} or \code{LMlatentcont}}
  \item{index}{a character vector with two elements indicating the name of the "id" column as first element and the "time" column as second element when est is of class \code{LMlatent} or \code{LMlatentcont}}
  \item{fort}{to use fortran routine when possible (FALSE for not use fortran)  when est is of class \code{LMlatent} or \code{LMlatentcont}}
  \item{TT}{number of time occasions when est is of class \code{LMmixed}}
  \item{\dots}{further arguments}
}

\value{
  \item{Y}{matrix of response configurations unit by unit when est is of class \code{LMbasic} or \code{LMmixed}; array of continuous outcomes (n x TT x r) when est is of class \code{LMbasiccont} or \code{LMlatentcont}}
  \item{S}{matrix of distinct response configurations when est is of class \code{LMbasic} or \code{LMmixed}}
  \item{yv}{corresponding vector of frequencies when est is of class \code{LMbasic} or \code{LMmixed}}
  \item{piv}{vector of initial probabilities of the latent Markov chain when est is of class \code{LMbasic}}
  \item{Pi}{set of transition probabilities matrices (k x k x TT) when est is of class \code{LMbasic}}
  \item{Psi}{array of conditional response probabitlies (mb x k x r)when est is of class \code{LMbasic}}
  \item{n}{sample size}
  \item{TT}{number of time occasions} 
  \item{est}{object of class \code{LMbasic}, \code{LMlatent}, \code{LMbasiccont}, \code{LMlatentcont}, or \code{LMmixed}}
  \item{U}{matrix containing the sequence of latent states (n x TT) when est is of class \code{LMlatent} or \code{LMlatentcont}}
  \item{Psi}{array of conditional response probabilities (mb x k x r) when est is of class \code{LMlatent}}
  \item{Be}{parameters affecting the logit for the initial probabilities when est is of class \code{LMlatent} or \code{LMlatentcont}}
  \item{Ga}{parametes affecting the logit for the transition probabilitieswhen est is of class \code{LMlatent} or \code{LMlatentcont}}
  \item{latentFormula}{a symbolic description of the model to be fitted when est is of class \code{LMlatent}. Detailed description is given in \code{\link{lmest}}}
  \item{data}{a data frame in long format, with rows corresponding to observations and columns corresponding to variables, a column corresponding to time occasions and a column containing the unit identifier when est is of class \code{LMlatent} or \code{LMlatentcont}}
  \item{Mu}{array of conditional means for the response variables (r x k) when est is of class \code{LMlatentcont}}
  \item{Si}{var-cov matrix common to all states (r x r) when est is of class \code{LMlatentcont}}
  \item{latentFormula}{a symbolic description of the model to be fitted. A detailed description is given in \code{\link{lmestCont}}}
}

\author{Francesco Bartolucci, Silvia Pandolfi, Fulvia Pennoni}


\examples{
\dontrun{
# draw a sample for 1000 units and only one response variable when est is of class LMbasic
n <- 1000
TT <- 6
k <- 2
r <- 1 #number of response variables
mb <- 3 #maximum number of response categories

piv <- c(0.7,0.3)
Pi <- matrix(c(0.9,0.1,0.1,0.9), k, k)
Pi <- array(Pi, c(k, k, TT))
Pi[,,1] <- 0
Psi <- matrix(c(0.7,0.2,0.1,0.5,0.4,0.1), mb, k)
Psi <- array(Psi, c(mb, k, r))
est = list(piv=piv, Pi=Pi, Psi=Psi, n=n, TT=TT)
class(est) = "LMbasic"

out <- draw(est)


data("data_SRHS_long")
SRHS <- data_SRHS_long[1:2400,]

SRHS$srhs <- 5 - SRHS$srhs

est <- lmest(responsesFormula = srhs ~ NULL,
             index = c("id","t"),
             data = SRHS,
             k = 3)

out1 <- draw(est = est, format = "matrices", seed = 4321, n = 100)

# draw a sample for 7074 units and only one response variable when est is of class LMlatent
data(data_SRHS_long)

data_SRHS_long$srhs <- 5 - data_SRHS_long$srhs
n <- length(unique(data_SRHS_long$id))
TT <- max(data_SRHS_long$t)

est  <- lmest(responsesFormula = srhs ~ NULL,
              latentFormula =  ~
              I(gender - 1) +
              I( 0 + (race == 2) + (race == 3)) +
              I(0 + (education == 4)) +
              I(0 + (education == 5)) +
              I(age - 50) + I((age-50)^2/100),
              index = c("id","t"),
              data = data_SRHS_long,
              k = 2,
              paramLatent = "multilogit",
              start = 0)

out <- draw(est = est, data = data_SRHS_long, index = c("id","t"),
            format = "matrices",seed = 4321)

est1 = list(Psi = est$Psi, Be = est$Be, Ga = est$Ga,
            paramLatent = "multilogit",n=n,TT=TT)

attributes(est1)$latentFormula = ~
              I(gender - 1) +
              I( 0 + (race == 2) + (race == 3)) +
              I(0 + (education == 4)) +
              I(0 + (education == 5)) +
              I(age - 50) + I((age-50)^2/100)
class(est1) = "LMlatent"

out1 <- draw(est = est1, data = data_SRHS_long, index = c("id","t"),
                     format = "matrices",
                     seed = 4321)

# draw a sample for 1000 units and 3 response variable when est is of class LMbasiccont
n <- 1000
TT <- 5
k <- 2
r <- 3 #number of response variables

piv <- c(0.7,0.3)
Pi <- matrix(c(0.9,0.1,0.1,0.9), k, k)
Pi <- array(Pi, c(k, k, TT))
Pi[,,1] <- 0
Mu <- matrix(c(-2,-2,0,0,2,2), r, k)
Si <- diag(r)
est = list(piv=piv,Pi=Pi,Mu=Mu,Si=Si,n=n,TT=TT)
class(est) = "LMbasiccont"

out <- draw(est)

data(data_long_cont)

est <- lmestCont(responsesFormula = Y1 + Y2 + Y3 ~ NULL,
                 index = c("id", "time"),
                 data = data_long_cont,
                 k = 3,
                 modBasic = 1,
                 tol = 10^-5)

out2 <- draw(est = est, n = 100, format = "long", seed = 4321)

# draw a sample for 1000 units and 3 response variable when est is of class LMlatentcont
data(data_long_cont)

est <- lmestCont(responsesFormula = Y1 + Y2 + Y3~ NULL,
                 latentFormula = ~ X1 + X2,
                 index = c("id", "time"),
                 data = data_long_cont,
                 k = 3)
out <- draw(est = est, data = data_long_cont, index = c("id", "time"), format = "matrices", 
            seed = 4321)

est1 <- list(Mu = est$Mu,Si = est$Si,Be = est$Be,Ga = est$Ga,paramLatent="multilogit",n=est$n,
             TT=est$TT)
attributes(est1)$latentFormula = ~ X1 + X2
class(est1) = "LMlatentcont"
out1 <- draw(est = est1, data = data_long_cont,
                         index = c("id", "time"),
                         fort=TRUE, seed = 4321, format = "matrices")
}

# draw a sample for 1000 units and only one response variable and 5 time occasions 
# when est if of class LMmixed
k1 <- 2
k2 <- 3
la <- rep(1/k1, k1)
Piv <- matrix(1/k2, k2, k1)
Pi <- array(0, c(k2, k2, k1))
Pi[,,1] <- diag(k2)
Pi[,,2] <- 1/k2
Psi <- cbind(c(0.6,0.3,0.1), c(0.1,0.3,0.6), c(0.3,0.6,0.1))
est <- list(la=la, Piv=Piv, Pi=Pi, Psi=Psi, n=1000,TT=5)
class(est) = "LMmixed"

out <- draw(est = est)

\dontrun{
# Example based on criminal data when est if of class LMmixed
data(data_criminal_sim)
data_criminal_sim = data.frame(data_criminal_sim)

# Estimate mixed LM model for females
responsesFormula <- lmestFormula(data = data_criminal_sim,
                                  response = "y")$responsesFormula
est <- lmestMixed(responsesFormula = responsesFormula,
                  index = c("id","time"),
                  k1 = 2,
                  k2 = 2,
                  data = data_criminal_sim[data_criminal_sim$sex == 2,])

out <- draw(est = est, n = 100, TT = 6, seed = 4321)
}

}
