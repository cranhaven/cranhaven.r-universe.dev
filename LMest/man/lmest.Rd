\name{lmest}
\alias{lmest}
\title{Estimate Latent Markov models  for categorical responses}
\description{Main function for estimating Latent Markov (LM) models for categorical responses.}
\usage{
lmest(responsesFormula = NULL, latentFormula = NULL,
      data, index, k = 1:4, start = 0,
      modSel = c("BIC", "AIC"), modBasic = 0,
      modManifest = c("LM", "FM"),
      paramLatent = c("multilogit", "difflogit"),
      weights = NULL, tol = 10^-8, maxit = 1000,
      out_se = FALSE, q = NULL, output = FALSE,
      parInit = list(piv = NULL, Pi = NULL, Psi = NULL,
                     Be = NULL, Ga = NULL, mu = NULL,
                     al = NULL, be = NULL, si = NULL,
                     rho = NULL, la = NULL, PI = NULL,
                     fixPsi = FALSE),
      fort = TRUE, seed = NULL, ntry = 0)
}
\arguments{
  \item{responsesFormula}{a symbolic description of the model to fit. A detailed description is given in the ‘Details’ section}
  \item{latentFormula}{a symbolic description of the model to fit. A detailed description is given in the ‘Details’ section}
  \item{data}{a \code{data.frame} in long format}
    \item{index}{a character vector with two elements, the first indicating the name of the unit identifier, and the second the time occasions}
  \item{k}{an integer vector specifying the number of latent states (default: \code{1:4})}
  \item{start}{type of starting values (0 = deterministic, 1 = random, 2 = initial values in input)}
    \item{modSel}{a string indicating the model selection criteria: "BIC" for Bayesian Information Criterion and "AIC" for Akaike Information Criterion Criterion}
  \item{modBasic}{model on the transition probabilities (0 for time-heterogeneity, 1 for time-homogeneity, from 2 to (TT-1) partial time-homogeneity of a certain order)}
  \item{modManifest}{ model for manifest distribution when covariates are included in the measurement model (\code{"LM"} = Latent Markov with stationary transition, \code{"FM"} = finite mixture model where a mixture of AR(1) processes is  estimated with common variance and specific correlation coefficients). }
  \item{paramLatent}{type of parametrization for the transition probabilities ("multilogit" = standard multinomial logit for every row of the transition matrix, "difflogit" = multinomial logit based on the difference between two sets of parameters)}
  \item{weights}{an optional vector of weights for the available responses}
  \item{tol}{tolerance level for convergence}
  \item{maxit}{maximum number of iterations of the algorithm}
  \item{out_se}{to compute the information matrix and standard errors}
  \item{q}{number of support points for the AR(1) process (if modManifest ="FM")}
   \item{output}{to return additional output: \code{V}, \code{Ul}, \code{S}, \code{yv}, \code{Pmarg} for the basic LM model and for the LM with covariates on the latent model (\code{\link{LMbasic-class}} and \code{\link{LMlatent-class}}) and \code{V}, \code{PRED1}, \code{S}, \code{yv}, \code{Pmarg} for the LM model with covariates in the measurement model (\code{\link{LMmanifest-class}})}
  \item{parInit}{list of initial model parameters when \code{"start = 2"}. For the list of parameters look at \code{\link{LMbasic-class}}, \code{\link{LMlatent-class}} and \code{\link{LMmanifest-class}}}
  \item{fort}{to use fortran routines when possible}
  \item{seed}{an integer value with the random number generator state}
  \item{ntry}{to set the number of random initializations}
}
\details{
\code{lmest} is a general function for estimating LM models for categorical responses. The function requires data in long format and two additional columns indicating the unit identifier and the time occasions.

Covariates are allowed to affect manifest distribution (measurement model) or  the initial and transition probabilities (latent model). Two different formulas are employed to specify the different LM models, \code{responsesFormula} and \code{latentFormula}:

\itemize{
\item{\code{responsesFormula}} is used  to specify the measurament model:
\itemize{
\item{\code{responsesFormula = y1 + y2 ~ NULL}} \cr  the LM model without covariates and two responses (\code{y1} and \code{y2}) is specified;
\item{\code{responsesFormula = NULL}}\cr  all the columns in the data except the \code{"id"} and \code{"time"} columns are used as responses to estimate the LM model without covariates;
\item{\code{responsesFormula = y1 ~ x1 + x2}}\cr the univariate LM model with  response (\code{y1}) and two covariates (\code{x1} and \code{x2}) in the measurement model is specified;
}

\item{\code{latentFormula}} is used to specify the LM model with covariates in the latent model:

\itemize{
\item{\code{responsesFormula = y1 + y2 ~ NULL}}  \cr
\code{latentFormula = ~ x1 + x2 | x3 + x4} \cr the LM model with two responses (\code{y1} and \code{y2}) and  two covariates affecting the initial probabilities (\code{x1} and \code{x2}) and other two affecting the transition probabilities (\code{x3} and \code{x4}) is specified;
\item{\code{responsesFormula = y1 + y2 ~ NULL}} \cr \code{latentFormula = ~ 1 | x1 + x2} \cr (or \code{latentFormula = ~ NULL | x1 + x2})  \cr the covariates affect only the transition probabilities and an intercept is specified for the intial probabilities;
\item{\code{responsesFormula = y1 + y2 ~ NULL}} \cr \code{latentFormula = ~ x1 + x2} \cr the LM model with two covariates (\code{x1} and \code{x2}) affecting both the initial and transition probabilities is specified;
\item{\code{responsesFormula = y1 + y2 ~ NULL}} \cr \code{latentFormula = ~ NULL | NULL} \cr (or \code{latentFormula = ~ 1 | 1}) \cr the LM model with only an intercept on the initial and transition probabilities is specified.
}}



The function also allows us to deal with missing responses, including drop-out and non-monotonic missingness, under the missing-at-random assumption. Missing values for the covariates are not allowed. 

The LM model with individual covariates in the measurement model is estimated only for complete univariate responses. In such a case, two possible formulations are allowed:  modManifest="LM" is used to estimate the model illustrated in Bartolucci et al. (2017), where the latent process is of first order with initial probabilities equal to those of the stationary distribution of the chain; modManifest="FM" is used to estimate a model relying on the assumption that the distribution of the latent process is a mixture of AR(1) processes with common variance and specific correlation coefficients. This model is illustrated in Bartolucci et al. (2014).

For continuous outcomes see the function \code{\link{lmestCont}}.

}




\value{
Returns an object of class \code{'LMbasic'} for the model without covariates (see \code{\link{LMbasic-class}}), or an object of class \code{'LMmanifest'} for the model with covariates on the manifest model (see \code{\link{LMmanifest-class}}), or an object of class \code{'LMlatent'} for the model with covariates on the latent model (see \code{\link{LMlatent-class}}).}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}


\references{
Bartolucci, F., Bacci, S., and Pennoni, F. (2014). Longitudinal analysis of the self-reported health status by mixture latent autoregressive models, \emph{Journal of the Royal Statistical Society - series C}, \bold{63}, pp. 267-288.

Bartolucci F., Pandolfi S., and Pennoni F. (2017) LMest: An R Package for Latent Markov Models for Longitudinal Categorical
Data, \emph{Journal of Statistical Software}, \bold{81}(4), 1-38.

Bartolucci, F., Farcomeni, A., and Pennoni, F. (2013) \emph{Latent Markov Models for Longitudinal Data}, Chapman and Hall/CRC press.

%Bartolucci, F., Pennoni, F. (2007). A class of latent Markov models for Capture-Recapture data allowing for Time, Heterogeneity and Behavior effects, \emph{Biometrics}, \bold{63}, 568-578.
}


\examples{
### Basic LM model

data("data_SRHS_long")
SRHS <- data_SRHS_long[1:2400,]

# Categories rescaled to vary from 0 (“poor”) to 4 (“excellent”)

SRHS$srhs <- 5 - SRHS$srhs

out <- lmest(responsesFormula = srhs ~ NULL,
             index = c("id","t"),
             data = SRHS,
             k = 3,
             start = 1,
             modBasic = 1,
             seed = 123)
out
summary(out)




\dontrun{

## Basic LM model with model selection using BIC

out1 <- lmest(responsesFormula = srhs ~ NULL,
              index = c("id","t"),
              data = SRHS,
              k = 1:5,
              tol = 1e-8,
              modBasic = 1,
              seed = 123, ntry = 2)
out1
out1$Bic

# Basic LM model with model selection using AIC

out2 <- lmest(responsesFormula = srhs ~ NULL,
              index = c("id","t"),
              data = SRHS,
              k = 1:5,
              tol = 1e-8,
              modBasic = 1,
              modSel = "AIC",
              seed = 123, ntry = 2)
out2
out2$Aic

# Criminal data

data(data_criminal_sim)
data_criminal_sim = data.frame(data_criminal_sim)

responsesFormula <- lmestFormula(data = data_criminal_sim,response = "y")$responsesFormula


out3 <- lmest(responsesFormula = responsesFormula,
              index = c("id","time"),
              data =data_criminal_sim,
              k = 1:7,
              modBasic = 1,
              tol = 10^-4)
out3

# Example of drug consumption data

data("data_drug")
long <- data_drug[,-6]-1
long <- data.frame(id = 1:nrow(long),long)
long <- reshape(long,direction = "long",
                idvar = "id",
                varying = list(2:ncol(long)))

out4 <- lmest(index = c("id","time"),
              k = 3, 
              data = long,
              weights = data_drug[,6],
              modBasic = 1)

out4
summary(out4)

### LM model with covariates in the latent model
# Covariates: gender, race, educational level (2 columns), age and age^2

out5 <- lmest(responsesFormula = srhs ~ NULL,
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

out5
summary(out5)

### LM model with the above covariates in the measurement model (stationary model)

out6 <- lmest(responsesFormula = srhs ~ -1 +
              I(gender - 1) +
              I( 0 + (race == 2) + (race == 3)) +
              I(0 + (education == 4)) +
              I(0 + (education == 5)) + I(age - 50) +
              I((age-50)^2/100),
              index = c("id","t"),
              data = SRHS,
              k = 2,
              modManifest = "LM",
              out_se = TRUE,
              tol = 1e-8,
              start = 1,
              seed = 123)
out6
summary(out6)

#### LM model with covariates in the measurement model (mixture latent auto-regressive model)

out7 <- lmest(responsesFormula = srhs ~ -1 +
              I(gender - 1) +
              I( 0 + (race == 2) + (race == 3)) +
              I(0 + (education == 4)) +
              I(0 + (education == 5)) + I(age - 50) +
              I((age-50)^2/100),
              index = c("id","t"),
              data = SRHS,
              k = 2,
              modManifest = "FM", q = 61,
              out_se = TRUE,
              tol = 1e-8)
out7
summary(out7)
}
}

