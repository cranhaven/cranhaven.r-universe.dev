\name{lmestCont}
\alias{lmestCont}
\title{Estimate Latent Markov models for continuous responses}

\description{
Main function for estimating Latent Markov (LM) models for continuous outcomes under the assumption of (multivariate) Gaussian distribution of the response variables given the latent process.}
\usage{
lmestCont(responsesFormula = NULL, latentFormula = NULL,
          data, index, k = 1:4, start = 0,
          modSel = c("BIC", "AIC"), modBasic = 0,
          paramLatent = c("multilogit", "difflogit"),
          weights = NULL, tol = 10^-10,
          maxit = 5000, out_se = FALSE, output = FALSE,
          parInit = list(piv = NULL, Pi = NULL,
                         Mu = NULL, Si = NULL,
                         Be = NULL, Ga = NULL),
           fort = TRUE, seed = NULL, ntry = 0, miss.imp = FALSE)
}
\arguments{
  \item{responsesFormula}{a symbolic description of the model to be fitted. A detailed description is given in the ‘Details’ section
}
  \item{latentFormula}{a symbolic description of the model to be fitted. A detailed description is given in the ‘Details’ section
}
  \item{data}{a \code{data.frame} in long format}

    \item{index}{a character vector with two elements, the first indicating the name of the unit identifier, and the second the time occasions}
  \item{k}{an integer vector specifying the number of latent states (default: \code{1:4})}
    \item{start}{type of starting values (0 = deterministic, 1 = random, 2 = initial values in input)}
      \item{modSel}{a string indicating the model selection criteria: "BIC" for Bayesian Information Criterion and "AIC" for Akaike Information Criterion Criterion}
  \item{modBasic}{model on the transition probabilities (0 for time-heter., 1 for time-homog., from 2 to (TT-1) partial homog. of that order)}
  \item{paramLatent}{type of parametrization for the transition probabilities ("multilogit" = standard multinomial logit for every row of the transition matrix, "difflogit" = multinomial logit based on the difference between two sets of parameters)}
  \item{weights}{vector of weights}
  \item{tol}{tolerance level for convergence}
  \item{maxit}{maximum number of iterations of the algorithm}
  \item{out_se}{to compute the information matrix and standard errors (By default is set to FALSE)}
   \item{output}{to return additional output (\code{V}, \code{Ul}, \code{Pmarg}) (\code{\link{LMbasiccont-class}},\code{\link{LMlatentcont-class}},\code{\link{LMmanifestcont-class}})}
  \item{parInit}{list of initial model parameters when \code{"start = 2"}. For the list of parameters look at \code{\link{LMbasiccont-class}}, \code{\link{LMlatentcont-class}}, and \code{\link{LMmanifestcont-class}}}
  \item{fort}{to use fortran routines when possible (By default is set to TRUE)}
  \item{seed}{an integer value with the random number generator state}
  \item{ntry}{to set the number of random initializations}
  \item{miss.imp}{how to deal with missing values (TRUE for imputation through the imp.mix function, FALSE for missing at random assumption)}
}
\details{

The function \code{lmestCont} is a general function for estimating LM models for continuous responses. The function requires data in long format and two additional columns indicating the unit identifier and the time occasions.

Covariates are allowed on the initial and transition probabilities (latent model). Two different formulas are employed to specify the different LM models, \code{responsesFormula} and \code{latentFormula}:

\itemize{
\item{\code{responsesFormula}} is used to specify the measurament model:
\itemize{
\item{\code{responsesFormula = y1 + y2 ~ NULL}} \cr the LM model without covariates and two responses (\code{y1} and \code{y2}) is specified.
\item{\code{responsesFormula = NULL}} \cr  all the columns in the data except the \code{"id"} and \code{"time"} columns are used as responses to estimate the LM model without covariates;
\item{\code{responsesFormula = y1 + y2 ~ x1 + x2}} \cr the LM model with two responses (\code{y1} and \code{y2}) and  two covariates in the measurement model is specified;
}


\item{\code{latentFormula}} is used to specify the LM model with covariates in the latent model:

\itemize{
\item{\code{responsesFormula = y1 + y2 ~ NULL}} \cr  \code{latentFormula = ~ x1 + x2 | x3 + x4} \cr the LM model with two responses (\code{y1} and \code{y2}) and  two covariates affecting the initial probabilities (\code{x1} and \code{x2}) and other two affecting the transition probabilities (\code{x3} and \code{x4})  is specified;
\item{\code{responsesFormula = y1 + y2 ~ NULL}} \cr \code{latentFormula = ~ 1 | x1 + x2} \cr (or \code{latentFormula = ~ NULL | x1 + x2}) \cr  the covariates affect only the transition probabilities and an intercept is specified for the intial probabilities;
\item{\code{responsesFormula = y1 + y2 ~ NULL}} \cr  \code{latentFormula = ~ x1 + x2}\cr the LM model with two covariates (\code{x1} and \code{x2}) affecting both the initial and transition probabilities is specified;
\item{\code{responsesFormula = y1 + y2 ~ NULL}} \cr  \code{latentFormula = ~ NULL | NULL} \cr (or \code{latentFormula = ~ 1 | 1}) \cr the LM model with only an intercept on the initial and transition probabilities is specified.
}

}


The function also allows us to deal with missing responses using the \code{mix} package (Schafer, 2024) for imputing the missing values. Missing values for the covariates are not allowed.

For categorical outcomes see the function \code{\link{lmest}.}
}


\seealso{\code{\link{lmestFormula}}}

\value{
Returns an object of class \code{'LMbasiccont'} for the model without covariates (see \code{\link{LMbasiccont-class}}), an object of class \code{'LMlatentcont'} for the model with covariates on the latent model (see \code{\link{LMlatentcont-class}}), or an object of class \code{'LMmanifestcont'} for the model with covariates on the measurement model (see \code{\link{LMmanifestcont-class}})).
}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni
}

\references{Bartolucci F., Pandolfi S., Pennoni F. (2017) LMest: An R Package for Latent Markov Models for Longitudinal Categorical
Data, \emph{Journal of Statistical Software}, \bold{81}(4), 1-38.

Bartolucci, F., Farcomeni, A. and Pennoni, F. (2013) \emph{Latent Markov Models for Longitudinal Data}, Chapman and Hall/CRC press.}

\examples{
\dontrun{

data(data_long_cont)

# Basic LM model

out <- lmestCont(responsesFormula = Y1 + Y2 + Y3 ~ NULL,
                 index = c("id", "time"),
                 data = data_long_cont,
                 k = 3,
                 modBasic = 1,
                 tol = 10^-5)

out
summary(out)

# Basic LM model with model selection using BIC

out1 <- lmestCont(responsesFormula = Y1 + Y2 + Y3 ~ NULL,
                  index = c("id", "time"),
                  data = data_long_cont,
                  k = 1:5,
                  ntry = 2,
                  modBasic = 1,
                  tol = 10^-5)

out1
out1$Bic

# Basic LM model with model selection using AIC

out2 <- lmestCont(responsesFormula = Y1 + Y2 + Y3 ~ NULL,
                  index = c("id", "time"),
                  data = data_long_cont,
                  k = 1:5,
                  modBasic = 1,
                  ntry = 2,
                  modSel = "AIC",
                  tol = 10^-5)
out2
out2$Aic


# LM model with covariates in the measurement model

out3 <- lmestCont(responsesFormula = Y1 + Y2 + Y3 ~ X1 + X2,
                  index = c("id", "time"),
                  data = data_long_cont,
                  k = 3,
                  output = TRUE)

out3 
summary(out3)

# LM model with covariates in the latent model

out4 <- lmestCont(responsesFormula = Y1 + Y2 + Y3 ~ NULL,
                  latentFormula = ~ X1 + X2,
                  index = c("id", "time"),
                  data = data_long_cont,
                  k = 3,
                  output = TRUE)

out4
summary(out4)

# LM model with two covariates affecting the initial probabilities and one 
# affecting the transition probabilities 

out5 <- lmestCont(responsesFormula = Y1 + Y2 + Y3 ~ NULL,
                  latentFormula = ~ X1 + X2 | X1,
                  index = c("id", "time"),
                  data = data_long_cont,
                  k = 3,
                  output = TRUE)

out5
summary(out5)

}
}
