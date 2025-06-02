\name{lmestMc}
\alias{lmestMc}
\title{Estimate Markov Chain models}
\description{Main function for estimating  Markov Chain (MC) models for categorical responses with or without covariates.}
\usage{
lmestMc(responsesFormula = NULL,
        data, index, start = 0,
        modBasic = 0, weights = NULL,
        tol = 10^-8, maxit = 1000,
        out_se = FALSE, output = FALSE, fort = TRUE, seed = NULL)
}
\arguments{
 \item{responsesFormula}{a symbolic description of the model to fit. A detailed description is given in the ‘Details’ section}
  \item{data}{a \code{data.frame} in long format}
    \item{index}{a character vector with two elements, the first indicating the name of the unit identifier, and the second the time occasions}
   \item{start}{type of starting values (0 = deterministic, 1 = random, 2 = initial values in input)}
  \item{modBasic}{model on the transition probabilities (0 for time-heter., 1 for time-homog., from 2 to (TT-1) partial homog. of that order)}
   \item{weights}{an optional vector of weights for the available responses}
  \item{tol}{tolerance level for convergence}
  \item{maxit}{maximum number of iterations of the algorithm}
  \item{out_se}{to compute the information matrix and standard errors (FALSE is the default option)}
  \item{output}{to return additional output (\code{PI},\code{Piv}) (\code{\link{MCcov-class}})}
  \item{fort}{to use fortran routines when possible (By default is set to TRUE)}
  \item{seed}{An integer value with the random number generator state.}
}

\details{
The function \code{lmestMc} estimates the basic MC model and the MC model with covariates for categorical responses. The function requires data in long format and two additional column indicating the unit identifier and the time occasions.

\code{responsesFormula} is used to specify the basic MC models and the model with covariates:
\itemize{
\item{\code{responsesFormula = y1 + y2 ~ NULL}} \cr the MC model without covariates and two responses (\code{y1} and \code{y2}) is specified;
\item{\code{responsesFormula = NULL}} \cr all the columns in the data except the \code{"id"} and \code{"time"} columns are used to estimate MC without covariates;
\item{\code{responsesFormula = y1 ~ x1 + x2 | x3 + x4}} \cr the MC model with one response (\code{y1}),  two covariates affecting the initial probabilities (\code{x1} and \code{x2}) and other two different covariates affecting the transition probabilities (\code{x3} and \code{x4}) is specified;
%\item{\code{responsesFormula = x1 ~ 1 | x4 + x5}} specifies the MC model where the covariates affects only the transition probabilities and an intercept is considered for the intial probabilities
\item{\code{responsesFormula = y1 ~ x1 + x2}} \cr the MC model with one response (\code{y1}) and two covariates (\code{x1} and \code{x2}) affecting both the initial and transition probabilities is specified.

}

Missing responses are not allowed.

}
\value{
Returns an object of class \code{'MCbasic'} for the basic model without covariates (see \code{\link{MCbasic-class}}), or an object of class \code{'MCcov'} for the model with covariates (see \code{\link{MCcov-class}}).
}

\author{Francesco Bartolucci,
  Silvia Pandolfi,
  Fulvia Pennoni,
  Alessio Farcomeni,
  Alessio Serafini
}

\references{
Bartolucci F., Pandolfi S., Pennoni F. (2017) LMest: An R Package for Latent Markov Models for Longitudinal Categorical
Data, \emph{Journal of Statistical Software}, \bold{81}(4), 1-38.

Bartolucci, F., Farcomeni, A. and Pennoni, F. (2013) \emph{Latent Markov Models for Longitudinal Data}, Chapman and Hall/CRC press.}

\examples{
\dontrun{
# Basic Markov Chain  model

data("RLMSlong")

# Categories  rescaled from 1 “absolutely unsatisfied” to  5 “absolutely satisfied”

RLMSlong$value <- 5 - RLMSlong$value

out <- lmestMc(responsesFormula = value ~ NULL,
               index = c("id","time"),
               modBasic = 1,
               data = RLMSlong)

out
summary(out)



# Example of drug consumption data

data("data_drug")
long <- data_drug[,-6]
long <- data.frame(id = 1:nrow(long),long)
long <- reshape(long,direction = "long",
                idvar = "id",
                varying = list(2:ncol(long)))

out1 <- lmestMc(index = c("id","time"), data = long,
                weights = data_drug[,6], modBasic = 1, out_se = TRUE)

out1

### MC model with covariates
### Covariates: gender, race, educational level (2 columns), age and age^2

data("data_SRHS_long")
SRHS <- data_SRHS_long[1:2400,]

# Categories of the responses rescaled from 1 “poor” to 5 “excellent”

SRHS$srhs <- 5 - SRHS$srhs


out2 <- lmestMc(responsesFormula = srhs ~
                I( 0 + (race==2) + (race == 3)) +
                I(0 + (education == 4)) +
                I(0 + (education == 5)) +
                I(age - 50) +
                I((age-50)^2/100),
                index = c("id","t"),
                data = SRHS)
out2
summary(out2)

# Criminal data

data(data_criminal_sim)
data_criminal_sim = data.frame(data_criminal_sim)

out3 <- lmestMc(responsesFormula = y5~sex,
                index = c("id","time"),
                data = data_criminal_sim,
                output = TRUE)

out3

}
}
