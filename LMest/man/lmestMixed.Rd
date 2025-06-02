\name{lmestMixed}
\alias{lmestMixed}
\title{Estimate mixed Latent Markov models}
\description{Main function for estimating the mixed latent Markov (LM) models  for categorical responses with discrete random effects in the latent model.}

\usage{
lmestMixed(responsesFormula = NULL,
           data, index, k1, k2, start = 0,
           weights = NULL, tol = 10^-8, maxit = 1000,
           out_se = FALSE, seed = NULL)
}
\arguments{
  \item{responsesFormula}{a symbolic description of the model to fit. A detailed description is given in the ‘Details’ section}
  \item{data}{a \code{data.frame} in long format}
    \item{index}{a character vector with two elements, the first indicating the name of the unit identifier, and the second the time occasions}
  \item{k1}{number of latent classes}
  \item{k2}{number of latent states}
  \item{start}{type of starting values (0 = deterministic, 1 = random, 2 = initial values in input)}
    \item{weights}{an optional vector of weights for the available responses}
  \item{tol}{tolerance level for convergence}
  \item{maxit}{maximum number of iterations of the algorithm}
  \item{out_se}{to compute the information matrix and standard errors (FALSE is the default option)}
  \item{seed}{an integer value with the random number generator state}
}
\details{
The function \code{lmestMixed} estimates the mixed LM for categorical data. The function requires data in long format and two additional columns indicating the unit identifier and the time occasions.

\code{responsesFormula} is used to specify the responses of the mixed LM model:
\itemize{
\item{\code{responsesFormula = y1 + y2 ~ NULL}} \cr the mixed LM model with two categorical responses (\code{y1} and \code{y2}) is specified;
\item{\code{responsesFormula = NULL}} \cr all the columns in the data except the \code{"id"} and \code{"time"} columns are used as responses to estimate the mixed LM.

}

Missing responses are not allowed.
}



\value{
Returns an object of class \code{'LMmixed'}  (see \code{\link{LMmixed-class}}).
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

# Example based on criminal data

data(data_criminal_sim)
data_criminal_sim <- data.frame(data_criminal_sim)

# Estimate mixed LM model for females

responsesFormula <- lmestFormula(data = data_criminal_sim,
                                 response = "y")$responsesFormula

# fit mixed LM model only for females
out <- lmestMixed(responsesFormula = responsesFormula,
                  index = c("id","time"),
                  k1 = 2,
                  k2 = 2,
                  data = data_criminal_sim[data_criminal_sim$sex == 2,])
out
summary(out)
}

}
