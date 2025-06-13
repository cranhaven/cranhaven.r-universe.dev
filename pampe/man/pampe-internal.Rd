\name{pampe-internal}
\alias{pampe-internal}
\alias{regsubsets2aic}
\docType{package}
\title{
Implementation of the Panel Data Approach for Program Evaluation}
\description{
Description of internal function for the pampe package, regsubsets2aic. Takes the models estimated using regsubsets and orders them according to a model selection criteria like AIC, AICc, or BIC.}
\usage{
regsubsets2aic(x,y,z)
}
\arguments{
\item{x}{ A matrix of predictors }
\item{y}{ A response vector }
\item{z}{ Output of regsubsets }
}
\seealso{
\code{\link{pampe}},\code{\link{leaps}} 
}
