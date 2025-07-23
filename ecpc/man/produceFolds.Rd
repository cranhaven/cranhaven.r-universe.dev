\name{produceFolds}
\alias{produceFolds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Produce folds
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Produce folds for cross-validation.
}
\usage{
produceFolds(nsam, outerfold, response, model = c("logistic","cox","other"), 
balance = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nsam}{
%%     ~~Describe \code{nsam} here~~
Number of samples
}
  \item{outerfold}{
%%     ~~Describe \code{outerfold} here~~
Number of folds.
}
  \item{response}{
%%     ~~Describe \code{response} here~~
Response data.
}
  \item{model}{
%%     ~~Describe \code{model} here~~
Type of model for the response.
}
  \item{balance}{
%%     ~~Describe \code{balance} here~~
Should folds be balanced in response?
}
}
\value{
A list with `outerfold' elements containing a vector of sample indices in each fold.
}
\examples{
n<-100
outerfold <- 10

#linear model
resp <- rnorm(n)
folds <- produceFolds(nsam=n, outerfold=outerfold, response=resp)

#logistic model: keep 0/1 balanced across folds
resp <- as.factor(rnorm(n)>0.5)
folds <- produceFolds(nsam=n, outerfold=outerfold, response=resp, balance = TRUE)

}
