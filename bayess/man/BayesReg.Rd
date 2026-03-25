\name{BayesReg}
\alias{BayesReg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bayesian linear regression output%%  ~~function to do ... ~~
}
\description{
This function contains the R code for the implementation of Zellner's \eqn{G}{G}-prior analysis of
the regression model as described in Chapter 3. The purpose of \code{BayesRef}
is dual: first, this R function shows how easily automated
this approach can be. Second, it also illustrates how it is possible to get exactly the same 
type of output as the standard R function \code{summary(lm(y~X))}. In particular,
it calculates the Bayes factors for variable selection, more precisely single variable exclusion.
}
\usage{
BayesReg(y, X, g = length(y), betatilde = rep(0, dim(X)[2]), prt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
response variable%%     ~~Describe \code{y} here~~
}
  \item{X}{
matrix of regressors%%     ~~Describe \code{X} here~~
}
  \item{g}{
constant g for the \eqn{G}{G}-prior%%     ~~Describe \code{g} here~~
}
  \item{betatilde}{
prior mean on \eqn{\beta}{beta}%%     ~~Describe \code{betatilde} here~~
}
  \item{prt}{
boolean variable for printing out the standard output%%     ~~Describe \code{prt} here~~
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{postmeancoeff }{posterior mean of the regression coefficients}
  \item{postsqrtcoeff }{posterior standard deviation of the regression coefficients}
  \item{log10bf }{log-Bayes factors against the full model}
  \item{postmeansigma2}{posterior mean of the variance of the model}
  \item{postvarsigma2}{posterior variance of the variance of the model}
%% ...
}
\examples{
data(faithful)
BayesReg(faithful[,1],faithful[,2])
}
