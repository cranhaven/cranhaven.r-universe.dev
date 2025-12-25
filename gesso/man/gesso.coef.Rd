\name{gesso.coef}
\alias{gesso.coef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get model coefficients
}
\description{
A function to obtain coefficients from the model fit object corresponding to the desired pair of tuning parameters \code{lambda} = (\code{lambda_1}, \code{lambda_2}).
}
\usage{
gesso.coef(fit, lambda)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fit}{model fit object obtained either by using function \code{gesso.fit} or  \code{gesso.cv}}
  \item{lambda}{a pair of tuning parameters organized in a tibble (ex: \code{lambda = tibble(lambda_1=grid[1], lambda_2=grid[1])})}
}
\value{
A list of model coefficients corresponding to \code{lambda} values of tuning parameters
  \item{beta_0 }{estimated intercept value}
  \item{beta_e}{estimated environmental coefficient value}
  \item{beta_g}{a vector of estimated main effect coefficients}
  \item{beta_c}{a vector of estimated confounders coefficients}
  \item{beta_gxe}{a vector of estimated interaction coefficients}
}
\examples{
data = data.gen()
model = gesso.cv(data$G_train, data$E_train, data$Y_train, grid_size=20, 
        parallel=TRUE, nfolds=3)
gxe_coefficients = gesso.coef(model$fit, model$lambda_min)$beta_gxe                
g_coefficients = gesso.coef(model$fit, model$lambda_min)$beta_g     
}
