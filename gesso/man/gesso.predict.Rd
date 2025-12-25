\name{gesso.predict}
\alias{gesso.predict}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict new outcome vector
}
\description{
Predict new outcome vector based on the new data and estimated model coefficients.
}
\usage{
gesso.predict(beta_0, beta_e, beta_g, beta_gxe, new_G, new_E, 
                   beta_c=NULL, new_C=NULL, family = "gaussian")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta_0 }{estimated intercept value}
  \item{beta_e}{estimated environmental coefficient value}
  \item{beta_g}{a vector of estimated main effect coefficients}
  \item{beta_gxe}{a vector of estimated interaction coefficients}
  \item{new_G}{matrix of main effects, variables organized by columns}
  \item{new_E}{vector of environmental measurments}
  \item{beta_c}{a vector of estimated confounders coefficients}
  \item{new_C}{matrix of confounders, variables organized by columns}
  \item{family}{set \code{family="gaussian"} for the continuous outcome and 
          \code{family="binomial"} for the binary outcome with 0/1 levels}
}
\value{
Returns a vector of predicted values
}
\examples{
data = data.gen()
tune_model = gesso.cv(data$G_train, data$E_train, data$Y_train)
coefficients = gesso.coef(tune_model$fit, tune_model$lambda_min)
beta_0 = coefficients$beta_0; beta_e = coefficients$beta_e                   
beta_g = coefficients$beta_g; beta_gxe = coefficients$beta_gxe     

new_G = data$G_test; new_E = data$E_test
new_Y = gesso.predict(beta_0, beta_e, beta_g, beta_gxe, new_G, new_E)
cor(new_Y, data$Y_test)^2
}

