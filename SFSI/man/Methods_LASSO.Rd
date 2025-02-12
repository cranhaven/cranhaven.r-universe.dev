
\name{Regression coefficients and predicted values in LASSO-type problems}
\alias{coef.LASSO}
\alias{predict.LASSO}
\title{LASSO methods}
\usage{
\method{coef}{LASSO}(object, ...)

\method{predict}{LASSO}(object, ...)
}
\arguments{
\item{object}{An object of the class 'LASSO' returned either by the function 'LARS' or 'solveEN'}

\item{...}{Other arguments: 
 \itemize{
  \item \code{X} (numeric matrix) scores for as many predictors there are in \code{ncol(object$beta)}  (in columns) for a desired number \code{n}  of observations (in rows)
  \item \code{iy} (integer vector) Optional index of columns of the matrix 'Gamma' to be returned in \code{coef} function
    \item \code{ilambda} (integer) Optional to return regression coefficients associated to a specific penalty position
  \item \code{nsup} (numeric) Optional to return regression coefficients associated to a given penalty that yield approximately 'nsup' non-zero coefficients
 }
}
}
\value{
Method \code{coef} returns a matrix that contains the regression coefficients (in rows) associated to each value of lambda (in columns). When the regression was applied to an object \code{Gamma} with more than one column, method \code{coef} returns a list

Method \code{predict} returns a matrix with predicted values
\ifelse{html}{\out{X&beta;}}{\eqn{\textbf{X}\boldsymbol{\beta}}{X beta}} (in rows)
for each value of lambda (in columns).
}
\description{
Retrieving regression coefficients and predicted values from the 'solveEN' and 'LARS' functions' outputs
}
\examples{
  # See examples in
  # help(solveEN, package="SFSI")
  # help(LARS, package="SFSI")
}
