\name{mlm}
\alias{mlm}
\title{Multiple Linear Models}
\description{
    Fits multiple linear models.
}
\usage{
    mlm(formula, data, vars, save.residuals=FALSE)
}
\arguments{
    \item{formula}{an object of class \code{formula} (or one that can be coerced
                   to that class): a symbolic description of the models to be
                   fitted.}
    \item{data}{an optional data frame, list or environment (or object coercible
                by \code{as.data.frame} to a data frame) containing the
                variables in the model.}
    \item{vars}{a character vector of variables of interest.}
    \item{save.residuals}{whether model residuals should be stored and
                          returned.}
}
\value{
    A list with elements:
    \item{coefficients}{if `vars` is missing or non-empty, an array (or data
                        frame, if \code{vars} contains a single element) of
                        regression coefficient estimates (\code{coef}), standard
                        errors (\code{coef.se}), and corresponding
                        \eqn{p}-values (\code{pval}).}
    \item{residuals}{if \code{save.residuals} is \code{TRUE}, a matrix of model
                     residuals.}
}
\examples{
    coefs <- matrix(runif(6*10), 6, 10)
    X <- matrix(rnorm(100*5), 100, 5)
    Y <- cbind(1, X) %*% coefs + rnorm(100*10)

    models <- mlm(Y ~ X)
    str(models)
}
\seealso{
    \code{\link{lm}}
}
