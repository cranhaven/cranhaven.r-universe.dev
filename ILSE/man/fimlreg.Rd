\name{fimlreg}
\alias{fimlreg}
\alias{fimlreg.numeric}
\alias{fimlreg.formula}
\title{Full Information Maximum Likelihood Linear Regression}
\description{
  Estimate regression coefficients based on Full Information Maximum Likelihood Estimation, which can couple missing data, including response missing or covariates missing.
}
\usage{
  fimlreg(...)

  \method{fimlreg}{formula}(formula, data=NULL, ...)
  \method{fimlreg}{numeric}(Y, X, ...)
}
\arguments{
  \item{formula}{an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under 'Details'.}
  \item{Y}{a numeric vector, the reponse variable.}
  \item{X}{a numeric matrix that may include NAs, the covariate matrix.}
  \item{data}{an optional data frame, list or environment (or object coercible by as.data.frame to a data frame) containing the variables in the model. If not found in data, the variables are taken from environment(formula), typically the environment from which clse is called.}
  \item{...}{Optional arguments.}
}
\details{
  Note that arguments ... of stats::nlm are the parameters of algorithm, see the details in help file of "nlm". "fimlreg" can cople with any type of missing data.
}
\value{
  Return a list including following components:
  \item{beta}{A named vector of coefficients}
  \item{formula}{The formula used}
  \item{data}{The raw data}
}
\author{
  Liu Wei
}
\seealso{
  \link{ilse}
}
\examples{
data(nhanes)
## example one: include missing value
fiml1 <- fimlreg(age~., data=nhanes)
print(fiml1)
# example two: No missing vlaue
## example two: No missing value
n <- 100
group <- rnorm(n, sd=4)
weight <- 3.2*group + 1.5 + rnorm(n, sd=0.1)
fimllm <- fimlreg(weight~group, data=data.frame(weight=weight, group=group))
print(fimllm)
}
\keyword{Linear Model}
\concept{Full Information Maximum Likelihood}
