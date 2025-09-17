\name{summary}
\alias{summary}
\alias{summary.ilse}
\alias{summary.fiml}
\alias{Fitted.values}
\alias{Residuals}
\title{Summarizing the inference information for ILSE or FIML methods}
\description{
  summary method for class "ilse" or "fiml".
}
\usage{
  summary(object, Nbt=20)

  \method{summary}{ilse}(object, Nbt=20)

  \method{summary}{fiml}(object, Nbt=20)

  ##
  Fitted.values(object)
  ##
  Residuals(object)

}
\arguments{
  \item{object}{an object of class "ilse".}
  \item{Nbt}{an positive integer, the repeated times of bootstrap to eatimate covariance matrix of regression coefficient.}
}

\value{
  The function summary.ilse computes and returns a named matrix of summary statistics of the fitted linear model given in object by ILSE or FIML methods.
  The function Fitted.values return a vector, fitted repsonse vlaues.
  The function Residuals return a vector, residuals.
}
\seealso{
summary.lm
fitted.vlaues
residuals
}
\examples{
# example one
data(nhanes)
NAlm <- ilse(age~., data=nhanes)
summary(NAlm, Nbt=5)

fimllm <- fimlreg(age~., data=nhanes)
summary(fimllm, Nbt = 5)
}
