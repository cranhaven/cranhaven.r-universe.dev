\name{print.sltime}
\alias{print.sltime}

\title{
S3 Method for Printing an 'sltime' Object
}
\description{
Print the contribution of learners included in the super learner.
}

\usage{
\method{print}{sltime}(x,  digits=7, ...)
}

\arguments{
  \item{x}{An object returned by the function \code{survivalSL}.}
  \item{digits}{An optional integer for the number of digits to print when printing numeric values.}
  \item{...}{For future methods.}
}

\value{No return value for this S3 method.}

\examples{
data(dataDIVAT2)

sl1 <- survivalSL(method=c("LIB_COXridge", "LIB_AFTggamma"),  metric="ci",
  data=dataDIVAT2[1:150,],  times="times", failures="failures", pro.time = 12,
  cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"), cv=3)

print(sl1, digits=4)
}

\keyword{Printing}
