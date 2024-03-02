\name{print.libsl}
\alias{print.libsl}

\title{
S3 Method for Printing an 'libsl' Object
}
\description{
Print the model or algorithm.
}

\usage{
\method{print}{libsl}(x, ...)
}

\arguments{
  \item{x}{An object returned by the function \code{flexsurv}.}
  \item{...}{For future methods.}
}

\value{No return value for this S3 method.}

\seealso{
  \code{\link{LIB_AFTgamma}}, \code{\link{LIB_AFTggamma}}, \code{\link{LIB_AFTllogis}}, \code{\link{LIB_AFTweibull}},
  \code{\link{LIB_PHexponential}}, \code{\link{LIB_PHgompertz}}.
}

\examples{
data(dataDIVAT2)

model <- LIB_AFTgamma(times="times", failures="failures",  data=dataDIVAT2[1:100,],
        cov.quanti=c("age"),  cov.quali=c("hla", "retransplant", "ecd"))

print(model)
}

\keyword{Printing}
