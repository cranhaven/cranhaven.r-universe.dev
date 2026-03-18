\name{fabric}
\alias{fabric}
\docType{data}
\title{ Numbers of Faults Found in Each of 32 Rolls of Fabric   }
\description{
  Numbers of faults found in each of 32 rolls of fabric produced 
  in a particular factory.  Also given is the length of the roll. 
}
\usage{data(fabric)}
\format{
  A data frame with 32 observations on the following 2 variables.
  \describe{
    \item{\code{length}}{length of roll}
    \item{\code{faults}}{number of faults in roll}
  }
}
\details{
    The book uses this for exercise 5. page 441
}
\examples{
data(fabric)
str(fabric)
names(fabric)
# Identity link:
with(fabric, plot(faults ~ length))
# log link:
with(fabric, plot(faults ~ length, log="y"))
# Fitting poisson regression models:
mod1 <- glm(faults ~ length-1, data=fabric, family=poisson)
OK <- require(MCMCpack)
if(OK) mod2 <- MCMCpoisson(faults ~ length-1, data=fabric, b0=0, B0=0.0001)
summary(mod1)
confint(mod1)
if(OK) summary(mod2)
# The exercise is to investigate overdispersion ...
}
\keyword{datasets}
\concept{poisson regression}
