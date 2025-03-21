\name{writeModel}
\alias{writeModel}
\title{Creating an OpenBUGS model file}
\description{Convert R function to an OpenBUGS model file}
\usage{
writeModel(model, con = "model.txt", digits = 5)
}
\arguments{
  \item{model}{R function containing the BUGS model in the BUGS model language, for minor differences see Section Details.}
  \item{con}{passed to \code{link{writeLines}} which actually writes the model file}
  \item{digits}{number of significant digits used for \pkg{BUGS}
    input, see \code{\link{formatC}}}
}
\value{
 Nothing, but as a side effect, the model file is written.
}
\details{
The fact that bugs models follow closely to S (R) syntax is used.
It should be possible to write most BUGS models as R functions.

As a difference, BUGS syntax allows truncation specification like this:
\code{dnorm(...) I(...)}
but this is illegal in R. To overcome this incompatibility, use \code{\%_\%} before \code{I(...)}:
\code{dnorm(...) \%_\% I(...)}.
The dummy operator \code{\%_\%} will be removed before the BUGS code is saved.
}
\author{original idea by Jouni Kerman, modified by Uwe Ligges}
\examples{
## Same "ratsmodel" that is used in the examples in ?BRugs and ?BRugsFit:
ratsmodel <- function(){
    for(i in 1:N){
        for(j in 1:T){
            Y[i, j] ~ dnorm(mu[i, j],tau.c)
            mu[i, j] <- alpha[i] + beta[i] * (x[j] - xbar)
        }
        alpha[i] ~ dnorm(alpha.c, alpha.tau)
        beta[i] ~ dnorm(beta.c, beta.tau)
    }
    tau.c ~ dgamma(0.001, 0.001)
    sigma <- 1 / sqrt(tau.c)
    alpha.c ~ dnorm(0.0, 1.0E-6)        
    alpha.tau ~ dgamma(0.001, 0.001)
    beta.c ~ dnorm(0.0, 1.0E-6)
    beta.tau ~ dgamma(0.001, 0.001)
    alpha0 <- alpha.c - xbar * beta.c       
}

## some temporary filename:
filename <- file.path(tempdir(), "ratsmodel.txt")
## write model file:
writeModel(ratsmodel, filename)
## and let's take a look:
file.show(filename)
}
\seealso{\code{\link{modelCheck}}, \code{\link{BRugs}}}
\keyword{IO}
\concept{model}

