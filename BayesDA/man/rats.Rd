\name{rats}
\alias{rats}
\docType{data}
\title{ Tumor Incidence in Historical Control Groups and Current Group of Rats    }
\description{
   In the evaluation of drugs for possible clinical application, studies
   are routinely performed on rodents.  For a particular study drawn from the
   statistical literature, suppose the immediate aim is to estimate theta, 
   the probability of tumor in a population of female laboratory rats of type 
   'F344' that receive  a zero dose of the drug --- a control group. This 
   gives data from historical control groups, and one current experimental control group.
}
\usage{data(rats)}
\format{
  A data frame with 71 observations on the following 3 variables.
  \describe{
    \item{\code{y}}{number of rats with tumors}
    \item{\code{N}}{number of rats in experiment}
    \item{\code{Current}}{a factor with levels \code{0} \code{1}}
  }
}
\examples{
data(rats)
summary(rats)
# moment estimate of (alfa, beta) in beta distribution is (1.4, 8.6)
with(subset(rats, Current=="0"), hist( y/N, freq=FALSE))
plot(function(x) dbeta(x, 1.4, 8.6), from=0, to=1, col="red", add=TRUE)
# plotting posterior in same plot:
plot(function(x) dbeta(x, 5.4, 18.6), from=0, to=1, col="blue", add=TRUE)
}
\keyword{datasets}
