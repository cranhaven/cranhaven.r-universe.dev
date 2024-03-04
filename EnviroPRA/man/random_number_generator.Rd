\name{random_number_generator}
\alias{random_number_generator}

\title{
Random number generator}
\description{
Return a vector of n random numbers following a truncated distribution (dist) in agreement with a fitted parameters "Fited"
}
\usage{
random_number_generator(n, Fited, dist, a, b)
}

\arguments{
  \item{n}{
The number of desired generated numbers}
  \item{Fited}{
A list contaning the parameters obtained by application of Fit_dist_parameter
}
  \item{dist}{
Character vector indicating the distribution to be applied:"norm",  "lnorm",  "geom", "exp",  "pois", "cauchy",  "logis",  "weibull"}
  \item{a}{
Truncation Lower limit}
  \item{b}{
Truncation Upper limit}
}

\value{
A vector of n random numbers
}

\author{
F. Barrio-Parra}



\seealso{
Fit_dist_parameter
}
\examples{

set.seed(123)
a <- rnorm(n = 100, mean = 10, sd = 1)
Fit <- Fit_dist_parameter(a)

b <-random_number_generator(n = 10000, Fited = Fit, 
                            dist = "norm", a =8, b = 12)

par(mfrow=c(2,1))

hist(a,xlim= c(7,14))
hist(b,xlim= c(7,14))
}

\keyword{ methods }