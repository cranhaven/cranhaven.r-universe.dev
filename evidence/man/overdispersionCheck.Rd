\name{overdispersionCheck}
\alias{overdispersionCheck}
\title{A robust comparison of the location and the scale of the input
  vector.
}
\description{
  A large sample of Normal-distributed data with more than 10\% of the
 observations further than 1.5 times the IQR from the median shows signs
 of overdispersion, as recommended in Gelman et al., 2014.
}
\usage{
overdispersionCheck(x)
}
\arguments{
  \item{x}{
    an input vector of reals without missing values
  }
}
\value{The function prints the approximate percentage of observations
  that are further from the median than would be expected in a normal
  distribution.
}
\references{Gelman, A., Carlin, J.B., Stern, H.S., Dunson, D.B.,
  Vehtari, A., and Rubin, D.B. 2014. Bayesian Data Analysis. Third
  Ed.. CRC Press
}
\author{
Robert van Hulst
}
\examples{overdispersionCheck(rt(100, 1))
}

  