\name{p2minpp}
\alias{p2minpp}
\title{
Conversion of a frequentist p-value to a lower bound of the posterior
probability that the null hypothesis is true assuming equal odds of the
null and the alternative
}
\description{
This function computes the approximate lower bound to the posterior
probability of the null hypothesis assuming equal odds of the
null and the alternative. See Sellke et al.(2001) for the derivation,
and note that the posterior probability of the null hypothesis is what
many incorrectly assume the p-value is measuring.
}
\note{ the p-value should be less than 1/e (0.37). }
\usage{
p2minpp(p)
}

\arguments{
  \item{p}{
the frequentist p-value (which has to be less than 1/e or 0.37)
}
}
\value{
the approximate lower bound of the posterior probability of the null hypothesis
}
\references{
Sellke, T., Bayarri, M.J., and Berger, J.O. 2001. Calibration of p
  Values for Testing Precise Hypotheses. Am. Statistician 55(1) pp
  62--71.
  
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\seealso{
  \code{\link{p2BF}}
}
\examples{
p2minpp(p=0.05)
}

\keyword{ htest }

