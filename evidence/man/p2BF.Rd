\name{p2BF}
\alias{p2BF}
\title{
Conversion of a frequentist p-value to the lower bound of the Bayes factor
against the null hypothesis assuming equal odds of the null and the alternative
}
\description{
This function computes the approximate lower bound to the Bayes factor
of the null hypothesis against the alternative, assuming equal odds of the
null and the alternatlve.
}

\note{ the p-value should be less than 1/e (= 0.37). }

\usage{
p2BF(p)
}

\arguments{
  \item{p}{
the frequentist p-value (which has to be less than 1/e or 0.37)
}
}
\value{
the approximate lower bound of the Bayes factor of the null hypothesis
against the alternative
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
  \code{\link{p2minpp}}
}
\examples{
p2BF(p = 0.05)
}

\keyword{ htest }

