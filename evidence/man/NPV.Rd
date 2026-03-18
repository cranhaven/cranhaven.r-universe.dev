\name{NPV}
\alias{NPV}
\title{
computes the Negative Predictive Value.
}
\description{
The negative predictive value (NPV) of a diagnostic test is the probability
that someone with a negative diagnostic test for a condition does not
have the condition. The NPV can easily be calculated from the
prevalence, the sensitivity, and the specificity, but this function
automates the procedure.
}
\usage{
NPV(sens, spec, prev)
}
\arguments{
  \item{sens}{
the sensitivity of the test
}
  \item{spec}{
the specificity of the test
}
  \item{prev}{
the prevalence of the disease
}
}
\value{
the negative predictive value
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\seealso{
\code{\link{PPV}}
}
\examples{
NPV(0.9, 0.8, 0.003)
}

\keyword{ misc }
