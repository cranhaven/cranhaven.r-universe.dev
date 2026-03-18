\name{PPV}
\alias{PPV}
\title{ calculates the positive predictive value (PPV) of a diagnostic test.
}
\description{
The positive predictive value (PPV) of a dianostic test is the probability
that someone with a positive diagnostic test for a condition does have
the condition. The PPV can easily be calculated from the
prevalence, the sensitivity, and the specificity, but this function
automates the procedure.
}
\usage{
PPV(sens, spec, prev)
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
the positive predictive value of the test
}
\references{
van Hulst, R. 2018. Evaluating Scientific Evidence. ms.
}
\author{
Robert van Hulst
}
\seealso{
\code{\link{NPV}}
}
\examples{

PPV(0.9, 0.8, 0.003)
}

\keyword{ misc }

