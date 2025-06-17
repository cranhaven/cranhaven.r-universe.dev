\name{jTestVaR}
\alias{jTestVaR}
\title{
VaR Back-testing
}
\description{
Provide some kinds of test for Value at risk.

The null hypothesis is the equation of the probability of loss cross over VaR and the given ruin level.

It will show how the calculated VaR can be accepted.

See the report: Value at Risk.<researchgate.net>
}
\usage{
jTestVaR(Ret, VaR, p, test_significant, type)
}
\arguments{
  \item{Ret}{
Return series use to back-test.
}

  \item{VaR}{
Value at Risk that has been calculated.
}

  \item{p}{
Given probability used to calculate VaR
}

  \item{test_significant}{
Significant level of the test.
}

  \item{type}{
Kinds of test.

	. p_value 

	. pof 

	. tuff

	. mixkup
}
}
\details{
See the report.
}
\value{
Statistic,Quantile and test result.
}
\author{
Hung Vu
}
\note{
viet-hung.vu@jvn.edu.vn
}


\seealso{
https://www.researchgate.net/profile/Vu_Hung4
}
\examples{
y <- c(11, 12, 10, 13, 12, 14, 13, 15, 13, 14, 12)
s <- jReturn(y)
alpha <- 0.2
h <- 0
v <- jVaR('non_adjust_hist',s,alpha,h)
jTestVaR(s, v, alpha, 0.05, 'p_value')
}

\keyword{ jVaR }
\keyword{ jPrice }
\keyword{ jReturn }