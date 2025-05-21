\name{trade.union}
\alias{trade.union}
\non_function{}
\title{Trade union data}
\usage{data(trade.union)}
\description{
The \code{trade.union} data frame has data on 534 U.S. workers.
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{years.educ}{number of years of education.}
    \item{south}{indicator of living in southern region of U.S.A.}
    \item{female}{gender indicator: 0=male,1=female.}
    \item{years.experience}{number of years of work experience}
    \item{union.member}{indicator of trade union membership: 
                       0=non-member, 1=member.}
    \item{wage}{wages in dollars per hour.}
    \item{age}{age in years.}
    \item{race}{1=black, 2=Hispanic, 3=white.}
    \item{occupation}{1=management, 2=sales, 3=clerical, 4=service, 5=professional, 6=other.}
    \item{sector}{0=other, 1=manufacturing, 2=construction.}
    \item{married}{indicator of being married: 0=unmarried, 1=married.}
  }
}
\source{Berndt, E.R. (1991)
\emph{The Practice of Econometrics.} New York: Addison-Wesley. 
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(trade.union)
pairs(trade.union,pch=".")
}
\keyword{datasets}
