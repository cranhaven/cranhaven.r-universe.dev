\name{retire.plan}
\alias{retire.plan}
\non_function{}
\title{Retirement plan data}
\usage{data(retire.plan)}
\description{
The \code{retire.plan} data frame has data on "401(k)" retirement
plans for employees of 
92 firms managed by a company code-named Best Retirement Inc. (BRI).
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{contrib}{contribution to retirement plan at end of first year}
    \item{group}{1=client has group life of group health insurance policy,
                 0=otherwise.}
    \item{turnover}{employee turnover rate.}
    \item{eligible}{number of employees eligible to 
                    participate in 401(k) plans.}
    \item{vest}{1=plan has immediate vesting of employer contributions,
                0=otherwise.}
    \item{failsafe}{1=plan has a fail-safe provision, 0=otherwise.}
    \item{match}{percentage of contributions matched by the employer.}
    \item{salary}{average annual employee salary in dollars}.
    \item{estimate}{underwriter's estimate of
                    end-of-year contributions in dollars.}
    \item{susan}{1=plan was sold by a sales representative 
               who has been specifically trained to deal 
               exclusively with 401(k) plans (code-named
                  Susan Shepard).}
  }
}
\source{
Bryant, P.G. and Smith, M.A. (1995).
\emph{Practical data analysis: case studies in business statistics.}
Chicago: Irwin.
}
\references{
    Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
     \emph{Semiparametric Regression} Cambridge University Press.\cr
     \url{http://stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(retire.plan)
pairs(retire.plan)
}
\keyword{datasets}
