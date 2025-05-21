\name{term.structure}
\alias{term.structure}
\non_function{}
\title{Term structure data}
\usage{data(term.structure)}
\description{
The \code{term.structure} data frame has 117 
observations on the prices of U.S. STRIPS 
(Separate Trading on Registered Interest and 
Principal of Securities) on December 31, 1995.}
\format{
This data frame contains the following columns:
\describe{
\item{time.to.maturity}{time in years between 31st December, 1995, 
      and the date on which the STRIPS matures.}
\item{price}{price of the STRIPS as a percent of par.}
}
}
\source{
University of Houston Fixed Income Database.
}
\references{
Jarrow, R., Ruppert, D., and Yu, Y. (2004).
Estimating the term structure of corporate debt 
with a semiparametric penalized spline model, 
\emph{Journal of the American Statistical Association},
\bold{99}, 57-66. 

Ruppert, D., Wand, M.P. and Carroll, R.J. (2003)\cr
\emph{Semiparametric Regression} Cambridge University Press.\cr
\url{http//stat.tamu.edu/~carroll/semiregbook/}
}
\examples{
library(SemiPar)
data(term.structure)
attach(term.structure)
plot(time.to.maturity,price)
}
\keyword{datasets} 
