\name{optiscale-package}
\alias{optiscale-package}
\alias{optiscale}
\docType{package}
\title{Optimal Scaling of a Data Vector}

\description{
This package provides tools to perform an optimal scaling analysis
   on a data vector.
   The main result of the optimal scaling is a vector of scores which
   are a least-squares approximation to a vector of quantitative
   values, subject to measurement constraints based upon a vector
   of qualitative data values.
   See Young (1981) for details.
}

\details{
\tabular{ll}{
Package: \tab optiscale\cr
Type: \tab Package\cr
Version: \tab 1.2.2\cr
Date: \tab 2021-02-02\cr
License: \tab GPL-2 \cr
LazyLoad: \tab yes\cr
}
   The function that performs the optimal scaling is \code{opscale()}.
   It produces an object of class "opscale".
   Generic methods are defined for \code{print}, \code{summary}, and
   \code{plot} (graphing optimally-scaled values versus
   original data values).
}
\author{
   William G. Jacoby

Maintainer: 
   William G. Jacoby <\email{wm.g.jacoby@gmail.com}>
}
\references{
   Young, Forrest W. (1981) \dQuote{Quantitative Analysis of
      Qualitative Data.} \emph{Psychometrika} 46: 357-388.
}

\seealso{
   \code{\link{opscale},\link{plot.opscale}, \link{print.opscale},
   \link{summary.opscale}}
}

\examples{
  ###   x1 is vector of qualitative data
  ###   x2 is vector of quantitative values
            x1 <- c(1,1,1,1,2,2,2,3,3,3,3,3,3)     
            x2 <- c(3,2,2,2,1,2,3,4,5,2,6,6,4)     
  ###   Optimal scaling, specifying that x1
  ###   is ordinal-discrete
     op.scaled <- opscale(x.qual=x1, x.quant=x2,   
                  level=2, process=1)              
     print(op.scaled)
     summary(op.scaled)
}
