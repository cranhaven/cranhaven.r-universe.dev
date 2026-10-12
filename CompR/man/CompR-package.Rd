\name{CompR-package}
\alias{CompR-package}
\alias{CompR}
\docType{package}
\title{
Paired Comparison Data Analysis
}
\description{
Different tools for describing and analysing paired comparison data are presented. Main methods are estimation of products scores according Bradley Terry Luce model. A segmentation of the individual could be conducted on the basis of a mixture distribution approach. The number of classes can be tested by the use of Monte Carlo simulations. This package deals also with multi-criteria paired comparison data.
}

\details{
\tabular{ll}{
Package: \tab CompR\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-07-01\cr
License: \tab GPL-2\cr
Depends: \tab methods, MASS, stats, graphics, utils\cr

}
Function to estimate products configurations (Bradley's scores) and weights of the \cr classes is \code{EstimBradley()}.


Function to perform a test concerning the number of classes is \code{ResSimulLvrRatio()}.


Function to obtain a graphical representation of Bradley's scores is \code{Piplot()}.

}
\author{
Michel Semenou

Maintainer: <michel.semenou@oniris-nantes.fr>
}
\keyword{ package }
\seealso{
  \code{\link{EstimBradley}}, \code{\link{ResSimulLvrRatio}}, \code{\link{Piplot}}
}

\examples{
data(Cocktail)
show(Cocktail)
ResCock1<-EstimBradley(Cocktail,Constraint=0,Tcla=1,eps=0.001,eps1=0.001,TestPi=TRUE)
show(ResCock1)
Res_LvrRatio1<-ResSimulLvrRatio(Cocktail,ResCock1,0,3,level=0.05,eps=0.001,eps1=0.001)
getSimu(Res_LvrRatio1)
getTest(Res_LvrRatio1)
}
