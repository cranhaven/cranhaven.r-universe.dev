\name{VUROCS-package}
\alias{VUROCS-package}
\alias{VUROCS}
\docType{package}
\title{ \packageTitle{VUROCS} }
\description{ \packageDescription{VUROCS} }
\details{
  The package VUROCS provides three core functions to determine the volume under the ROC surface (VUS) as well as the variance and covariance of the VUS. The implementation is generally based on the algorithms presented in Waegeman, De Baets and Boullart (2008).
  
  \itemize{
    \item \code{\link[VUROCS]{VUS}(y,fx)} calculates the VUS for a vector of realizations \code{y} and a vector of predictions \code{fx}.
    \item \code{\link[VUROCS]{VUSvar}(y,fx)} calculates the variance of VUS  for a vector of realizations \code{y} and a vector of predictions \code{fx}.
    \item \code{\link[VUROCS]{VUScov}(y,fx1,fx2)} calculates the covariance of the two VUS implied by the predictions \code{fx1} and \code{fx2} for a vector of realizations \code{y}.
  }
  
  
  In addition to these three core functions, the package also provides an implementation of the cumulative LGD accuracy ratio (CLAR) suggested by Ozdemir and Miu (2009) specially for the purpose of assessing the discriminatory power of Loss Given Default (LGD) credit risk models. The CLAR as well as an adjusted version are computed by the functions \code{\link[VUROCS]{clar}} and \code{\link[VUROCS]{clarAdj}}. Moreover, the package provides time-efficient implementations of Somers' D , Kruskall's Gamma,  Kendall's Tau_b and  Kendall's Tau_c in the functions \code{\link[VUROCS]{SomersD}}, \code{\link[VUROCS]{Kruskal_Gamma}}, \code{\link[VUROCS]{Kendall_taub}} and \code{\link[VUROCS]{Kendall_tauc}}. These functions also compute asymptotic standard errors defined by Brown and Benedetti (1977) and Goktas and Oznur (2011).
}
\author{
Kazianka Hannes, Morgenbesser Anna, Nowak Thomas
}
\references{
Brown, M.B., Benedetti, J.K., 1977. Sampling Behavior of Tests for Correlation in Two-Way Contingency Tables. Journal of the American Statistical Association 72(358), 309-315

Goktas, A., Oznur, I., 2011. A Comparison of the Most Commonly Used Measures of Association for Doubly Ordered Square Contingency Tables via Simulation. Metodoloski zvezki 8 (1), 17-37

Ozdemir, B., Miu, P., 2009. Basel II Implementation: A Guide to Developing and Validating a Compliant, Internal Risk Rating System. McGraw-Hill, USA.

Waegeman W., De Baets B., Boullart L., 2008. On the scalability of ordered multi-class ROC analysis. Computational Statistics & Data Analysis 52, 3371-3388.
}

\keyword{ package }

\examples{
y  <- rep(1:5,each=3)
fx <- c(3,3,3,rep(2:5,each=3))

VUS(y,fx)
clar(y,fx)
clarAdj(y,fx)
SomersD(y,fx)
Kruskal_Gamma(y,fx)
Kendall_taub(y,fx)
Kendall_tauc(y,fx)

VUSvar(rep(1:5,each=3),c(1,2,3,rep(2:5,each=3)))
VUScov(c(1,2,1,3,2,3),c(1,2,3,4,5,6),c(1,3,2,4,6,5))
}
