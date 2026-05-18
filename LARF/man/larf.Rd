\name{larf}
\alias{larf}

\title{Local Average Response Functions for Instrumental Variable Estimation of Treatment Effects}
\description{
The function provides instrumental variable estimation of treatment effects when both the endogenous treatment and its instrument are binary. Applicable to both binary and continuous outcomes.}

\usage{
larf(formula, treatment, instrument, data, method = "LS",  
     AME = FALSE, optimizer = "Nelder-Mead", zProb = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{specification of the outcome model in the form like either \code{y ~ x1 + x2} or \code{y ~ X} where \code{X} is a matrix containing all the covariates excluding the treatment. Also support multi-part formulas (Zeileis and Croissant, 2010). For example, \code{y + d ~ x1 + x2 | z}, where \code{d} represents the treatment and \code{z} the instrument.}
  \item{treatment}{A vector containing the binary treatment.}
  \item{instrument}{A vector containing the binary instrument for the endogenous treatment.}
  \item{data}{an optional data frame. If unspecified, the data will be taken from the working environment.}
  \item{method}{the estimation method to be used. The default is ``LS", standing for least squares. ``ML", standing for maximum likelihood, is an alternative.}
  \item{AME}{whether average marginal effects (AME) should be reported. The default is FALSE, in which case marginal effects at the means (MEM) are reported.}
    \item{optimizer}{the optimization algorithm for the ML method. It should be one of ``Nelder-Mead", ``BFGS", ``CG", ``L-BFGS-B", ``SANN", or ``Brent". See \code{optim} in R for more detail.}
  \item{zProb}{a vector containing the probability of receiving the treatment inducement (i.e., instrument = 1) that have been estimated by semiparametrical methods.}
}
\details{
\code{larf} is the high-level interface to the work-horse function \code{\link{larf.fit}}. A set of standard methods (including \code{print}, \code{summary}, \code{coef}, \code{vcov}, \code{fitted}, \code{resid}, \code{predict}) can be used to extract the corresponding information from a \code{\link{larf}} object.

The function provides instrumental variable estimation of treatment effects when both the endogenous treatment and its instrument (i.e., the treatment inducement) are binary. The method (Abadie, 2003) involves two steps. First, pseudo-weights are constructed from the probability of receiving the treatment inducement. By default the function estimates the probability by a Probit regression. But it also allows users to employ the probability that has been estimated by semiparametric methods. Second, the pseudo-weights are used to estimate the local average response function of the outcome conditional on the treatment and covariates. The function provides both least squares and maximum likelihood estimates of the conditional treatment effects.
}

\value{
\item{coefficients}{Estimated coefficients.}
\item{SE}{Standard errors of the estimated coefficients.}
\item{MargEff}{Estimated marginal effects, available only for binary outcomes.}
\item{MargStdErr}{Standard errors of the estimated marginal effects, available only for binary outcomes.}
\item{vcov}{Variance covariance matrix of the estimated coefficients.}
\item{fitted.values}{Predicted outcomes based on the estimated model. They are probabilities when the outcome is binary.}
}

\references{
Abadie, Alberto. 2003. "Semiparametric Instrumental Variable Estimation of Treatment Response Models." \emph{Journal of Econometrics} 113: 231-263. \cr
An, Weihua and Xuefu Wang. 2016. "LARF: Instrumental Variable Estimation of Causal Effects through Local Average Response Functions." \emph{Journal of Statistical Software} 71(1): 1-13.  \cr
Zeileis, Achim and Yves Croissant. 2010. "Extended Model Formulas in R: Multiple Parts and Multiple Responses." \emph{Journal of Statistical Software} 34(1): 1-13. \url{ http://www.jstatsoft.org/v34/i01/}.
}
\author{
Weihua An, Departments of Sociology and Statistics, Indiana University Bloomington, \email{weihuaan@indiana.edu}. \cr
Xuefu Wang, Department of Statistics, Indiana University Bloomington, \email{wangxuef@umail.iu.edu}.
}

\note{
We derived part of the code from the Matlab code written by Professor Alberto Abadie, available at \url{http://www.hks.harvard.edu/fs/aabadie/larf.html}. We thank Onur Altindag and Behzad Kianian for helpful suggestions on improving the computation.
}

\seealso{
\code{\link{larf.fit}}, \code{\link{c401k}}
}

\examples{
data(c401k)
attach(c401k)

\dontrun{
# Continuous outcome. Treatment effects of participation in 401(k) 
# on net family financial assest
est1 <- larf(nettfa ~ inc + age + agesq + marr + fsize, treatment = p401k, 
instrument = e401k, data = c401k)
summary(est1)

# Nonparametric estimates of the probability of 
# receiving the treatment inducement
library(mgcv)
firstStep <- gam(e401k ~ s(inc) + s(age) + s(agesq) + marr + s(fsize), 
data=c401k, family=binomial(link = "probit"))
zProb <- firstStep$fitted
est2<- larf(nettfa ~ inc + age + agesq + marr + fsize, treatment = p401k, 
instrument = e401k, data = c401k, zProb = zProb)
summary(est2) 

# Binary outcome. Treatment effects of participation in 401(k) 
# on participation in IRA
est3 <- larf(pira ~ inc + age + agesq + marr + fsize, treatment = p401k, 
instrument = e401k, data = c401k)
summary(est3) 
}
}
