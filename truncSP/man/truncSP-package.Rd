\name{truncSP-package}
\alias{truncSP-package}
\alias{truncSP}
\docType{package}
\title{
Estimators of semi-parametric truncated regression models
}
\description{
Functions for estimation of semi-parametric linear regression models with truncated response variables (fixed truncation point). Estimation using the Symmetrically Trimmed Least Squares (STLS) estimator (Powell 1986), Quadratic Mode (QME) estimator (Lee 1993) and Left Truncated (LT) estimator (Karlsson 2006).
}
\details{
\tabular{ll}{
Package: \tab truncSP\cr
Type: \tab Package\cr
Version: \tab 1.2.2\cr
Date: \tab 2014-05-05\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
Depends: \tab R(>= 2.10), methods, truncreg, boot\cr
}
These semi-parametric estimators provide an alternative to maximum likelihood estimators, which are sensitive to distributional misspecification (Davidson and MacKinnon, 1993, p 536). All three estimators use trimming of the conditional density of the error terms. STLS assumes symmetrically distributed error terms, while QME and LT have been shown to be consistent for estimation of the slope parameters under asymmetrically distributed errors as well (Laitila 2001 and Karlsson 2006). The functions in the package (\code{\link{qme}}, \code{\link{lt}} and \code{\link{stls}}), all use \code{\link{optim}} to maximize or minimize objective functions wrt the vector of regression coefficients in order to find estimates (Karlsson and Lindmark, 2014). As the covariance matrices of the estimators depend on the density of the error distribution, the estimation of these is complicated and bootstrap (as described in Karlsson 2004 and Karlsson and Lindmark 2014) is used in all three functions. 
}
\author{
Anita Lindmark and Maria Karlsson, Department of Statistics, Umea University\cr\cr
Maintainer: Anita Lindmark <anita.lindmark@stat.umu.se>
}
\references{
Davidson, R., MacKinnon, J. G. (1993) \emph{Estimation and Inference in Econometrics}, Oxford University Press, USA\cr\cr
Karlsson, M. (2004) Finite sample properties of the QME, \emph{Communications in Statistics - Simulation and Computation}, \bold{5}, pp 567--583\cr\cr
Karlsson, M. (2006) Estimators of regression parameters for truncated and censored data, \emph{Metrika}, \bold{63}, pp 329--341\cr\cr
Karlsson, M., Lindmark, A. (2014) truncSP: An R Package for Estimation of Semi-Parametric Truncated Linear Regression Models, \emph{Journal of Statistical Software}, \bold{57(14)}, pp 1--19, \url{http://www.jstatsoft.org/v57/i14/} \cr\cr
Laitila, T. (2001) Properties of the QME under asymmetrically distributed disturbances, \emph{Statistics & Probability Letters}, \bold{52}, pp 347--352\cr\cr
Lee, M. (1993) Quadratic mode regression, \emph{Journal of Econometrics}, \bold{57}, pp 1-19\cr\cr
Lee, M., Kim, H. (1998) Semiparametric econometric estimators for a truncated regression model: a review with an extension, \emph{Statistica Neerlandica}, \bold{52(2)}, pp 200--225\cr\cr
Powell, J. (1986) Symmetrically Trimmed Least Squares Estimation for Tobit Models, \emph{Econometrika}, \bold{54(6)}, pp 1435--1460
}
\keyword{ package }
\seealso{
\code{\link[truncreg]{truncreg}}, function for estimating models with truncated response variables by maximum likelihood assuming Gaussian errors 
}
\examples{
 ##Simulate a data.frame (model with asymmetrically distributed errors)
n <- 10000
x1 <- runif(n,0,10)
x2 <- runif(n,0,10)
x3 <- runif(n,-5,5)
eps <- rexp(n,0.2)- 5
y <- 2-2*x1+x2+2*x3+eps
d <- data.frame(y=y,x1=x1,x2=x2,x3=x3)
   
##Use a truncated subsample
dtrunc <- subset(d, y>0)

##Use qme or lt to consistently estimate the slope parameters
qme(y~x1+x2+x3, dtrunc, point=0, direction="left", cval="ols", const=1, 
   beta="ols", covar=FALSE)
lt(y~x1+x2+x3, dtrunc, point=0, direction="left", clower="ols", const=1, 
   cupper=2, beta="ols", covar=FALSE)
   
##Simulate a data.frame (symmetrically distributed errors)
n <- 10000
x1 <- runif(n,0,10)
x2 <- runif(n,0,10)
x3 <- runif(n,-5,5)
y <- 1-2*x1+x2+2*x3+rnorm(n,0,2)
d <- data.frame(y=y,x1=x1,x2=x2,x3=x3)

##Use a truncated subsample
dtrunc <- subset(d, y>0)
  
##Use stls to estimate the model
stls(y~x1+x2+x3, dtrunc, point=0, direction="left", beta="ols", covar=FALSE)
}
