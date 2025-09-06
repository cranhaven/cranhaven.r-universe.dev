\name{qme}
\alias{qme}
\alias{print,qme-method}
\alias{summary,qme-method}
\alias{print,summary.qme-method}
\alias{coef,qme-method}
\alias{vcov,qme-method}
\alias{residuals,qme-method}
\alias{fitted,qme-method}

\title{
  Estimation of truncated regression models using the Quadratic Mode Estimator (QME)
}
\description{
  Estimation of linear regression models with truncated response variables (fixed truncation point), using the Quadratic Mode Estimator (QME) (Lee 1993 and Laitila 2001)
}
\usage{
qme(formula, data, point = 0, direction = "left", cval = "ml", 
  const = 1, beta = "ml", covar = FALSE, na.action, ...)
\S4method{print}{qme}(x, digits = max(3, getOption("digits") - 3), ...)
\S4method{summary}{qme}(object, level=0.95, ...)
\S4method{print}{summary.qme}(x, digits = max(3, getOption("digits") - 3), ...)
\S4method{coef}{qme}(object,...)
\S4method{vcov}{qme}(object,...)
\S4method{residuals}{qme}(object,...)
\S4method{fitted}{qme}(object,...)
}
\arguments{
\item{x, object}{
     an object of class \code{"qme"}
}
 \item{formula}{
     a symbolic description of the model to be estimated
}
  \item{data}{
     an optional data frame
}
  \item{point}{
     the value of truncation (the default is 0)
}
  \item{direction}{
     the direction of truncation, either \code{"left"} (the default) or \code{"right"}
}
  \item{cval}{
     the threshold value to be used when trimming the conditional density of the errors. The default is \code{"ml"} meaning that the estimated residual standard deviation from a maximum likelihood model for truncated regression, fitted using \code{\link[truncreg]{truncreg}}, is used.  Method \code{"ols"} uses the residual standard deviation from fitting a linear model using \code{\link{lm}}. It is also possible to manually supply the threshold by setting \code{cval} to be equal to a number or numeric vector of length one.
}
  \item{const}{
     a number that can be used to alter the size of the threshold value. \code{const=0.5} would give a threshold value that is half the original size. The default value is 1.
}
  \item{beta}{the method of determining the starting values of the regression coefficients (See Details for more information):
\itemize{
     \item The default method is \code{"ml"}, meaning that the estimated regression coefficients from fitting a maximum likelihood model for truncated regression, assuming Gaussian errors, are used. The maximum likelihood model is fitted using \code{\link[truncreg]{truncreg}}. 
\item Method \code{"ols"} means that the estimated regression coefficients from fitting a linear model with \code{\link{lm}} are used. 
\item The third option is to manually provide starting values as either a vector, column matrix or row matrix. 
}
}
  \item{covar}{
    logical. Indicates whether or not the covariance matrix should be estimated. If \code{TRUE} the covariance matrix is estimated using bootstrap, as described in Karlsson (2004). The default number of replicates is 2000 but this can be adjusted (see argument \code{\dots}). However, since the bootstrap procedure is time-consuming the default is \code{covar=FALSE}.
}
  \item{na.action}{
     a function which indicates what should happen when the data contain \code{NA}s. 
}
 \item{digits}{
    the number of digits to be printed     
}
 \item{level}{
     the desired level of confidence, for confidence intervals provided by \code{summary.qme}. A number between 0 and 1. The default value is \code{0.95}.
}
 \item{\dots}{
     additional arguments. For \code{qme} the number of bootstrap replicates can be adjusted by setting \code{R=}the desired number of replicates. Also the \code{control} argument of \code{\link{optim}} can be set by \code{control=list()} (for more information on this see Details).
}
}
\details{
  Finds the QME estimates of the regression coefficients by maximizing the objective function described in Lee (1993) wrt the vector of regression coefficients. The maximization is performed by \code{\link{optim}} using the "Nelder--Mead" method. The maximum number of iterations is set at 2000, but this can be adjusted by setting \code{control=list(maxit=\dots)} (for more information see the documentation for \code{\link{optim}}). \cr\cr The starting values of the regression coefficients can have a great impact on the result of the maximization. For this reason it is recommended to use one of the methods for generating these rather than supplying the values manually, unless one is confident that one has a good idea of what the starting values should be. For more detailed information see Karlsson and Lindmark (2014).
}
\value{
  \code{qme} returns an object of class \code{"qme"}. \cr\cr The function \code{summary} prints a summary of the results, including two types of confidence intervals (normal approximation and percentile method). The generic accessor functions
\code{coef}, \code{fitted}, \code{residuals} and \code{vcov} extract various useful features of the value returned by \code{qme}\cr\cr An object of class \code{"qme"}, a list with elements:
  \item{coefficients }{the named vector of coefficients}
  \item{startcoef }{the starting values of the regression coefficients used by \code{\link{optim}}}
  \item{cval }{information about the threshold value used. The method and constant value used and the resulting threshold value.}
  \item{value }{the value of the objective function corresponding to \code{coefficients}}
  \item{counts }{number of iterations used by \code{\link{optim}}. See the documentation for \code{\link{optim}} for further details}
  \item{convergence }{from \code{\link{optim}}. An integer code. 0 indicates successful completion. Possible error codes are \cr 1  indicating that the iteration limit maxit had been reached.\cr 10  indicating degeneracy of the Nelder--Mead simplex.}	
  \item{message }{from \code{\link{optim}}. A character string giving any additional information returned by the optimizer, or \code{NULL}.}
  \item{residuals }{the residuals of the model}
  \item{fitted.values }{the fitted values}
  \item{df.residual }{the residual degrees of freedom}
  \item{call }{the matched call}
  \item{covariance }{if \code{covar}\code{=}\code{TRUE}, the estimated covariance matrix}
  \item{R }{if \code{covar}\code{=}\code{TRUE}, the number of bootstrap replicates}
  \item{bootrepl }{if \code{covar}\code{=}\code{TRUE}, the bootstrap replicates}
}
\references{
Karlsson, M. (2004) Finite sample properties of the QME, \emph{Communications in Statistics - Simulation and Computation}, \bold{5}, pp 567--583\cr\cr
Karlsson, M., Lindmark, A. (2014) truncSP: An R Package for Estimation of Semi-Parametric Truncated Linear Regression Models, \emph{Journal of Statistical Software}, \bold{57(14)}, pp 1--19, \url{http://www.jstatsoft.org/v57/i14/} \cr\cr
Laitila, T. (2001) Properties of the QME under asymmetrically distributed disturbances, \emph{Statistics & Probability Letters}, \bold{52}, pp 347--352\cr\cr
Lee, M. (1993) Quadratic mode regression, \emph{Journal of Econometrics}, \bold{57}, pp 1-19\cr\cr
Lee, M. & Kim, H. (1998) Semiparametric econometric estimators for a truncated regression model: a review with an extension, \emph{Statistica Neerlandica}, \bold{52(2)}, pp 200--225

}
\author{
  Anita Lindmark and Maria Karlsson
}


\seealso{
\code{\link{qme.fit}}, the function that does the actual fitting \cr\cr
\code{\link{lt}}, for estimation of models with truncated response variables using the LT estimator \cr\cr
\code{\link{stls}}, for estimation of models with truncated response variables using the STLS estimator \cr\cr
\code{\link[truncreg]{truncreg}} for estimating models with truncated response variables by maximum likelihood, assuming Gaussian errors
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

##Use qme to consistently estimate the slope parameters
qme(y~x1+x2+x3, dtrunc, point=0, direction="left", cval="ml", const=1, 
   beta="ml", covar=FALSE)
   
##Example using data "PM10trunc"
data(PM10trunc)

qmepm10 <- qme(PM10~cars+temp+wind.speed+temp.diff+wind.dir+hour+day, 
   data=PM10trunc, point=2, control=list(maxit=4500))

summary(qmepm10)

}

\keyword{ regression }