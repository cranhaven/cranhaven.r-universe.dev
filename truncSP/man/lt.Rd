\name{lt}
\alias{lt}
\alias{print,lt-method}
\alias{summary,lt-method}
\alias{print,summary.lt-method}
\alias{coef,lt-method}
\alias{vcov,lt-method}
\alias{residuals,lt-method}
\alias{fitted,lt-method}

\title{
Estimation of truncated regression models using the Left Truncated (LT) estimator
}
\description{
Estimates linear regression models with truncated response variables (fixed truncation point), using the LT estimator (Karlsson 2006).
}
\usage{
lt(formula, data, point = 0, direction = "left", clower = "ml", const = 1, cupper = 2,
   beta = "ml", covar = FALSE, na.action, ...)
\S4method{print}{lt}(x, digits = max(3, getOption("digits") - 3), ...)
\S4method{summary}{lt}(object, level=0.95, ...)
\S4method{print}{summary.lt}(x, digits= max(3, getOption("digits") - 3), ...)
\S4method{coef}{lt}(object,...)
\S4method{vcov}{lt}(object,...)
\S4method{residuals}{lt}(object,...)
\S4method{fitted}{lt}(object,...)
}

\arguments{
  \item{x, object}{
     an object of class \code{"lt"}
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
  \item{clower}{
the lower threshold value to be used when trimming the conditional density of the errors from below. The default is \code{"ml"} meaning that the residual standard deviation from fitting a maximum likelihood model for truncated regression, using \code{\link[truncreg]{truncreg}}, is used. Method \code{"ols"} uses the estimated residual standard deviation from a linear model fitted by \code{\link{lm}}. It is also possible to manually supply the threshold value by setting \code{clower} to be equal to a number or numeric vector of length one.
}
  \item{const}{
a number that can be used to alter the size of the lower threshold. \code{const=0.5} would give a lower threshold value that is half the original size. The default value is 1.
}
  \item{cupper}{
number indicating what upper threshold to use when trimming the conditional density of the errors from above. The number is used to multiply the lower threshold value, i.e. if \code{cupper=2} (the default value) the upper threshold value is two times larger than the lower threshold value.
}
  \item{beta}{the method of determining the starting values of the regression coefficients (See Details for more information):
\itemize{
     \item The default method is \code{"ml"}, meaning that the estimated regression coefficients from fitting a maximum likelihood model for truncated regression, assuming Gaussian errors, are used. The maximum likelihood model is fitted using \code{\link[truncreg]{truncreg}}. 
\item Method \code{"ols"} means that the estimated regression coefficients from fitting a linear model with \code{\link{lm}}. 
\item The third option is to manually provide starting values as either a vector, column matrix or row matrix. 
}
}
  \item{covar}{
logical. Indicates whether or not the covariance matrix should be estimated. If \code{TRUE} the covariance matrix is estimated using bootstrap. The default number of replicates is 2000 but this can be adjusted (see argument \code{\dots}). However, since the bootstrap procedure is time-consuming the default is \code{covar=FALSE}.
}
  \item{na.action}{
a function which indicates what should happen when the data contain \code{NA}s.
}
\item{digits}{
    the number of digits to be printed     
}
 \item{level}{
     the desired level of confidence, for confidence intervals provided by \code{summary.lt}. A number between 0 and 1. The default value is \code{0.95}.
}
  \item{\dots}{
additional arguments. For \code{lt} the number of bootstrap replicates can be adjusted by setting \code{R=}the desired number of replicates. Also the \code{control} argument of \code{\link{optim}} can be set by \code{control=list()} (see Details for more information).
}
}
\details{
Minimizes the objective function described in Karlsson (2006) wrt the vector of regression coefficients, in order to find the LT estimates. The minimization is performed by \code{\link{optim}} using the "Nelder--Mead" method, and a maximum number of iterations of 2000. The maximum number of iterations can be adjusted by setting \code{control=list(maxit=\dots)} (for more information see the documentation for \code{\link{optim}}). \cr\cr It is recommended to use one of the methods for generating the starting values of the regression coefficients (see argument \code{beta}) rather than supplying these manually, unless one is confident that one has a good idea of what these should be. This because the starting values can have a great impact on the result of the minimization. \cr\cr Note that setting \code{cupper=1} means that the LT estimates will coincide with the estimates from the Quadratic Mode Estimator (see function \code{\link{qme}}). For more detailed information see Karlsson and Lindmark (2014).
}
\value{
\code{lt} returns an object of class \code{"lt"}. \cr\cr The function \code{summary} prints a summary of the results, including two types of confidence intervals (normal approximation and percentile method). The generic accessor functions
\code{coef}, \code{fitted}, \code{residuals} and \code{vcov} extract various useful features of the value returned by \code{lt}\cr\cr An object of class \code{"lt"}, a list with elements:
  \item{coefficients }{the named vector of coefficients}
  \item{startcoef }{the starting values of the regression coefficients used by \code{\link{optim}}}
  \item{cvalues }{information about the thresholds used. The method and constant used and the resulting lower and upper threshold values.}
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
Karlsson, M. (2006) Estimators of regression parameters for truncated and censored data, \emph{Metrika}, \bold{63}, pp 329--341\cr\cr
Karlsson, M., Lindmark, A. (2014) truncSP: An R Package for Estimation of Semi-Parametric Truncated Linear Regression Models, \emph{Journal of Statistical Software}, \bold{57(14)}, pp 1--19, \url{http://www.jstatsoft.org/v57/i14/} \cr\cr
}
\author{
Anita Lindmark and Maria Karlsson
}

\seealso{
\code{\link{lt.fit}}, the function that does the actual fitting \cr\cr
\code{\link{qme}}, for estimation of models with truncated response variables using the QME estimator \cr\cr
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

##Use lt to consistently estimate the slope parameters
lt(y~x1+x2+x3, dtrunc, point=0, direction="left", clower="ml", const=1, 
   cupper=2, beta="ml", covar=FALSE)
   
##Example using data "PM10trunc"
data(PM10trunc)

ltpm10 <- lt(PM10~cars+temp+wind.speed+temp.diff+wind.dir+hour+day, 
   data=PM10trunc, point=2, control=list(maxit=2500))

summary(ltpm10)

}

\keyword{ regression }
