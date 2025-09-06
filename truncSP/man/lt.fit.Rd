\name{lt.fit}
\alias{lt.fit}
\title{
Function for fitting LT
}
\description{
Function to find LT estimates of the regression coefficients for regression models with truncated response variables. Uses \code{\link{optim}}. Intended to be called through \code{\link{lt}}, not on its own, since \code{lt} also transforms data into the correct form etc.
}
\usage{
lt.fit(formula, mf, point, direction, bet, cl, cu, ...)
}
\arguments{
  \item{formula}{
     a symbolic description of the model to be estimated
}
  \item{mf}{
     the \code{model.frame} containing the variables to be used when fitting the model. \code{\link{lt}} transforms the model frame to the correct form before calling \code{lt.fit}. If \code{lt.fit} is called on its own the model frame needs to be transformed manually.
}
  \item{point}{
     point of truncation
}
  \item{direction}{
     direction of truncation
}
  \item{bet}{
     starting values to be used by \code{\link{optim}}. Column matrix with p rows.
}
  \item{cl}{
     lower threshold value to be used, number or numeric vector of length 1.  (See \code{\link{lt}}, argument \code{clower}, for more information).
}
  \item{cu}{
     upper threshold value to be used, number or numeric vector of length 1. (See \code{\link{lt}}, argument \code{cupper}, for more information).
}
  \item{\dots}{
     additional arguments to be passed to \code{\link{optim}} (see the documentation for \code{\link{lt}} for further details).
}
}
\value{
a list with components:
  \item{startcoef }{the starting values of the regression coefficients used by \code{\link{optim}}}
  \item{coefficients }{the named vector of coefficients}
  \item{counts }{number of iterations used by \code{\link{optim}}. See the documentation for \code{\link{optim}} for further details}
  \item{convergence }{from \code{\link{optim}}. An integer code. 0 indicates successful completion. Possible error codes are \cr 1  indicating that the iteration limit maxit had been reached.\cr 10  indicating degeneracy of the Nelder--Mead simplex.}
  \item{message }{from \code{\link{optim}}. A character string giving any additional information returned by the optimizer, or \code{NULL}.}
  \item{residuals }{the residuals of the model}
  \item{df.residual }{the residual degrees of freedom}
  \item{fitted.values }{the fitted values}
}
\author{
Anita Lindmark  and Maria Karlsson
}
\seealso{
\code{\link{lt}}
}
\examples{
require(utils)
##Model frame
n <- 10000
x <- rnorm(n,0,2)
y <- 2+x+4*rnorm(n)
d <- data.frame(y=y, x=x)
dl0 <- subset(d, y>0)
mf <- model.frame(y~x, data=dl0)

##Starting values and threshold values
lmmod <- lm(data=mf)
bet <- lmmod$coef
bet <- matrix(bet)
cl <- sqrt(deviance(lmmod)/df.residual(lmmod))
cu <- 2*cl

str(lt. <- lt.fit(y~x,mf,point=0,direction="left",bet,cl,cu))
}

\keyword{ regression }
