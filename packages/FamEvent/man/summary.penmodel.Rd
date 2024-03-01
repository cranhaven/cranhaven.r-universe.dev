\name{summary.penmodel}
\alias{summary.penmodel}

\title{
Summary method for class \code{penmodel}
}
\description{
Provides a summary of a fitted penetrance model.
}
\usage{
\method{summary}{penmodel}(object, correlation=FALSE, \ldots)
}

\arguments{
  \item{object}{
An object class of \code{'penmodel'}, a fitted model by \code{\link{penmodel}} or \code{\link{penmodelEM}} functions.
}
 \item{correlation}{Logical; if \code{TRUE}, returns the correlation matrix of the estimated parameters. Default is \code{FALSE}.}
 \item{...}{Further arguments passed to or from other methods.}
}

\value{
Returns the object of class \code{'summary.penmodel'}, including the following summary values:
\item{estimates}{List of parameter estimates of transformed baseline parameters and regression coefficients, their standard errors, their robust standard errors if \code{robust=TRUE} was selected when fitting the penetrance model, \eqn{t}-statistics and corresponding two-sided \eqn{p}-values.}
\item{varcov}{Variance-covariance matrix of the parameter estimates.}
\item{varcov.robust}{Robust variance-covariance matrix of the parameter estimates if \code{robust = TRUE} was selected when fitting the penetrance model.}
\item{correlation}{Correlation matrix obtained from the variance-covariance matrix.}
\item{correlation.robust}{Correlation matrix obtained from the robust variance-covariance matrix if \code{robust = TRUE} was selected when fitting the penetrance model.}
}

\examples{
# Simulated family data 

set.seed(4321)
fam <- simfam(N.fam = 200, design = "pop+", variation = "none", base.dist = "Weibull", 
       base.parms = c(0.01, 3), vbeta = c(-1.13, 2.35), agemin = 20, allelefreq = 0.02)
 
# Penetrance model fit for the simulated family data

fit <- penmodel(Surv(time, status) ~ gender + mgene, cluster = "famID", 
       parms=c(0.01, 3, -1.13, 2.35), data = fam, design = "pop+", base.dist = "Weibull")

# Summary of the model parameter and penetrance estimates from model fit

summary(fit)

\dontrun{
Estimates: 
            Estimate Std. Error t value Pr(>|t|)  
log(lambda)   -4.531    0.08583 -52.793  0.01206 *
log(rho)       1.113    0.04688  23.737  0.02680 *
gender        -1.302    0.19233  -6.768  0.09339 .
mgene          2.349    0.23825   9.859  0.06436 .
Signif. codes:   0 '***'  0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
}

}
\author{
Yun-Hee Choi	
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{penmodel}, \link{penmodelEM}, \link{print.penmodel}, \link{print.summary.penmodel} \link{plot.penmodel}}

% Function \code{\link{coef}} will extract the matrix of coefficients with standard errors, \eqn{t}-statistics and \eqn{p}-values.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ summary }
