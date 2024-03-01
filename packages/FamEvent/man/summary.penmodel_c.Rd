\name{summary.penmodel_c}
\alias{summary.penmodel_c}

\title{
Summary method for class \code{penmodel_c}
}
\description{
Provides a summary of a fitted competing risks penetrance model.
}
\usage{
\method{summary}{penmodel_c}(object, correlation=FALSE, \ldots)
}

\arguments{
  \item{object}{
An object class of \code{'penmodel_c'}, a fitted model by \code{\link{penmodel_c}} function.
}
 \item{correlation}{Logical; if \code{TRUE}, returns the correlation matrix of the estimated parameters. Default is \code{FALSE}.}
 \item{...}{Further arguments passed to or from other methods.}
}

\value{
Returns the object of class \code{'summary.penmodel_c'}, including the following summary values:
\item{estimates}{List of parameter estimates of transformed baseline parameters and regression coefficients, their standard errors, their robust standard errors if \code{robust=TRUE} was selected when fitting the penetrance model, \eqn{t}-statistics and corresponding two-sided \eqn{p}-values.}
\item{varcov}{Variance-covariance matrix of the parameter estimates.}
\item{varcov.robust}{Robust variance-covariance matrix of the parameter estimates if \code{robust = TRUE} was selected when fitting the penetrance model.}
\item{correlation}{Correlation matrix obtained from the variance-covariance matrix.}
\item{correlation.robust}{Correlation matrix obtained from the robust variance-covariance matrix if \code{robust = TRUE} was selected when fitting the penetrance model.}
}

\examples{
# Simulated family completing risks data 

\dontrun{
set.seed(4321)
fam1 <- simfam_c(N.fam = 300, design = "pop+", variation = "frailty", competing=TRUE,
              base.dist = "Weibull", frailty.dist = "gamma", depend=c(0.5, 1), 
              allelefreq = 0.02, base.parms = list(c(0.01, 3), c(0.01, 3)), 
              vbeta = list(c(-1.13, 2.35),c(-1, 2)))

# Penetrance model fit for the simulated family data

fit <- penmodel_c(
       formula1 = Surv(time, status==1) ~ gender + mgene, 
       formula2 = Surv(time, status==2) ~ gender + mgene, 
       cluster = "famID", 
       parms = list(c(0.01, 3, -1.13, 2.35), c(0.01, 3, -1, 2)),
       data = fam1, design = "pop+", base.dist = "Weibull")

# Summary of the model parameter and penetrance estimates from model fit

summary(fit)
}

}
\author{
Yun-Hee Choi	
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{penmodel_c}, \link{print.penmodel_c}, \link{print.summary.penmodel_c} \link{plot.penmodel_c}}
}

\keyword{ summary }
