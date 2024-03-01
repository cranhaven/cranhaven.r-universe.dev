\name{plot.penmodel_c}
\alias{plot.penmodel_c}
\title{
Plot method for \code{penmodel_c}
}
\description{
Plots penetrance curves for each event estimated from the fitted competing risks model and overlays non-parametric cumulative incidence curves estimated from the data without proabands.
}
\usage{
\method{plot}{penmodel_c}(x, agemax = 80, print = TRUE, conf.int = FALSE, 
add.CIF = TRUE, MC = 100, col = c("blue", "red", "blue", "red"), lty = c(1, 1, 2, 2), 
xlab = "Age at onset", ylab = "Penetrance", ylim = NULL,  ...)
}
\arguments{
  \item{x}{
An object class of \code{'penmodel'}, a fitted model by \code{\link{penmodel}} or \code{\link{penmodelEM}} functions.
}
\item{agemax}{
Maximum age of disease onset or maximum age. Default is 80 years of age.
}
\item{print}{Logical; if \code{TRUE}, displays parameter estimates and penetrance estimates by age 70.}
\item{conf.int}{Logical; if \code{TRUE}, displays 95\% confidence intervals for both parametric and non-parametric penetrance estimates for each subgroup and returns their \code{lower} and \code{upper} limits.}
 \item{add.CIF}{Logical; if \code{TRUE}, displays cumulative incidence curves from competing event data.}
 \item{MC}{
    Number of simulated samples used to calculate confidence intervals with a Monte-Carlo method. 
    If \code{MC = 0}, no confidence intervals will be calculated. Default value is 100.
  }
\item{col}{
Colors of lines for male carriers, female carriers, male noncarrers, and female noncarriers. Default is \code{c("blue", "red", "blue", "red")}.
}
\item{lty}{
Types of lines for male carriers, female carriers, male noncarriers, and female noncarriers. Default is \code{c(1, 1, 2, 2)}.
}
\item{xlab}{Title for the x-axis. Default is \code{"Age at onset"}.}
\item{ylab}{Title for the y-axis. Default is \code{"Penetrance"}.}
\item{ylim}{Limits for the y-axis. Default is \code{NULL}. If \code{NULL}, \code{ylim} will be automatically determined.}
\item{...}{Other parameters to be passed through to plotting functions.}

}
\details{
The 95\% confidence intervals for the parametric penetrance curves are
obtained based on simulations of the parameters, assuming a multivariate normal distribution for the estimated
parameters with their variance-covariance matrix. See \code{\link{penetrance_c}} for more details.
}
\value{
Returns the following summary values:
\item{coefficients}{Parameter estimates from the competing risks model.}
\item{pen70}{Penetrance estimates by age 70, specific to gender and mutation-status subgroups.}
\item{age}{Vector of ages of onsest ranging from \code{agemin} to \code{agemax} years}
\item{pen1}{Penetrance estimates for event 1 at each age in \code{age}, specific to gender and mutation-status subgroups.}
\item{pen2}{Penetrance estimates for event 2 at each age in \code{age}, specific to gender and mutation-status subgroups.}
\item{lower1}{Lower limits of 95\% confidence interval estimates for penetrance for event 1 at each age in \code{age}, specific to gender and mutation status subgroups.}
\item{upper1}{Upper limits of 95\% confidence interval estimates for penetrance for event 1 at each age in \code{x.age}, specific to gender and mutation status subgroups.}
\item{lower2}{Lower limits of 95\% confidence interval estimates for penetrance for event 2 at each age in \code{age}, specific to gender and mutation status subgroups.}
\item{upper2}{Upper limits of 95\% confidence interval estimates for penetrance for event 2 at each age in \code{x.age}, specific to gender and mutation status subgroups.}
}

\author{
Yun-Hee Choi	
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{penmodel_c}, \link{print.penmodel_c}, \link{summary.penmodel_c},
\link{print.summary.penmodel_c}, \link{simfam_c}	}
}
\examples{

\dontrun{
# Simulated family data  
set.seed(4321)
fam2 <- simfam_c(N.fam = 500, design = "pop+", variation = "frailty", competing=TRUE,
                  base.dist = "Weibull", frailty.dist = "cgamma", depend=c(2, 2, 2), 
                  allelefreq = 0.02, base.parms = list(c(0.01, 3), c(0.01, 3)), 
                  vbeta = list(c(-1.13, 2.35),c(-1, 2)))

# Fit family data
fit2 <- penmodel_c(formula1 = Surv(time, status==1)~ gender + mgene, 
                   formula2 = Surv(time, status==2)~ gender + mgene, 
                   cluster = "famID", gvar = "mgene", frailty.dist = "cgamma",
                   parms=list(c(0.01, 3, -1, 2.3), c(0.01, 3, -1, 2), c(2, 2, 2)), 
                   data=fam2, design="pop+", base.dist="Weibull", robust=TRUE)

# Plot penetrance function curves with 95% CIs
plot(fit2, conf.int=TRUE, MC=200, ylim=c(0, 0.7))

}


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Penetrance plot}
