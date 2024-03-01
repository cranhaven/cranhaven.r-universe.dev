\name{plot.penmodel}
\alias{plot.penmodel}
\title{
Plot method for \code{penmodel}
}
\description{
Plots penetrance curves estimated from the fitted penetrance model and overlays non-parametric penetrance curves estimated from the data without proabands.
}
\usage{
\method{plot}{penmodel}(x, agemax = 80, print = TRUE, mark.time = FALSE, conf.int = FALSE, 
add.KM = TRUE, MC = 100, col = c("blue", "red", "blue", "red"), lty = c(1, 1, 2, 2), 
add.legend = TRUE, add.title = TRUE, xpos = "topleft", ypos = NULL, 
xlab = "Age at onset", ylab = "Penetrance", ylim = NULL, main = NULL,  ...)
}
\arguments{
  \item{x}{
An object class of \code{'penmodel'}, a fitted model by \code{\link{penmodel}} or \code{\link{penmodelEM}} functions.
}
\item{agemax}{
Maximum age of disease onset or maximum age. Default is 80 years of age.
}
 \item{print}{Logical; if \code{TRUE}, displays parameter estimates and penetrance estimates by age 70.}
 \item{mark.time}{Logical; if \code{TRUE}, curves are marked at each censoring time, otherwise, no labeling is done.}
 \item{conf.int}{Logical; if \code{TRUE}, displays 95\% confidence intervals for both parametric and non-parametric penetrance estimates for each subgroup and returns their \code{lower} and \code{upper} limits.}
 \item{add.KM}{Logical; if \code{TRUE}, displays Kaplan-Meier curves from data.}
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
\item{add.legend}{Logical; if \code{TRUE}, displays a legend in the plot.}
\item{add.title}{Logical; if \code{TRUE}, displays a title in the plot.}
\item{xpos, ypos}{
Position of legend; see \link{legend}. Defaults are \code{xpos = "topleft", ypos = NULL}.
}
\item{xlab}{Title for the x-axis. Default is \code{"Age at onset"}.}
\item{ylab}{Title for the y-axis. Default is \code{"Penetrance"}.}
\item{ylim}{Limits for the y-axis. Default is \code{NULL}. If \code{NULL}, \code{ylim} will be automatically determined.}
\item{main}{Main title of the plot. Default is \code{NULL}. If \code{NULL}, the title will be automatically created.}
 \item{...}{Other parameters to be passed through to plotting functions.}

}
\details{
The 95\% confidence intervals for the parametric penetrance curves are
obtained based on simulations of the parameters, assuming a multivariate normal distribution for the estimated
parameters with their variance-covariance matrix. See \code{\link{penetrance}} for more details.
}
\value{
Returns the following summary values:
\item{coefficients}{Parameter estimates of transformed baseline parameters (\eqn{\lambda, \rho}{lambda, rho}) and regression coefficients for gender and mutation status (\eqn{\beta_s, \beta_g}{beta.s, beta.g}).}
\item{pen70}{ Penetrance estimates by age 70, specific to gender and mutation-status subgroups.}
\item{x.age}{Vector of ages of onsest ranging from \code{agemin} to \code{agemax} years}
\item{pen}{Penetrance estimates at each age in \code{x.age}, specific to gender and mutation-status subgroups.}
\item{lower}{Lower limits of 95\% confidence interval estimates for penetrance at each age in \code{x.age}, specific to gender and mutation status subgroups.}
\item{upper}{Upper limits of 95\% confidence interval estimates for penetrance at each age in \code{x.age}, specific to gender and mutation status subgroups.}
}

\author{
Yun-Hee Choi	
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{penmodel}, \link{print.penmodel}, \link{penmodelEM}, \link{summary.penmodel},\link{print.summary.penmodel}}, 
\code{\link{simfam}	}
}
\examples{

# Simulated family data  

set.seed(4321)
fam <- simfam(N.fam = 300, design = "pop+", base.dist = "Weibull", variation = "none",
       base.parms = c(0.01,3), vbeta = c(-1.13, 2.35), allelefreq = 0.02, agemin = 20)
 
# Fit family data

fit <- penmodel(Surv(time, status) ~ gender + mgene, cluster = "famID", design = "pop+",
parms = c(0.01, 3, -1.13, 2.35), data = fam, base.dist = "Weibull", robust = TRUE)       

# Plot penetrance function curves with 95% CIs

plot(fit, agemax = 80, conf.int = TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Penetrance plot}
