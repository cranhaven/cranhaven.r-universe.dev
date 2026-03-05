\name{survkit}
\title{Weibull and Cox Models with Random Effects}
\alias{survkit}
\alias{baseline}
\alias{baseline.survivalkit}
\alias{print.survivalkit}
\alias{residuals.survivalkit}
\alias{survival}
\alias{survival.survivalkit}
\usage{
survkit(times, censor=NULL, ccov=NULL, tvcov=NULL,
	strata=NULL, id=NULL, model="Weibull", baseline=FALSE,
	residuals=FALSE, survival=NULL, svalues=NULL, valrho=NULL,
	constraints=NULL, impose=NULL, dist=NULL, random=NULL,
	estimate=NULL, moments=FALSE, rule=NULL, pedigree=NULL,
	integrate=NULL, jointmode=FALSE, within=NULL, converge=1.e-8,
	iterlim=100)
}
\description{
\code{survfit} was written in Fortran by Dr. V. Ducrocq (INRA, France:
vincent.ducrocq@dga.jouy.inra.fr) and Dr. J. Soelkner (Vienna:
soelkner@mail.boku.ac.at) to fit Weibull and Cox proportional hazards
models with random effects for very large data sets. This is a
cut-down version adapted to R. The full Survival Kit, including the
manual, can be obtained from http://www.boku.ac.at/nuwi/popgen.
}
\arguments{
\item{times}{Vector of times (events, right-censoring, change in
time-varying covariate, left-truncation).}
\item{censor}{Corresponding vector of censoring indicators. 1: event;
0: censored; -1: change of time-varying covariate; -2: left-truncation
time.}
\item{ccov}{Model formula for time-constant covariates. These may have
one value per individual or one per time. Because of the way factor
variables are handled, interactions must be coded as new variables.}
\item{tvcov}{Model formula for time-varying covariates with one value
per time. There can only be one change-point per individual. Again,
interactions must be coded as new variables.}
\item{strata}{A factor variable specifying stratification. With the
Weibull model, different intercepts and power parameters are
calculated for each stratum. For the Cox model, a different baseline
curve is fitted.}
\item{id}{A variable giving individual identification numbers
(starting at one). If not supplied, all times are assumed to refer to
different individuals.}
\item{model}{Weibull or Cox model, or Kaplan-Meier estimates.}
\item{baseline}{If TRUE, the baseline values are calculated for the
Cox model.}
\item{residuals}{If TRUE, calculate residuals (only for Cox model).}
\item{survival}{Calculate values of the survival function at
\code{quantiles},or at \code{equal}ly-spaced, \code{specific}, or
\code{all} observed times.}
\item{svalues}{A vector of quantile values (between 0 and 100),
spacing and maximum for equally-spaced, or specific times for
\code{survival}.}
\item{valrho}{A fixed value of the Weibull power parameter if it is
not to be estimated.}
\item{constraints}{By default, the category of each factor variable
with the \code{largest} number of events is taken as baseline. Other
options are \code{none} which gives values around the mean and
\code{find}. See also, \code{impose}.}
\item{impose}{A list of a vector of variable names and a corresponding
vector of their baseline category numbers. Any factor variables not
given will have their first category as baseline.}
\item{dist}{The distribution of the random effect: loggamma, normal,
or multivariate (normal).}
\item{random}{A factor variable specifying the random effect.}
\item{estimate}{One fixed value for the mode of the variance of the
random effect or three values if the mode is to be estimated: lower
and upper bounds, and precision.}
\item{moments}{Estimate the first three moments of the random effect
as well as the mode.}
\item{rule}{For the multivariate normal random effect, the genetic
relationships: \code{usual}, \code{mgs} (sire or father model), or
\code{sire.dam} (father and mother).}
\item{pedigree}{A matrix with four columns required for the
multivariate normal random effect, containing the individual
id, the sex, the father's category, and the mother's category.}
\item{integrate}{A factor variable to integrate out as the log-gamma
random effect in a Weibull model. (Not available for the Cox model.)}
\item{jointmode}{If TRUE, the log-gamma variance parameter is
estimated simultaneously with the other parameters using the
information in \code{estimate}. Otherwise, a fixed value, given in
\code{estimate} is assumed.}
\item{within}{A second factor variable (within the \code{integrate}
variable) to integrate out.}
\item{converge}{The convergence criterion, by default 1.e-8.}
\item{iterlim}{Maximum number of iterations.}
}
\author{V. Ducrocq, J. Soelkner, and J.K. Lindsey}
\seealso{
\code{\link[event]{coxre}}, \code{\link[event]{kalsurv}}.
}
\examples{
# y <- trunc(rweibull(20,2,20))
y <- c(6,22,43,16,7,6,15,35,10,9,18,34,7,13,10,17,14,19,11,13)
# cens <- rbinom(20,1,0.9)
cens <- c(1,1,1,1,0,1,1,1,1,1,1,1,0,1,1,1,1,1,1,1)
id <- gl(2,10)
# x <- rnorm(20)
x <- c(1.82881379,1.06606868,0.70877744,-0.09932880,-0.60626148,-0.75371046,
  0.23884069,0.51199483,-0.73060095,-0.93222151,2.27947539,-0.73855454,
 -0.36412735,-0.89122114,-0.05025962,-0.10001587,1.11460865,-1.87315971,
 -0.11280052,-1.6880509)
# Kaplan-Meier estimates
survkit(y, censor=cens, model="Kaplan")
# null Weibull model
survkit(y, censor=cens)
# one time-constant covariate
survkit(y, censor=cens, ccov=~x)
# stratify
survkit(y, censor=cens, ccov=~x, strata=id)
# estimate a normal random effect
survkit(y, censor=cens, ccov=~x, random=id, dist="normal",
	estimate=c(0.1,10,0.01), moments=TRUE)
# try a fixed value for the normal random effect
survkit(y, censor=cens, ccov=~x, random=id, dist="normal",
	estimate=1.3)
# estimate a log-gamma random effect
survkit(y, censor=cens, ccov=~x, random=id, dist="loggamma",
	estimate=c(0.1,10,0.01))
# estimate a log-gamma random effect by integrating it out
\dontrun{
survkit(y, censor=cens, ccov=~x, dist="loggamma", estimate=1.3,
	integ=id, jointmode=TRUE)
# try a fixed value of the log-gamma random effect, integrating it out
survkit(y, censor=cens, ccov=~x, dist="loggamma", estimate=1,
	integ=id)
}
#
# Cox model with one time-constant covariate
print(z <- survkit(y, censor=cens, ccov=~x, model="Cox", residuals=TRUE,
	baseline=TRUE))
residuals(z)
baseline(z)
# obtain the quantiles
print(z <- survkit(y, censor=cens, ccov=~x, model="Cox",
	survival="quantiles", svalues=seq(10,90,by=10)))
survival(z)
# estimate a log-gamma random effect
survkit(y, censor=cens, ccov=~x, model="Cox", random=id,
	dist="loggamma", estimate=c(0.1,10,0.01))
}
\keyword{models}
