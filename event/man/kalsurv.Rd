\name{kalsurv}
\title{Repeated Events Models with Frailty or Serial Dependence}
\alias{kalsurv}
\alias{deviance.kalsurv}
\alias{fitted.kalsurv}
\alias{print.kalsurv}
\alias{residuals.kalsurv}
\usage{
kalsurv(response, intensity="exponential", distribution="Pareto",
	depend="independence", update="Markov", mu=NULL, shape=NULL,
	renewal=TRUE, density=FALSE, censor=NULL, delta=NULL, ccov=NULL,
	tvcov=NULL, preg=NULL, ptvc=NULL, pbirth=NULL,
	pintercept=NULL, pshape=NULL, pinitial=1, pdepend=NULL,
	pfamily=NULL, envir=parent.frame(), print.level=0,
	ndigit=10, gradtol=0.00001, steptol=0.00001, iterlim=100,
	fscale=1, typsize=abs(p), stepmax=10*sqrt(p\%*\%p))
}
\description{
\code{kalsurv} is designed to handle event history models with
time-varying covariates. The distributions have two extra parameters
as compared to the functions specified by \code{intensity} and are
generally longer tailed than those distributions. Dependence of
inter-event times can be through gamma frailties (a type of random
effect), with or without autoregression, or several kinds of serial
dependence by updating, as in Kalman filtering.

By default, a gamma mixture of the distribution specified in
\code{intensity} is used, as the conditional distribution in the
\code{serial} dependence models, and as a symmetric multivariate
(random effect) model for \code{frailty} dependence. For example, with
a Weibull \code{intensity} and \code{frailty} dependence, this yields
a multivariate Burr distribution and with \code{Markov} or
\code{serial} dependence, univariate Burr conditional distributions.

If a value for \code{pfamily} is used, the gamma mixture is replaced
by a power variance family mixture.

Nonlinear regression models can be supplied as formulae where
parameters are unknowns in which case factor variables cannot be used and
parameters must be scalars. (See \code{\link[rmutil]{finterp}}.)

Marginal and individual profiles can be plotted using
\code{\link[rmutil]{mprofile}} and \code{\link[rmutil]{iprofile}} and
residuals with \code{\link[rmutil]{plot.residuals}}.
}
\arguments{
\item{response}{A list of vectors with times between events for
each individual, one matrix or dataframe of such times if all
individuals have the same number of events, or an object of class,
\code{response} (created by \code{\link[rmutil]{restovec}}) or \code{repeated}
(created by \code{\link[rmutil]{rmna}} or \code{\link[rmutil]{lvna}}). If the
\code{repeated} data object contains more than one response variable,
give that object in \code{envir} and give the name of the response
variable to be used here.}
\item{intensity}{The form of intensity function to be put in the
distribution given by dist. Choices are exponential, Weibull, gamma,
log normal, log logistic, log Cauchy, log Student, and gen(eralized)
logistic.}
\item{distribution}{The outer distribution. Choices are Pareto, gamma,
and Weibull.}
\item{depend}{Type of dependence. Choices are \code{independence},
\code{frailty}, and \code{serial}.}
\item{update}{Type of update for serial dependence. Choices are
\code{Markov}, \code{elapsed Markov}, \code{serial}, \code{event},
\code{cumulated}, \code{count}, and \code{kalman}. With \code{frailty}
dependence, weighting by length of observation time may be specified
by setting update to \code{time}.}
\item{mu}{A regression function for the location parameter or a
formula beginning with ~, specifying either a linear regression
function in the Wilkinson and Rogers notation or a general function
with named unknown parameters. Give the initial estimates in
\code{preg} if there are no time-varying covariates and in \code{ptvc}
if there are.}
\item{shape}{A regression function for the shape parameter or a
formula beginning with ~, specifying either a linear regression
function in the Wilkinson and Rogers notation or a general function
with named unknown parameters. It must yield one value per observation.}
\item{renewal}{IF TRUE, a renewal process is modelled, with time
reinitialized after each event. Otherwise, time is cumulated from the
origin of observations.}
\item{density}{If TRUE, the density of the function specified in
\code{intensity} is used instead of the intensity.}
\item{censor}{A vector of the same length as the number of individuals
containing a binary indicator, with a one indicating that the last
time period in the series terminated with an event and zero that it
was censored. For independence and frailty models, where response is
matrix, censor may also be a matrix of the same size. Ignored if
response has class, \code{response} or \code{repeated}.}
\item{delta}{Scalar or vector giving the unit of measurement for each
response value, set to unity by default. For example, if a response is
measured to two decimals, delta=0.01. If the response has been
pretransformed, this must be multiplied by the Jacobian. This
transformation cannot contain unknown parameters. For example, with a
log transformation, \code{delta=1/y}. (The delta values for the
censored response are ignored.) Ignored if response has class,
\code{response} or \code{repeated}.}
\item{ccov}{A vector or matrix containing time-constant baseline
covariates with one entry per individual, a model formula using
vectors of the same size, or an object of class, \code{tccov} (created
by \code{\link[rmutil]{tcctomat}}). If response has class, \code{repeated},
the covariates must be supplied as a Wilkinson and Rogers formula
unless none are to be used or \code{mu} is given.}
\item{tvcov}{A list of matrices with time-varying covariate values,
observed at the event times in \code{response}, for each individual
(one column per variable), one matrix or dataframe of such covariate
values, or an object of class, \code{tvcov} (created by
\code{\link[rmutil]{tvctomat}}). If response has class, \code{repeated}, the
covariates must be supplied as a Wilkinson and Rogers formula unless
none are to be used or \code{mu} is given.}
\item{preg}{Initial parameter estimates for the regression model:
intercept plus one for each covariate in \code{ccov}. If \code{mu} is
a formula or function, the parameter estimates must be given here only
if there are no time-varying covariates. If \code{mu} is
a formula with unknown parameters, their estimates must be supplied
either in their order of appearance in the expression or in a named
list.}
\item{ptvc}{Initial parameter estimates for the coefficients of the
time-varying covariates, as many as in \code{tvcov}. If \code{mu} is a
formula or function, the parameter estimates must be given here if
there are time-varying covariates present.}
\item{pbirth}{If supplied, this is the initial estimate for the
coefficient of the birth model.}
\item{pintercept}{The initial estimate of the intercept for the
generalized logistic intensity.}
\item{pshape}{An initial estimate for the shape parameter of the
intensity (except exponential intensity). If \code{shape} is
a function or formula, the corresponding initial estimates. If
\code{shape} is a formula with unknown parameters, their estimates
must be supplied either in their order of appearance in the expression
or in a named list.}
\item{pinitial}{An initial estimate for the initial parameter. In
\code{frailty} dependence, this is the frailty parameter.}
\item{pdepend}{An initial estimate for the serial dependence
parameter. For \code{frailty} dependence, if a value is given here, an
autoregression is fitted as well as the frailty.}
\item{pfamily}{An optional initial estimate for the second parameter
of a two-parameter power variance family mixture instead of the
default gamma mixture. This yields a gamma mixture as \code{family ->
0}, an inverse Gauss mixture for \code{family = 0.5}, and a compound
distribution of a Poisson-distributed number of gamma distributions
for \code{-1 < family < 0}.}
\item{envir}{Environment in which model formulae are to be
interpreted or a data object of class, \code{repeated}, \code{tccov},
or \code{tvcov}; the name of the response variable should be given in
\code{response}.
If \code{response} has class \code{repeated}, it is used as the
environment.}
\item{print.level}{\code{nlm} control options.}
\item{ndigit}{\code{nlm} control options.}
\item{gradtol}{\code{nlm} control options.}
\item{steptol}{\code{nlm} control options.}
\item{iterlim}{\code{nlm} control options.}
\item{fscale}{\code{nlm} control options.}
\item{typsize}{\code{nlm} control options.}
\item{stepmax}{\code{nlm} control options.}
}
\value{
A list of classes \code{kalsurv} and \code{recursive} is returned.
}
\author{J.K. Lindsey}
\seealso{
\code{\link[event]{coxre}}, 
\code{\link[rmutil]{finterp}},
\code{\link[rmutil]{gettvc}}, 
%\code{\link[repeated]{gnlmm}},
%\code{\link[gnlm]{gnlr}}, 
\code{\link[rmutil]{iprofile}},
%\code{\link[repeated]{kalcount}}, \code{\link[repeated]{kalseries}},
\code{\link[rmutil]{mprofile}}, 
%\code{\link[repeated]{nbkal}},
\code{\link[rmutil]{read.list}}, 
\code{\link[rmutil]{restovec}},
\code{\link[rmutil]{rmna}}, \code{\link[rmutil]{tcctomat}},
\code{\link[rmutil]{tvctomat}}.
}
\examples{
treat <- c(0,0,1,1)
tr <- tcctomat(treat)
cens <- matrix(rbinom(20,1,0.9),ncol=5)
times <- # matrix(rweibull(20,2,1+3*rep(treat,5)),ncol=5)
	matrix(c(1.36,0.18,0.84,0.65,1.44,1.79,1.04,0.43,1.35,1.63,2.15,1.15,
		1.21,5.46,1.58,3.44,4.40,2.75,4.78,2.44),ncol=5,byrow=TRUE)
times <- restovec(times, censor=cens)
reps <- rmna(times, ccov=tr)
# exponential intensity model with independence
kalsurv(times, pinitial=0.5, preg=1, dep="independence",
	intensity="exponential")
# Weibull intensity model with independence
kalsurv(times, pinitial=0.5, preg=1, pshape=1, dep="independence",
	intensity="Weibull")
# same model with serial update
kalsurv(times, pinitial=0.5, pdep=0.1, preg=1, pshape=1, dep="serial",
	intensity="Weibull")
# try power variance family instead of gamma distribution for mixture
kalsurv(times, pinitial=0.5, pdep=0.1, preg=1, pshape=1, dep="serial",
	intensity="Weibull", pfamily=0.1)
# treatment effect with log link
kalsurv(times, pinitial=0.5, preg=c(1,0), pshape=1, intensity="Weibull",
	ccov=treat)
# or equivalently
kalsurv(times, mu=~exp(a+b*treat), pinitial=0.1, preg=c(1,0), pshape=1,
	intensity="Weibull", envir=reps)
# with identity link instead
kalsurv(times, mu=~treat, pinitial=0.5, preg=c(1,0), pshape=1,
	intensity="Weibull")
# or equivalently
kalsurv(times, mu=~a+b*treat, pinitial=0.5, preg=c(1,0), pshape=1,
	intensity="Weibull", envir=reps)
# add the birth model
kalsurv(times, pinitial=0.5, preg=c(1,0), pshape=1,
	intensity="Weibull", ccov=treat, pbirth=0)
# try frailty dependence
kalsurv(times, pinitial=0.5, preg=c(1,0), pshape=1, dep="frailty",
	intensity="Weibull", ccov=treat)
# add autoregression
kalsurv(times, pinitial=0.5, preg=c(1,0), pshape=1, dep="frailty",
	pdep=0.1, intensity="Weibull", ccov=treat)
# switch to gamma intensity model
kalsurv(times, pinitial=0.5, preg=c(1,0), pshape=1, intensity="gamma",
	ccov=treat)
}
\keyword{models}
