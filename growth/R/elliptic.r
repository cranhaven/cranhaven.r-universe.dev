#
#  growth : A Library of Normal Distribution Growth Curve Models
#  Copyright (C) 1998, 1999, 2000, 2001 J.K. Lindsey
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public Licence as published by
#  the Free Software Foundation; either version 2 of the Licence, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public Licence for more details.
#
#  You should have received a copy of the GNU General Public Licence
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#  SYNOPSIS
#
#     elliptic(response=NULL, model="linear", distribution="normal",
#	times=NULL, dose=NULL, ccov=NULL, tvcov=NULL, nest=NULL, torder=0,
#	interaction=NULL, transform="identity", link="identity",
#	autocorr="exponential", pell=NULL, preg=NULL, pvar=var(y), varfn=NULL,
#	covfn=NULL, par=NULL, pre=NULL, delta=NULL, shfn=FALSE, common=FALSE,
#	twins=FALSE, envir=parent.frame(), print.level=0, ndigit=10,
#	gradtol=0.00001, steptol=0.00001, iterlim=100, fscale=1,
#	stepmax=10*sqrt(theta%*%theta), typsize=abs(c(theta)))
#
#  DESCRIPTION
#
#    Function to fit the multivariate elliptical distribution with
# various autocorrelation functions, one or two levels of random
# effects, and nonlinear regression.



#' Nonlinear Multivariate Elliptically-contoured Repeated Measurements Models
#' with AR(1) and Two Levels of Variance Components
#' 
#' \code{elliptic} fits special cases of the multivariate
#' elliptically-contoured distribution, the multivariate normal, Student t, and
#' power exponential distributions. The latter includes the multivariate normal
#' (power=1), a multivariate Laplace (power=0.5), and the multivariate uniform
#' (power -> infinity) distributions as special cases. As well, another form of
#' multivariate skew Laplace distribution is also available.
#' 
#' With two levels of nesting, the first is the individual and the second will
#' consist of clusters within individuals.
#' 
#' For clustered (non-longitudinal) data, where only random effects will be
#' fitted, \code{times} are not necessary.
#' 
#' This function is designed to fit linear and nonlinear models with
#' time-varying covariates observed at arbitrary time points. A continuous-time
#' AR(1) and zero, one, or two levels of nesting can be handled. Recall that
#' zero correlation (all zeros off-diagonal in the covariance matrix) only
#' implies independence for the multivariate normal distribution.
#' 
#' Nonlinear regression models can be supplied as formulae where parameters are
#' unknowns in which case factor variables cannot be used and parameters must
#' be scalars. (See \code{\link[rmutil]{finterp}}.)
#' 
#' Recursive fitted values and residuals are only available for the
#' multivariate normal distribution with a linear model without a variance
#' function and with either an AR(1) of \code{exponential} form and/or one
#' level of random effect. In these cases, marginal and individual profiles can
#' be plotted using \code{\link[rmutil]{mprofile}} and
#' \code{\link[rmutil]{iprofile}} and residuals with
#' \code{\link[rmutil]{plot.residuals}}.
#' 
#' 
#' @aliases elliptic fitted.elliptic residuals.elliptic
#' @param response A list of two or three column matrices with response values,
#' times, and possibly nesting categories, for each individual, one matrix or
#' dataframe of response values, or an object of class, \code{response}
#' (created by \code{\link[rmutil]{restovec}}) or \code{repeated} (created by
#' \code{\link[rmutil]{rmna}} or \code{\link[rmutil]{lvna}}). If the
#' \code{repeated} data object contains more than one response variable, give
#' that object in \code{envir} and give the name of the response variable to be
#' used here.
#' @param model The model to be fitted for the location. Builtin choices are
#' (1) \code{linear} for linear models with time-varying covariate; if
#' \code{torder > 0}, a polynomial in time is automatically fitted; (2)
#' \code{logistic} for a four-parameter logistic growth curve; (3) \code{pkpd}
#' for a first-order one-compartment pharmacokinetic model. Otherwise, set this
#' to a function of the parameters or a formula beginning with ~, specifying
#' either a linear regression function for the location parameter in the
#' Wilkinson and Rogers notation or a general function with named unknown
#' parameters that describes the location, returning a vector the same length
#' as the number of observations, in which case \code{ccov} and \code{tvcov}
#' cannot be used.
#' @param distribution Multivariate \code{normal}, \code{power exponential},
#' \code{Student t}, or skew \code{Laplace} distribution. The latter is not an
#' elliptical distribution. Note that the latter has a different
#' parametrization of the skew (family) parameter than the univariate skew
#' Laplace distribution in \code{\link[rmutil]{dskewlaplace}}:
#' \eqn{skew=\frac{\sigma(1-\nu^2)}{\sqrt{2}\nu}}{skew = (s * (1 - f^2)) /
#' (sqrt(2) * f)}. Here, zero skew yields a symmetric distribution.
#' @param times When \code{response} is a matrix, a vector of possibly
#' unequally spaced times when they are the same for all individuals or a
#' matrix of times. Not necessary if equally spaced. Ignored if response has
#' class, \code{response} or \code{repeated}.
#' @param dose A vector of dose levels for the \code{pkpd model}, one per
#' individual.
#' @param ccov A vector or matrix containing time-constant baseline covariates
#' with one line per individual, a model formula using vectors of the same
#' size, or an object of class, \code{tccov} (created by
#' \code{\link[rmutil]{tcctomat}}). If response has class, \code{repeated},
#' with a \code{linear}, \code{logistic}, or \code{pkpd} model, the covariates
#' must be specified as a Wilkinson and Rogers formula unless none are to be
#' used. For the \code{pkpd} and \code{logistic} models, all variables must be
#' binary (or factor variables) as different values of all parameters are
#' calculated for all combinations of these variables (except for the logistic
#' model when a time-varying covariate is present). It cannot be used when
#' model is a function.
#' @param tvcov A list of vectors or matrices with time-varying covariates for
#' each individual (one column per variable), a matrix or dataframe of such
#' covariate values (if only one covariate), or an object of class,
#' \code{tvcov} (created by \code{\link[rmutil]{tvctomat}}). If times are not
#' the same as for responses, the list can be created with
#' \code{\link[rmutil]{gettvc}}. If response has class, \code{repeated}, with a
#' \code{linear}, \code{logistic}, or \code{pkpd} model, the covariates must be
#' specified as a Wilkinson and Rogers formula unless none are to be used. Only
#' one time-varying covariate is allowed except for the \code{linear model}; if
#' more are required, set \code{model} equal to the appropriate mean function.
#' This argument cannot be used when model is a function.
#' @param nest When \code{response} is a matrix, a vector of length equal to
#' the number of responses per individual indicating which responses belong to
#' which nesting category. Categoriess must be consecutive increasing integers.
#' This option should always be specified if nesting is present. Ignored if
#' response has class, \code{repeated}.
#' @param torder When the \code{linear model} is chosen, order of the
#' polynomial in time to be fitted.
#' @param interaction Vector of length equal to the number of time-constant
#' covariates, giving the levels of interactions between them and the
#' polynomial in time in the \code{linear model}.
#' @param transform Transformation of the response variable: \code{identity},
#' \code{exp}, \code{square}, \code{sqrt}, or \code{log}.
#' @param link Link function for the location: \code{identity}, \code{exp},
#' \code{square}, \code{sqrt}, or \code{log}. For the \code{linear model}, if
#' not the \code{identity}, initial estimates of the regression parameters must
#' be supplied (intercept, polynomial in time, time-constant covariates,
#' time-varying covariates, in that order).
#' @param autocorr The form of the autocorrelation function: \code{exponential}
#' is the usual \eqn{\rho^{|t_i-t_j|}}{rho^|t_i-t_j|}; \code{gaussian} is
#' \eqn{\rho^{(t_i-t_j)^2}}{rho^((t_i-t_j)^2)}; \code{cauchy} is
#' \eqn{1/(1+\rho(t_i-t_j)^2)}{1/(1+rho(t_i-t_j)^2)}; \code{spherical} is
#' \eqn{((|t_i-t_j|\rho)^3-3|t_i-t_j|\rho+2)/2}{((|t_i-t_j|rho)^3-3|t_i-t_j|rho+2)/2}
#' for \eqn{|t_i-t_j|\leq1/\rho}{|t_i-t_j|<=1/rho} and zero otherwise;
#' \code{IOU} is the integrated Ornstein-Uhlenbeck process, \eqn{(2\rho
#' \min(t_i,t_j)+\exp(-\rho t_i) }{(2rho min(t_i,t_j)+exp(-rho t_i) +exp(-rho
#' t_j)-1 -exp(rho|ti-t_j|))/2rho^3}\eqn{+\exp(-\rho t_j)-1
#' -\exp(\rho|ti-t_j|))/2\rho^3}{(2rho min(t_i,t_j)+exp(-rho t_i) +exp(-rho
#' t_j)-1 -exp(rho|ti-t_j|))/2rho^3}.
#' @param pell Initial estimate of the power parameter of the multivariate
#' power exponential distribution, of the degrees of freedom parameter of the
#' multivariate Student t distribution, or of the asymmetry parameter of the
#' multivariate Laplace distribution. If not supplied for the latter, asymmetry
#' depends on the regression equation in \code{model}.
#' @param preg Initial parameter estimates for the regression model. Only
#' required for \code{linear model} if the \code{link} is not the
#' \code{identity} or a variance (dispersion) function is fitted.
#' @param pvar Initial parameter estimate for the variance or dispersion. If
#' more than one value is provided, the log variance/dispersion depends on a
#' polynomial in time. With the \code{pkpd model}, if four values are supplied,
#' a nonlinear regression for the variance/dispersion is fitted.
#' @param varfn The builtin variance (dispersion) function has the
#' variance/dispersion proportional to a function of the location: pvar*v(mu) =
#' \code{identity} or \code{square}. If pvar contains two initial values, an
#' additive constant is included: pvar(1)+pvar(2)*v(mu). Otherwise, either a
#' function or a formula beginning with ~, specifying either a linear
#' regression function in the Wilkinson and Rogers notation or a general
#' function with named unknown parameters for the log variance can be supplied.
#' If it contains unknown parameters, the keyword \code{mu} may be used to
#' specify a function of the location parameter.
#' @param covfn Either a function or a formula beginning with ~, specifying how
#' the covariance depends on covariates: either a linear regression function in
#' the Wilkinson and Rogers notation or a general function with named unknown
#' parameters.
#' @param pre Zero, one or two parameter estimates for the variance components,
#' depending on the number of levels of nesting. If covfn is specified, this
#' contains the initial estimates of the regression parameters.
#' @param par If supplied, an initial estimate for the autocorrelation
#' parameter.
#' @param delta Scalar or vector giving the unit of measurement for each
#' response value, set to unity by default. For example, if a response is
#' measured to two decimals, \code{delta=0.01}. Ignored if response has class,
#' \code{response} or \code{repeated}.
#' @param shfn If TRUE, the supplied variance (dispersion) function depends on
#' the mean function. The name of this mean function must be the last argument
#' of the variance/dispersion function.
#' @param common If TRUE, \code{mu} and \code{varfn} must both be either
#' functions with, as argument, a vector of parameters having some or all
#' elements in common between them so that indexing is in common between them
#' or formulae with unknowns. All parameter estimates must be supplied in
#' \code{preg}. If FALSE, parameters are distinct between the two functions and
#' indexing starts at one in each function.
#' @param envir Environment in which model formulae are to be interpreted or a
#' data object of class, \code{repeated}, \code{tccov}, or \code{tvcov}; the
#' name of the response variable should be given in \code{response}. If
#' \code{response} has class \code{repeated}, it is used as the environment.
#' @param twins Only possible when there are two observations per individual
#' (e.g. twin data). If TRUE and \code{covfn} is supplied, allows the
#' covariance to vary across pairs of twins with the diagonal "variance" of the
#' covariance matrix remaining constant.
##' @param print.level Arguments for nlm.
##' @param typsize Arguments for nlm.
##' @param ndigit Arguments for nlm.
##' @param gradtol Arguments for nlm.
##' @param stepmax Arguments for nlm.
##' @param steptol Arguments for nlm.
##' @param iterlim Arguments for nlm.
##' @param fscale Arguments for nlm.
#' @param object An object of class, \code{elliptic}.
#' @param x An object of class, \code{elliptic}.
#' @param recursive If TRUE, recursive residuals or fitted values are given;
#' otherwise, marginal ones. In all cases, raw residuals are returned, not
#' standardized by the standard deviation (which may be changing with
#' covariates or time).
#' @param correlation logical; print correlations.
#' @param digits number of digits to print.
#' @param ... additional arguments.
#' @return A list of class \code{elliptic} is returned that contains all of the
#' relevant information calculated, including error codes.
#' @author J.K. Lindsey
#' @seealso \code{\link[growth]{carma}}, \code{\link[rmutil]{dpowexp}},
#' \code{\link[rmutil]{dskewlaplace}}, \code{\link[rmutil]{finterp}},
#' \code{\link[repeated]{gar}}, \code{\link[rmutil]{gettvc}},
#' \code{\link[repeated]{gnlmix}}, \code{\link[repeated]{glmm}},
#' \code{\link[repeated]{gnlmm}}, \code{\link[gnlm]{gnlr}},
#' \code{\link[rmutil]{iprofile}}, \code{\link[repeated]{kalseries}},
#' \code{\link[rmutil]{mprofile}}, \code{\link[growth]{potthoff}},
#' \code{\link[rmutil]{read.list}}, \code{\link[rmutil]{restovec}},
#' \code{\link[rmutil]{rmna}}, \code{\link[rmutil]{tcctomat}},
#' \code{\link[rmutil]{tvctomat}}.
#' @references Lindsey, J.K. (1999) Multivariate elliptically-contoured
#' distributions for repeated measurements. Biometrics 55, 1277-1280.
#' 
#' Kotz, S., Kozubowski, T.J., and Podgorski, K. (2001) The Laplace
#' Distribution and Generalizations. A Revisit with Applications to
#' Communications, Economics, Engineering, and Finance. Basel: Birkhauser, Ch.
#' 6.
#' @keywords models
#' @examples
#' 
#' # linear models
#' y <- matrix(rnorm(40),ncol=5)
#' x1 <- gl(2,4)
#' x2 <- gl(2,1,8)
#' # independence with time trend
#' elliptic(y, ccov=~x1, torder=2)
#' # AR(1)
#' elliptic(y, ccov=~x1, torder=2, par=0.1)
#' elliptic(y, ccov=~x1, torder=3, interact=3, par=0.1)
#' # random intercept
#' elliptic(y, ccov=~x1+x2, interact=c(2,0), torder=3, pre=2)
#' #
#' # nonlinear models
#' time <- rep(1:20,2)
#' dose <- c(rep(2,20),rep(5,20))
#' mu <- function(p) exp(p[1]-p[3])*(dose/(exp(p[1])-exp(p[2]))*
#' 	(exp(-exp(p[2])*time)-exp(-exp(p[1])*time)))
#' shape <- function(p) exp(p[1]-p[2])*time*dose*exp(-exp(p[1])*time)
#' conc <- matrix(rnorm(40,mu(log(c(1,0.3,0.2))),sqrt(shape(log(c(0.1,0.4))))),
#' 	ncol=20,byrow=TRUE)
#' conc[,2:20] <- conc[,2:20]+0.5*(conc[,1:19]-matrix(mu(log(c(1,0.3,0.2))),
#' 	ncol=20,byrow=TRUE)[,1:19])
#' conc <- ifelse(conc>0,conc,0.01)
#' # with builtin function
#' # independence
#' elliptic(conc, model="pkpd", preg=log(c(0.5,0.4,0.1)), dose=c(2,5))
#' # AR(1)
#' elliptic(conc, model="pkpd", preg=log(c(0.5,0.4,0.1)), dose=c(2,5),
#' 	par=0.1)
#' # add variance function
#' elliptic(conc, model="pkpd", preg=log(c(0.5,0.4,0.1)), dose=c(2,5),
#' 	par=0.1, varfn=shape, pvar=log(c(0.5,0.2)))
#' # multivariate power exponential distribution
#' elliptic(conc, model="pkpd", preg=log(c(0.5,0.4,0.1)), dose=c(2,5),
#' 	par=0.1, varfn=shape, pvar=log(c(0.5,0.2)), pell=1,
#' 	distribution="power exponential")
#' # multivariate Student t distribution
#' elliptic(conc, model="pkpd", preg=log(c(0.5,0.4,0.1)), dose=c(2,5),
#' 	par=0.1, varfn=shape, pvar=log(c(0.5,0.2)), pell=5,
#' 	distribution="Student t")
#' # multivariate Laplace distribution
#' elliptic(conc, model="pkpd", preg=log(c(0.5,0.4,0.1)), dose=c(2,5),
#' 	par=0.1, varfn=shape, pvar=log(c(0.5,0.2)),
#' 	distribution="Laplace")
#' # or equivalently with user-specified function
#' # independence
#' elliptic(conc, model=mu, preg=log(c(0.5,0.4,0.1)))
#' # AR(1)
#' elliptic(conc, model=mu, preg=log(c(0.5,0.4,0.1)), par=0.1)
#' # add variance function
#' elliptic(conc, model=mu, preg=log(c(0.5,0.4,0.1)), par=0.1,
#' 	varfn=shape, pvar=log(c(0.5,0.2)))
#' # multivariate power exponential distribution
#' elliptic(conc, model=mu, preg=log(c(0.5,0.4,0.1)), par=0.1,
#' 	varfn=shape, pvar=log(c(0.5,0.2)), pell=1,
#' 	distribution="power exponential")
#' # multivariate Student t distribution
#' elliptic(conc, model=mu, preg=log(c(0.5,0.4,0.1)), par=0.1,
#' 	varfn=shape, pvar=log(c(0.5,0.2)), pell=5,
#' 	distribution="Student t")
#' # multivariate Laplace distribution
#' elliptic(conc, model=mu, preg=log(c(0.5,0.4,0.1)), par=0.1,
#' 	varfn=shape, pvar=log(c(0.5,0.2)), pell=5,
#' 	distribution="Laplace")
#' # or with user-specified formula
#' # independence
#' elliptic(conc, model=~exp(absorption-volume)*
#' 	dose/(exp(absorption)-exp(elimination))*
#' 	(exp(-exp(elimination)*time)-exp(-exp(absorption)*time)),
#' 	preg=list(absorption=log(0.5),elimination=log(0.4),
#' 	volume=log(0.1)))
#' # AR(1)
#' elliptic(conc, model=~exp(absorption-volume)*
#' 	dose/(exp(absorption)-exp(elimination))*
#' 	(exp(-exp(elimination)*time)-exp(-exp(absorption)*time)),
#' 	preg=list(absorption=log(0.5),elimination=log(0.4),volume=log(0.1)),
#' 	par=0.1)
#' # add variance function
#' elliptic(conc, model=~exp(absorption-volume)*
#' 	dose/(exp(absorption)-exp(elimination))*
#' 	(exp(-exp(elimination)*time)-exp(-exp(absorption)*time)),
#' 	preg=list(absorption=log(0.5),elimination=log(0.4),volume=log(0.1)),
#' 	varfn=~exp(b1-b2)*time*dose*exp(-exp(b1)*time),
#' 	par=0.1, pvar=list(b1=log(0.5),b2=log(0.2)))
#' # variance as function of the mean
#' elliptic(conc, model=~exp(absorption-volume)*
#' 	dose/(exp(absorption)-exp(elimination))*
#' 	(exp(-exp(elimination)*time)-exp(-exp(absorption)*time)),
#' 	preg=list(absorption=log(0.5),elimination=log(0.4),volume=log(0.1)),
#' 	varfn=~d*log(mu),shfn=TRUE,par=0.1, pvar=list(d=1))
#' # multivariate power exponential distribution
#' elliptic(conc, model=~exp(absorption-volume)*
#' 	dose/(exp(absorption)-exp(elimination))*
#' 	(exp(-exp(elimination)*time)-exp(-exp(absorption)*time)),
#' 	preg=list(absorption=log(0.5),elimination=log(0.4),volume=log(0.1)),
#' 	varfn=~exp(b1-b2)*time*dose*exp(-exp(b1)*time),
#' 	par=0.1, pvar=list(b1=log(0.5),b2=log(0.2)), pell=1,
#' 	distribution="power exponential")
#' # multivariate Student t distribution
#' elliptic(conc, model=~exp(absorption-volume)*
#' 	dose/(exp(absorption)-exp(elimination))*
#' 	(exp(-exp(elimination)*time)-exp(-exp(absorption)*time)),
#' 	preg=list(absorption=log(0.5),elimination=log(0.4),volume=log(0.1)),
#' 	varfn=~exp(b1-b2)*time*dose*exp(-exp(b1)*time),
#' 	par=0.1, pvar=list(b1=log(0.5),b2=log(0.2)), pell=5,
#' 	distribution="Student t")
#' # multivariate Laplace distribution
#' elliptic(conc, model=~exp(absorption-volume)*
#' 	dose/(exp(absorption)-exp(elimination))*
#' 	(exp(-exp(elimination)*time)-exp(-exp(absorption)*time)),
#' 	preg=list(absorption=log(0.5),elimination=log(0.4),volume=log(0.1)),
#' 	varfn=~exp(b1-b2)*time*dose*exp(-exp(b1)*time),
#' 	par=0.1, pvar=list(b1=log(0.5),b2=log(0.2)), pell=5,
#' 	distribution="Laplace")
#' #
#' # generalized logistic regression with square-root transformation
#' # and square  link
#' time <- rep(seq(10,200,by=10),2)
#' mu <- function(p) {
#' 	yinf <- exp(p[2])
#' 	yinf*(1+((yinf/exp(p[1]))^p[4]-1)*exp(-yinf^p[4]
#' 		*exp(p[3])*time))^(-1/p[4])}
#' y <- matrix(rnorm(40,sqrt(mu(c(2,1.5,0.05,-2))),0.05)^2,ncol=20,byrow=TRUE)
#' y[,2:20] <- y[,2:20]+0.5*(y[,1:19]-matrix(mu(c(2,1.5,0.05,-2)),
#' 	ncol=20,byrow=TRUE)[,1:19])
#' y <- ifelse(y>0,y,0.01)
#' # with builtin function
#' # independence
#' elliptic(y, model="logistic", preg=c(2,1,0.1,-1), trans="sqrt",
#' 	link="square")
#' # the same model with AR(1)
#' elliptic(y, model="logistic", preg=c(2,1,0.1,-1), trans="sqrt",
#' 	link="square", par=0.4)
#' # the same model with AR(1) and one component of variance
#' elliptic(y, model="logistic", preg=c(2,1,0.1,-1),
#' 	trans="sqrt", link="square", pre=1, par=0.4)
#' # or equivalently with user-specified function
#' # independence
#' elliptic(y, model=mu, preg=c(2,1,0.1,-1), trans="sqrt",
#' 	link="square")
#' # the same model with AR(1)
#' elliptic(y, model=mu, preg=c(2,1,0.1,-1), trans="sqrt",
#' 	link="square", par=0.4)
#' # the same model with AR(1) and one component of variance
#' elliptic(y, model=mu, preg=c(2,1,0.1,-1),
#' 	trans="sqrt", link="square", pre=1, par=0.4)
#' # or equivalently with user-specified formula
#' # independence
#' elliptic(y, model=~exp(yinf)*(1+((exp(yinf-y0))^b4-1)*
#' 	exp(-exp(yinf*b4+b3)*time))^(-1/b4),
#' 	preg=list(y0=2,yinf=1,b3=0.1,b4=-1), trans="sqrt", link="square")
#' # the same model with AR(1)
#' elliptic(y, model=~exp(yinf)*(1+((exp(yinf-y0))^b4-1)*
#' 	exp(-exp(yinf*b4+b3)*time))^(-1/b4),
#' 	preg=list(y0=2,yinf=1,b3=0.1,b4=-1), trans="sqrt",
#' 	link="square", par=0.1)
#' # add one component of variance
#' elliptic(y, model=~exp(yinf)*(1+((exp(yinf-y0))^b4-1)*
#' 	exp(-exp(yinf*b4+b3)*time))^(-1/b4),
#' 	preg=list(y0=2,yinf=1,b3=0.1,b4=-1),
#' 	trans="sqrt", link="square", pre=1, par=0.1)
#' #
#' # multivariate power exponential and Student t distributions for outliers
#' y <- matrix(rcauchy(40,mu(c(2,1.5,0.05,-2)),0.05),ncol=20,byrow=TRUE)
#' y[,2:20] <- y[,2:20]+0.5*(y[,1:19]-matrix(mu(c(2,1.5,0.05,-2)),
#' 	ncol=20,byrow=TRUE)[,1:19])
#' y <- ifelse(y>0,y,0.01)
#' # first with normal distribution
#' elliptic(y, model="logistic", preg=c(1,1,0.1,-1))
#' elliptic(y, model="logistic", preg=c(1,1,0.1,-1), par=0.5)
#' # then power exponential
#' elliptic(y, model="logistic", preg=c(1,1,0.1,-1), pell=1,
#' 	distribution="power exponential")
#' elliptic(y, model="logistic", preg=c(1,1,0.1,-1), par=0.5, pell=1,
#' 	distribution="power exponential")
#' # finally Student t
#' elliptic(y, model="logistic", preg=c(1,1,0.1,-1), pell=1,
#' 	distribution="Student t")
#' elliptic(y, model="logistic", preg=c(1,1,0.1,-1), par=0.5, pell=1,
#' 	distribution="Student t")
#' 
#' @export elliptic
##' @importFrom graphics abline lines plot 
##' @importFrom stats contr.poly fft model.frame model.matrix na.fail nlm terms var
##' @import rmutil
##' @useDynLib growth, .registration = TRUE
elliptic <- function(response=NULL, model="linear", distribution="normal",
	times=NULL, dose=NULL, ccov=NULL, tvcov=NULL, nest=NULL,
	torder=0, interaction=NULL, transform="identity",
	link="identity", autocorr="exponential", pell=NULL, preg=NULL,
	covfn=NULL, pvar=var(y), varfn=NULL, par=NULL, pre=NULL, delta=NULL,
	shfn=FALSE, common=FALSE, twins=FALSE, envir=parent.frame(),
	print.level=0, ndigit=10, gradtol=0.00001, steptol=0.00001,
	iterlim=100, fscale=1, stepmax=10*sqrt(theta%*%theta),
	typsize=abs(c(theta))){
#
# likelihood function returning parameter values
#
plra <- function(theta){
	if(mdl==2)mu <- mu1(theta)
	if(cvar==1)varn <- sh1(theta)
	if(!is.null(covfn))covn <- cv1(theta)
	z <- .Fortran("plra",
		theta=as.double(theta),
		like=double(1),
		dist=as.integer(dst),
		as.double(rxl),
		x=as.double(times),
		as.double(y),
		tvcov=as.double(resp$tvcov$tvcov),
		ccov=as.double(resp$ccov$ccov),
		dose=as.double(dose),
		nobs=as.integer(nobs),
		nbs=as.integer(n),
		nest=as.integer(resp$response$nest),
		lnest=as.integer(lnest),
		dev=double(nm),
		nind=as.integer(nind),
		nld=as.integer(nld),
		nxrl=as.integer(nxrl),
		np=as.integer(np),
		npell=as.integer(npell),
		npv=as.integer(npv),
		npvl=as.integer(npvl),
		nccov=as.integer(nccov),
		npvar=as.integer(npvar),
		cvar=as.integer(cvar),
		ccvar=as.integer(!is.null(covfn)),
		twins=as.integer(twins),
		npre=as.integer(npre),
		npar=as.integer(npar),
		link=as.integer(lnk),
		torder=as.integer(torder),
		inter=as.integer(interaction),
		model=as.integer(mdl),
		ar=as.integer(ar),
		tvc=as.integer(ntvc),
		beta=double(npv2),
		betacov=double(npv2*npv2),
		v=double(nld*nld),
		sigsq=double(nld),
		ey=double(nld),
		tb=double(npvl),
		as.double(mu),
		as.double(varn),
		as.double(covn),
		#DUP=FALSE,
		PACKAGE="growth")
	list(like=z$like,res=z$dev,beta=z$beta,betacov=z$betacov)}
#
# likelihood function for nlm
#
plral <- function(theta){
	if(mdl==2)mu <- mu1(theta)
	if(cvar==1)varn <- sh1(theta)
	if(!is.null(covfn))covn <- cv1(theta)
	z <- .Fortran("plra",
		theta=as.double(theta),
		like=double(1),
		dist=as.integer(dst),
		as.double(rxl),
		x=as.double(times),
		as.double(y),
		tvcov=as.double(resp$tvcov$tvcov),
		ccov=as.double(resp$ccov$ccov),
		dose=as.double(dose),
		nobs=as.integer(nobs),
		nbs=as.integer(n),
		nest=as.integer(resp$response$nest),
		lnest=as.integer(lnest),
		dev=double(nm),
		nind=as.integer(nind),
		nld=as.integer(nld),
		nxrl=as.integer(nxrl),
		np=as.integer(np),
		npell=as.integer(npell),
		npv=as.integer(npv),
		npvl=as.integer(npvl),
		nccov=as.integer(nccov),
		npvar=as.integer(npvar),
		cvar=as.integer(cvar),
		ccvar=as.integer(!is.null(covfn)),
		twins=as.integer(twins),
		npre=as.integer(npre),
		npar=as.integer(npar),
		link=as.integer(lnk),
		torder=as.integer(torder),
		inter=as.integer(interaction),
		model=as.integer(mdl),
		ar=as.integer(ar),
		tvc=as.integer(ntvc),
		beta=double(npv2),
		betacov=double(npv2*npv2),
		v=double(nld*nld),
		sigsq=double(nld),
		ey=double(nld),
		tb=double(npvl),
		as.double(mu),
		as.double(varn),
		as.double(covn),
		#DUP=FALSE,
		PACKAGE="growth")
	z$like}
call <- sys.call()
#
# check type of model to be fitted
#
if(!is.function(model)&&!inherits(model,"formula")&&!is.null(model))
	model <- match.arg(model,c("linear","logistic","pkpd"))
tmp <- c("exponential","gaussian","cauchy","spherical","IOU")
ar <- match(autocorr <- match.arg(autocorr,tmp),tmp)
tmp <- c("normal","power exponential","Student t","Laplace")
dst <- match(distribution <- match.arg(distribution,tmp),tmp)
if(dst==1)pell <- NULL
npell <- !is.null(pell)
if(!npell){
	if(dst==2)
		stop("An estimate of the elliptic power parameter must be supplied")
	else if(dst==3)
		stop("An estimate of the degrees of freedom must be supplied")}
if(npell&&pell<=0){
	if(dst==2)stop("The elliptic power parameter must be positive.")
	else if(dst==3)
		stop("The degrees of freedom parameter must be positive.")}
#
# check transformation and link
#
transform <- match.arg(transform,c("identity","exp","square","sqrt","log"))
tmp <- c("identity","exp","square","sqrt","log")
lnk <- match(link <- match.arg(link,tmp),tmp)
#
# check if mean and variance can have common parameters
#
if(common){
	if(!is.function(model)&&!inherits(model,"formula"))
		stop("with common parameters, model must be a function or formula")
	if(!is.function(varfn)&&!inherits(varfn,"formula"))
		stop("with common parameters, varfn must be a function or formula")
	pvar <- NULL}
npar <- length(par)
ntvc <- if(!is.null(tvcov))1 else 0
#
# check if a data object is being supplied
#
respenv <- exists(deparse(substitute(response)),envir=parent.frame())&&
	inherits(response,"repeated")&&!inherits(envir,"repeated")
if(respenv){
	if(dim(response$response$y)[2]>1)
		stop("elliptic only handles univariate responses")
	if(!is.null(response$NAs)&&any(response$NAs))
		stop("elliptic does not handle data with NAs")}
envname <- if(respenv)deparse(substitute(response))
	else if(!is.null(class(envir)))deparse(substitute(envir))
	else NULL
#
# if envir, remove extra (multivariate) responses
#
type <- "unknown"
if(!respenv&&inherits(envir,"repeated")){
	if(!is.null(envir$NAs)&&any(envir$NAs))
		stop("elliptic does not handle data with NAs")
	cn <- deparse(substitute(response))
	if(length(grep("\"",cn))>0)cn <- response
	if(length(cn)>1)stop("only one response variable allowed")
	response <- envir
	col <- match(cn,colnames(response$response$y))
	if(is.na(col))stop(paste("response variable",cn,"not found"))
	type <- response$response$type[col]
	if(dim(response$response$y)[2]>1){
		response$response$y <- response$response$y[,col,drop=FALSE]
		if(!is.null(response$response$delta)){
			response$response$delta <- response$response$delta[,col,drop=FALSE]
			if(all(response$response$delta==1)||all(is.na(response$response$delta)))response$response$delta <- NULL}}}
#
# if response is not a data object, make one, and prepare covariates
#
if(inherits(response,"repeated")){
	if(dim(response$response$y)[2]>1)
		stop("elliptic only handles univariate responses")
	if(!is.null(response$NAs)&&any(response$NAs))
		stop("elliptic does not handle data with NAs")
	if(is.character(model)){
		resp <- response
		if(is.null(ccov))resp$ccov <- NULL
		else if(inherits(ccov,"formula")){
		# find time-constant covariates and put in data object
			if(any(is.na(match(rownames(attr(terms(ccov,data=response),"factors")),colnames(response$ccov$ccov)))))
				stop("ccov covariate(s) not found")
			tmp <- wr(ccov, data=response, expand=FALSE)$design
			resp$ccov$ccov <- tmp[,-1,drop=FALSE]
			rm(tmp)}
		else stop("ccov must be a W&R formula")
		if(is.null(tvcov))resp$tvcov <- NULL
		else if(inherits(tvcov,"formula")){
		# find time-varying covariates and put in data object
#			if(any(is.na(match(rownames(attr(terms(tvcov,data=response),"factors")),colnames(response$tvcov$tvcov)))))
#				stop("tvcov covariate(s) not found")
			tmp <- wr(tvcov, data=response)$design
			resp$tvcov$tvcov <- tmp[,-1,drop=FALSE]
			rm(tmp)}
		else stop("tvcov must be a W&R formula")}
	else resp <- rmna(response$response)
	type <- resp$response$type
	nccov <- if(is.null(resp$ccov$ccov)) 0 else  dim(resp$ccov$ccov)[2]
	ntvc <- if(is.null(resp$tvcov$tvcov)) 0 else  dim(resp$tvcov$tvcov)[2]}
else {
	if(inherits(response,"response"))resp <-  response
	else {
		respname <- deparse(substitute(response))
		resp <- restovec(response,times,nest=nest,delta=delta)
		colnames(resp$y) <- respname}
	type <- resp$type
	if(dim(resp$y)[2]>1)stop("elliptic only handles univariate responses")
	if(is.null(ccov))nccov <- 0
	else {
	# set up time-constant covariates
		if(!inherits(ccov,"tccov")){
			ccname <- deparse(substitute(ccov))
			if((is.matrix(ccov)&&is.null(colnames(ccov)))){
				ccname <- deparse(substitute(ccov))
				if(dim(ccov)[2]>1){
					tmp <- NULL
					for(i in 1:dim(ccov)[2])tmp <- c(tmp,paste(ccname,i,sep=""))
					ccname <- tmp}}
			ccov <- tcctomat(ccov,names=ccname)}
		nccov <- dim(ccov$ccov)[2]}
	if(is.null(tvcov))ntvc <- 0
	else {
	# set up time-varying covariates
		if(!inherits(tvcov,"tvcov")){
			tvcname <- deparse(substitute(tvcov))
			if(is.list(tvcov)&&dim(tvcov[[1]])[2]>1){
				if(is.null(colnames(tvcov[[1]]))){
					tvcname <- deparse(substitute(tvcov))
					tmp <- NULL
					for(i in 1:dim(tvcov[[1]])[2])tmp <- c(tmp,paste(tvcname,i,sep=""))
					tvcname <- tmp}
				else tvcname <- colnames(tvcov[[1]])}
			tvcov <- tvctomat(tvcov,names=tvcname)}
		ntvc <- dim(tvcov$tvcov)[2]}
	resp <- rmna(response=resp,tvcov=tvcov,ccov=ccov)
	if(!is.null(ccov))rm(ccov)
	if(!is.null(tvcov))rm(tvcov)}
if((inherits(envir,"repeated")&&(length(nobs(resp))!=length(nobs(envir))||
	any(nobs(resp)!=nobs(envir))))||(inherits(envir,"tvcov")&&
	(length(nobs(resp))!=length(envir$tvcov$nobs)||
	any(nobs(resp)!=envir$tvcov$nobs))))
	stop("response and envir objects are incompatible")
if(type!="unknown"&&type!="continuous"&&type!="duration")
	stop("continuous or duration data required")
if(!is.null(resp$response$censor))stop("elliptic does not handle censoring")
full <- !is.null(resp$ccov$ccov)
y <- resp$response$y
n <- length(y)
times <- resp$response$times
nobs <- nobs(resp)
nind <- length(nobs)
nld <- max(nobs)
mu <- varn <- covn <- NULL
npre <- length(pre)
if(is.null(covfn)&&twins)stop("covfn must be specified for twin data")
if(!is.null(covfn)&&npre==0)stop("initial estimates must be provide for covfn")
if(twins&&max(nobs)!=2)stop("covfn can only be used with two observations per individual")
#
# if a data object was supplied, modify formulae or functions to read from it
#
mu3 <- sh3 <- cv3<- NULL
if(respenv||inherits(envir,"repeated")||inherits(envir,"tccov")||inherits(envir,"tvcov")){
	if(inherits(model,"formula")){
		mu3 <- if(respenv)finterp(model,.envir=response,.name=envname)
			else finterp(model,.envir=envir,.name=envname)}
	else if(is.function(model)){
		if(is.null(attr(model,"model"))){
		        tmp <- parse(text=deparse(model)[-1])
		        model <- if(respenv)fnenvir(model,.envir=response,.name=envname)
		        	else fnenvir(model,.envir=envir,.name=envname)
		        mu3 <- model
		        attr(mu3,"model") <- tmp}
		else mu3 <- model}
	if(inherits(varfn,"formula")){
		sh3 <- if(respenv)finterp(varfn,.envir=response,.name=envname)
			else finterp(varfn,.envir=envir,.name=envname)}
	else if(is.function(varfn)){
		if(is.null(attr(varfn,"model"))){
		        tmp <- parse(text=deparse(varfn)[-1])
		        varfn <- if(respenv)fnenvir(varfn,.envir=response,.name=envname)
		        	else fnenvir(varfn,.envir=envir,.name=envname)
		        sh3 <- varfn
		        attr(sh3,"model") <- tmp}
		else sh3 <- varfn}
	if(inherits(covfn,"formula")){
		cv3 <- if(respenv)finterp(covfn,.envir=response,.name=envname)
			else finterp(covfn,.envir=envir,.name=envname)}
	else if(is.function(covfn)){
		if(is.null(attr(covfn,"model"))){
		        tmp <- parse(text=deparse(covfn)[-1])
		        covfn <- if(respenv)fnenvir(covfn,.envir=response,.name=envname)
		        	else fnenvir(covfn,.envir=envir,.name=envname)
		        cv3 <- covfn
		        attr(cv3,"model") <- tmp}
		else cv3 <- covfn}}
npr <- length(preg)
#
# transform model formula to function and check number of parameters
#
if(inherits(model,"formula")){
	mu2 <- if(respenv)finterp(model,.envir=response,.name=envname)
		else finterp(model,.envir=envir,.name=envname)
	npt1 <- length(attr(mu2,"parameters"))
	if(is.character(attr(mu2,"model"))){
	# W&R formula
		if(length(attr(mu2,"model"))==1){
			mu1 <- function(p) p[1]*rep(1,n)
			attributes(mu1) <- attributes(mu2)
			mu2 <- NULL}}
	else {
	# formula with unknowns
		if(npr!=npt1&&!common){
			cat("\nParameters are ")
			cat(attr(mu2,"parameters"),"\n")
			stop(paste("preg should have",npt1,"estimates"))}
		if(is.list(preg)){
			if(!is.null(names(preg))){
				o <- match(attr(mu2,"parameters"),names(preg))
				preg <- unlist(preg)[o]
				if(sum(!is.na(o))!=length(preg))stop("invalid estimates for model - probably wrong names")}
			else preg <- unlist(preg)}}
	if(!is.null(mu2)){
		if(inherits(envir,"tccov")){
		# fix length if only time-constant covariates
			cv <- covind(response)
			mu1 <- function(p) mu2(p)[cv]
			attributes(mu1) <- attributes(mu2)}
		else {
			mu1 <- mu2
			rm(mu2)}}}
else if(is.function(model))mu1 <- model
else mu1 <- NULL
#
# give appropriate attributes to mu1 for printing
#
if(!is.null(mu1)&&is.null(attr(mu1,"parameters"))){
	attributes(mu1) <- if(is.function(model)){
		if(!inherits(model,"formulafn")){
			if(respenv)attributes(fnenvir(model,.envir=response))
			else attributes(fnenvir(model,.envir=envir))}
		else attributes(model)}
		else {
			if(respenv)attributes(fnenvir(mu1,.envir=response))
			else attributes(fnenvir(mu1,.envir=envir))}}
#
# if possible, check that correct number of estimates was supplied
#
nlp <- if(is.function(mu1))length(attr(mu1,"parameters"))
	else if(is.null(mu1))NULL
	else npt1
if(!is.null(nlp)&&!common&&nlp!=npr)
	stop(paste("preg should have",nlp,"initial estimates"))
if(is.function(mu1))mdl <- 2
else if(model=="linear"){
	if(dst==4)stop("linear model option not allowed with Laplace distribution")
	mdl <- 1
	torder <- torder+1}
else if(model=="logistic")mdl <- 3
else if(model=="pkpd")mdl <- 4
if(mdl==2&&length(mu1(preg))!=sum(nobs(resp)))
	stop("The mean function must provide an estimate for each observation")
npvar <- length(pvar)
n1 <- if(common){if(inherits(varfn,"formula"))
	length(attr(mu1,"parameters"))+1 else 1} else length(preg)+1
n2 <- length(c(preg,pvar))
n3 <- n2+1
n4 <- length(c(preg,pvar,pre))
#
# transform variance formula to function and check number of parameters
#
if(inherits(varfn,"formula")){
	old <- if(common)mu1 else NULL
	mufn <- if(shfn)"mu" else NULL
	sh3 <- if(respenv)finterp(varfn,.envir=response,.start=n1,.name=envname,.old=old,.args=mufn)
		else finterp(varfn,.envir=envir,.start=n1,.name=envname,.old=old,.args=mufn)
	tmp <- attributes(sh3)
	sh2 <- if(shfn)function(p) sh3(p,mu1(p)) else sh3
	attributes(sh2) <- tmp
	npt2 <- length(attr(sh2,"parameters"))
	if(is.character(attr(sh2,"model"))){
	# W&R formula
		if(length(attr(sh2,"model"))==1){
			sh1 <- function(p) p[n1]*rep(1,n)
			attributes(sh1) <- attributes(sh2)
			sh2 <- NULL}}
	else {
	# formula with unknowns
		if(npvar!=npt2&&!common){
			cat("\nParameters are ")
			cat(attr(sh2,"parameters"),"\n")
			stop(paste("pvar should have",npt2,"estimates"))}
		if(is.list(pvar)){
			if(!is.null(names(pvar))){
				o <- match(attr(sh2,"parameters"),names(pvar))
				pvar <- unlist(pvar)[o]
				if(sum(!is.na(o))!=length(pvar))stop("invalid estimates for varfn - probably wrong names")}
			else pvar <- unlist(pvar)}}
	if(!is.null(sh2)){
		if(inherits(envir,"tccov")){
			cv <- covind(response)
			sh1 <- function(p) sh2(p)[cv]
			attributes(sh1) <- attributes(sh2)}
		else {
			sh1 <- sh2
			rm(sh2)}}}
else if(is.function(varfn))sh1 <- if(shfn) function(p) varfn(p[n1:n2],mu1(p))
		else function(p) varfn(p[n1:n2])
else sh1 <- NULL
#
# give appropriate attributes to sh1 for printing
#
if(!is.null(sh1)&&is.null(attr(sh1,"parameters")))
	attributes(sh1) <- if(is.function(varfn)){
		if(!inherits(varfn,"formulafn")){
			if(respenv)attributes(fnenvir(varfn,.envir=response))
			else attributes(fnenvir(varfn,.envir=envir))}
		else attributes(varfn)}
		else {
			if(respenv)attributes(fnenvir(sh1,.envir=response))
			else attributes(fnenvir(sh1,.envir=envir))}
nlp <- if(is.function(varfn))length(attr(sh1,"parameters"))-shfn
	else if(is.null(varfn)||is.character(varfn))NULL
	else if(!inherits(varfn,"formula"))stop("varfn must be a function or formula")
	else npt2
#
# if possible, check that correct number of estimates was supplied
#
if(!is.null(nlp)&&!common&&nlp!=npvar)
	stop(paste("pvar should have",nlp,"initial estimates"))
if(common){
	nlp <- length(unique(c(attr(mu1,"parameters"),attr(sh1,"parameters"))))
	if(nlp!=npr)stop(paste("with a common parameter model, preg should contain",nlp,"estimates"))}
#
# transform covariance formula to function and check number of parameters
#
if(inherits(covfn,"formula")){
	cv3 <- if(respenv)finterp(covfn,.envir=response,.start=n3,.name=envname)
		else finterp(covfn,.envir=envir,.start=n3,.name=envname)
	tmp <- attributes(cv3)
	cv2 <- cv3
	attributes(cv2) <- tmp
	npt3 <- length(attr(cv2,"parameters"))
	if(is.character(attr(cv2,"model"))){
	# W&R formula
		if(length(attr(cv2,"model"))==1){
			cv1 <- function(p) p[n3]*rep(1,n)
			attributes(cv1) <- attributes(cv2)
			cv2 <- NULL}}
	else {
	# formula with unknowns
		if(npre!=npt3){
			cat("\nParameters are ")
			cat(attr(cv2,"parameters"),"\n")
			stop(paste("pre should have",npt3,"estimates"))}
		if(is.list(pre)){
			if(!is.null(names(pre))){
				o <- match(attr(cv2,"parameters"),names(pre))
				pre <- unlist(pre)[o]
				if(sum(!is.na(o))!=length(pre))stop("invalid estimates for covfn - probably wrong names")}
			else pre <- unlist(pre)}}
	if(!is.null(cv2)){
		if(inherits(envir,"tccov")){
			cv <- covind(response)
			cv1 <- function(p) cv2(p)[cv]
			attributes(cv1) <- attributes(cv2)}
		else {
			cv1 <- cv2
			rm(cv2)}}}
else if(is.function(covfn))cv1 <- function(p) covfn(p[n3:n4])
else cv1 <- NULL
#
# give appropriate attributes to cv1 for printing
#
if(!is.null(cv1)&&is.null(attr(cv1,"parameters")))
	attributes(cv1) <- if(is.function(covfn)){
		if(!inherits(covfn,"formulafn")){
			if(respenv)attributes(fnenvir(covfn,.envir=response))
			else attributes(fnenvir(covfn,.envir=envir))}
		else attributes(covfn)}
		else {
			if(respenv)attributes(fnenvir(cv1,.envir=response))
			else attributes(fnenvir(cv1,.envir=envir))}
nlp3 <- if(is.function(covfn))length(attr(cv1,"parameters"))
	else if(is.null(covfn)||is.character(covfn))NULL
	else if(!inherits(covfn,"formula"))stop("covfn must be a function or formula")
	else npt3
#
# if possible, check that correct number of estimates was supplied
#
if(!is.null(nlp3)&&nlp3!=npre)
	stop(paste("pre should have",nlp3,"initial estimates"))
#
# check interactions of covariates with times
#
if(mdl==1&&!is.null(interaction)){
	if(length(interaction)!=nccov)
		stop(paste(nccov,"interactions with time must be specified"))
	else if(any(interaction>torder-1))
		stop(paste("Interactions can be at most of order ",torder-1))
	else interaction <- interaction+1}
else interaction <- rep(1,nccov)
if(mdl==4&&ntvc==0&&is.null(dose))stop("Doses required for PKPD model")
#
# set up nesting indicator
#
if(!is.null(resp$response$nest))lnest <- max(resp$response$nest)
else {
	lnest <- 0
	resp$response$nest <- rep(1,length(y))}
#
# check times
#
if(!is.null(times)){
	if(mdl==1){
		if(torder>length(unique(times)))
			stop("torder is too large for the number of distinct times")
		ave <- mean(times)
		times <- times-ave}
	else ave <- 0}
else {
	if(!is.null(par))stop("No times. AR cannot be fitted")
	if(torder>1)stop("No times. Time trends cannot be fitted.")
	ave <- 0}
#
# set up for full logistic model
#
if(full){
	rxl <- NULL
	for(i in 1:nind){
		tmp <- 1
		for(j in 1:dim(resp$ccov$ccov)[2])
			tmp <- tmp+resp$ccov$ccov[i,j]*2^(j-1)
		rxl <- c(rxl,tmp)}
	nxrl <- length(unique(rxl)) #max(rxl)
	p <- preg}
else {
	nxrl <- 1
	rxl <- matrix(1,ncol=1,nrow=nind)
	p <- NULL}
nm <- sum(nobs(resp))
#
# calculate transformation and Jacobian
#
if(transform=="identity")jacob <- 0
else if(transform=="exp"){
	jacob <- -sum(y)
	y <- exp(y)}
else if(any(y==0))stop("Zero response values: invalid transformation")
else if(transform=="square"){
	jacob <- -sum(log(abs(y)))-length(resp$response$y)*log(2)
	y  <- y^2}
else if(any(y<0))stop("Nonpositive response values: invalid transformation")
else if(transform=="sqrt"){
	jacob <- sum(log(y))/2+length(resp$response$y)*log(2)
	y <- sqrt(y)}
else if(transform=="log"){
	jacob <- sum(log(y))
	y <- log(y)}
#
# include delta in Jacobian
#
if(!is.null(resp$response$delta)){
	if(length(resp$response$delta)==1)jacob <- jacob-nm*log(resp$response$delta)
	else jacob <- jacob -sum(log(resp$response$delta))}
#
# special cases for various models
#
if(mdl==1){
# linear model
	if(lnk==1&&is.null(varfn)&&is.null(covfn)){
		npvl <- torder+sum(interaction)+ntvc
		npv <- 0}
	else {
		npv <- torder+sum(interaction)+ntvc
		npvl <- 0
		if(is.null(preg)){
			if(lnk!=1)stop(paste("Initial regression estimates must be supplied for the linear model with ",link,"link"))
			else stop("Initial regression estimates must be supplied for the linear model when there is a variance or covariance function")}}}
else {
	npvl <- 0
	if(mdl==2)npv <- length(preg)
	else if(mdl==3)npv <- if(ntvc==0)4*nxrl else 4+nxrl-1
	else if(mdl==4){
		if(!is.function(varfn))npv <- 2+nxrl
		else npv <- 3}}
npv2 <- max(npv,npvl)
if(length(preg)!=npv)
	stop(paste(npv,"initial parameter estimates for the mean regression must be supplied"))
#
# set up variance function if there is one and check initial estimates
#
if(is.null(varfn))cvar <- 0
else if(is.function(sh1)){
	cvar <- 1
	if(length(sh1(if(common)preg else pvar))!=sum(nobs(resp)))
		stop("The variance function or formula must provide an estimate for each observation")}
else if(varfn=="identity"){
	if(any(y<0))warning("identity variance function not recommended with negative responses")
	cvar <- 2}
else if(varfn=="square")cvar <- 3
else stop("Unknown variance function: choices are identity and square")
if(!(mdl==4&&npvar==4)&&cvar==0){
	if(any(pvar<=0))stop("All variance parameters must be positive")
	pvar <- log(pvar)}
theta <- c(preg,pvar)
#
# set up covariance function if there is one and check initial estimates
#
if(is.function(cv1)){
	if(length(cv1(pre))!=sum(nobs(resp)))
		stop("The covariance function or formula must provide an estimate for each observation")}
#
# check AR and random effect
#
if(npre>0){
	if(is.null(covfn)){
		if(any(pre<=0))stop("All variance components must be positive")
		theta <- c(theta,log(pre))}
	else theta <- c(theta,pre)}
if(npar>0){
	if(par<=0||(par>=1&&ar!=5))
		stop("Estimate of autocorrelation must lie between 0 and 1")
	theta <- if(ar!=5)c(theta,log(par/(1-par)))
		else c(theta,log(par))}
if(npell)theta <- c(theta,if(dst==4)pell else log(pell))
np <- npv+npvl*(mdl==1)+npvar+npre+npar+npell
#
# estimate model
#
if(fscale==1)fscale <- plral(theta)
z0 <- nlm(plral, p=theta, hessian=TRUE, print.level=print.level,
	typsize=typsize, ndigit=ndigit, gradtol=gradtol, stepmax=stepmax,
	steptol=steptol, iterlim=iterlim, fscale=fscale)
p <- z0$estimate
z <- plra(p)
like <- z$like+nm*log(pi)/2+jacob
pred <- y-z$res
#
# get parameters
#
sigsq <- p[(npv+1):(npv+npvar)]
if(!(mdl==4&&npvar==4)&&!common)sigsq <- exp(sigsq)
if(npre>0&&is.null(covfn))tausq <- exp(p[(npv+npvar+1):(npv+npvar+npre)])
else tausq <- 0
if(npar>0){
	rho <- exp(p[npv+npvar+npre+1])
	if(ar!=5)rho <- rho/(1+rho)}
else rho <- 0
#
# calculate se's
#
if(np-npvl==1){
	nlcov <- 1/z0$hessian
	nlse <- sqrt(nlcov)}
else {
	a <- if(any(is.na(z0$hessian))||any(abs(z0$hessian)==Inf))0
		else qr(z0$hessian)$rank
	if(a==np-npvl)nlcov <- solve(z0$hessian)
	else nlcov <- matrix(NA,ncol=np-npvl,nrow=np-npvl)
	nlse <- sqrt(diag(nlcov))}
if(length(nlse)>1)nlcorr <- nlcov/(nlse%o%nlse)
else nlcorr <- as.matrix(nlcov/nlse^2)
dimnames(nlcorr) <- list(seq(1,np-npvl),seq(1,np-npvl))
betase <- betacorr <- NULL
if(mdl==1){
	if(lnk==1&&cvar==0){
		betacov <- matrix(z$betacov,ncol=npvl)
		bt <- exp(p[length(p)])
		if(npell>0)betacov <- betacov/(npvl*gamma(npvl/(2*bt)))*
			2^(1/bt)*gamma((npvl+2)/(2*bt))
		if(npvl==1)betase <- sqrt(betacov)
		else if(npvl>1)betase <- sqrt(diag(betacov))
		if(npvl>1){
			betacorr <- betacov/(betase%o%betase)
			dimnames(betacorr) <- list(seq(1,npvl),seq(1,npvl))}}
	else {
		betase <- nlse[1:npv]
		betacov <- nlcov[1:npv,1:npv]
		if(npvl>1){
			betacorr <- nlcorr[1:npv,1:npv]
			dimnames(betacorr) <- list(seq(1,npv),seq(1,npv))}}}
else betacov <- NULL
#
# if possible, calculate recursive fitted values
#
if(dst==1&&!common&&length(sigsq)==1&&((npar>0&&
	autocorr=="exponential")||(npar==0&&length(pre)==1))){
	nt <- sum(nobs(resp))
	mse <- sigsq*length(y)/(length(y)-npv-npvl*(mdl==1))
	coef <- NULL
	if(npar>0)coef <- c(coef,log(-log(rho)))
	if(length(pre)==1)coef <-
	c(coef,sqrt(exp(p[npv+npvar+1])/mse))
	z1 <- .Fortran("resid2",
		np=as.integer(npar+length(pre)),
		par=as.double(coef),
		ave=as.double(ave),
		pred=as.double(pred),
		rpred=double(nt),
		sdr=double(nt),
		res=double(nt),
		y=as.double(y),
		mse=as.double(mse),
		ns=as.integer(nind),
		nt=as.integer(nt),
		model=as.integer(npar),
		t=as.double(times),
		nobs=as.integer(nobs(resp)),
		nod=as.integer(length(pre)),
		p=double(length(p)),
		#DUP=FALSE,
		PACKAGE="growth")
	rpred <- z1$rpred
	ii <- covind(resp$response)
	kk <- unique(resp$response$nest)
	if(lnest)for(i in 1:nind)for(k in kk)
		z1$rpred[i==ii&k==resp$response$nest][1] <- z1$pred[i==ii&k==resp$response$nest][1]
	sdr <- z1$sdr
	rres <- z1$res}
else rpred <- sdr <- rres <- NULL
if(!is.null(mu3))mu1 <- mu3
if(!is.null(sh3))sh1 <- sh3
if(!is.null(cv3))cv1 <- cv3
z <- list(
	call=call,
	model=model,
	distribution=distribution,
	mu1=mu1,
	varfn=varfn,
	covfn=covfn,
	common=common,
	sh1=sh1,
	cv1=cv1,
	shfn=shfn,
	autocorr=autocorr,
	response=resp$response,
	transform=transform,
	torder=torder-1,
	interaction=interaction-1,
	ccov=resp$ccov,
	tvcov=resp$tvcov,
	full=full,
	link=link,
	maxlike=like,
	aic=like+np,
	df=nm-np,
	np=np,
	npell=npell,
	npv=npv,
	npvl=npvl,
	npvar=npvar,
	npar=npar,
	npre=npre,
	coefficients=p,
	beta=z$beta,
	betacov=betacov,
	betacorr=betacorr,
	betase=betase,
	nlse=nlse,
	nlcov=nlcov,
	nlcorr=nlcorr,
	sigsq=sigsq,
	tausq=tausq,
	rho=rho,
	residuals=z$res,
	pred=pred,
	rpred=rpred,
	sdrpred=sdr,
	rresiduals=rres,
	grad=z0$gradient,
	iterations=z0$iterations,
	code=z0$code)
class(z) <- "elliptic"
if(!is.null(rpred))class(z) <- c(class(z),"recursive")
return(z)}

### standard methods
#' @describeIn elliptic Deviance method
#' @export 
deviance.elliptic <- function(object, ...) 2*object$maxlike
#' @describeIn elliptic Fitted method
#' @export 
fitted.elliptic <- function(object, recursive=FALSE, ...){
if(recursive){
	if(is.null(object$rpred))stop("recursive fitted values not available")
	object$rpred}
else object$pred}
#' @describeIn elliptic Residuals method
#' @export 
residuals.elliptic <- function(object, recursive=FALSE, ...){
if(recursive){
	if(is.null(object$rresiduals))stop("recursive residuals not available")
	return(object$rresiduals)}
else {
	if(object$transform=="exp")object$response$y <- exp(object$response$y)
	else if(object$transform=="square")object$response$y  <- object$response$y^2
	else if(object$transform=="sqrt")object$response$y <- sqrt(object$response$y)
	else if(object$transform=="log")object$response$y <- log(object$response$y)
	return(object$response$y-object$pred)}}

### print method
#' @describeIn elliptic Print method
#' @export 
print.elliptic <- function(x,digits=max(3,.Options$digits-3),correlation=TRUE, ...){
  z <- x; # S3 consistency
if(!is.null(z$ccov$ccov))nccov <- dim(z$ccov$ccov)[2]
else nccov <- 0
cat("\nMultivariate",z$distribution,"distribution\n")
cat("\nCall:",deparse(z$call),sep="\n")
cat("\n")
if(z$code>2)cat("Warning: no convergence - error",z$code,"\n\n")
cat("Response              ",colnames(z$response$y),"\n")
cat("Number of subjects    ",length(nobs(z)),"\n")
cat("Number of observations",length(z$response$y),"\n")
if(!inherits(z$mu1,"formulafn")){
	if(z$model=="linear"){
		if(z$torder>0){
			cat("\nPolynomial model\n")
			cat("Times centred at  ",mean(z$response$times),"\n\n")}
		else cat("\nLinear model\n\n")}
	else if(z$model=="logistic")cat("\nGeneralized logistic model\n\n")
	else if(z$model=="pkpd")cat("\nPKPD model\n\n")}
cat("Transformation:",z$trans,"\n")
cat("Link function: ",z$link,"\n\n")
cat("-Log likelihood   ",z$maxlike,"\n")
cat("Degrees of freedom",z$df,"\n")
cat("AIC               ",z$aic,"\n")
cat("Iterations        ",z$iterations,"\n\n")
ntvc <- !is.null(z$tvcov)
if(!inherits(z$mu1,"formulafn")){
	cat("Location parameters\n")
	if(inherits(z$ccov$linear,"formula"))
		cat("Linear part: ",deparse(z$ccov$linear),sep="\n")}
if(inherits(z$mu1,"formulafn")){
	if(z$common)cat("Location function\n")
	else cat("Location function parameters\n")
	if(!is.null(attr(z$mu1,"formula")))
		cat(deparse(attr(z$mu1,"formula")),sep="\n")
	else if(!is.null(attr(z$mu1,"model"))){
		t <- deparse(attr(z$mu1,"model"))
		t[1] <- sub("expression\\(","",t[1])
		t[length(t)] <- sub("\\)$","",t[length(t)])
		cat(t,sep="\n")}
	cname <- if(is.character(attr(z$mu1,"model")))attr(z$mu1,"model")
		else attr(z$mu1,"parameters")
	coef.table <- cbind(z$coef[1:z$npv],z$nlse[1:z$npv])
	if(!z$common){
		dimnames(coef.table) <- list(cname,c("estimate","se"))
		print.default(coef.table,digits=digits,print.gap=2)}}
else if(z$model=="linear"){
	if(z$npell==1&&z$link=="identity"&&!is.function(z$varfn))
		cat("(Approximate s.e.)\n")
	tord <- z$torder+1+nccov
	if(ntvc)tord <- tord+1
	coef.table <- cbind(z$beta,z$betase)
	cname <- "(Intercept)"
	if(z$torder>0)cname <- c(cname,paste("t^",1:z$torder,sep=""))
	if(nccov>0)for(i in 1:nccov){
		cname <- c(cname,colnames(z$ccov$ccov)[i])
		if(z$interaction[i]>0){
			cname <- c(cname,paste(colnames(z$ccov$ccov)[i],".t^",1:z$interaction[i],sep=""))}}
	if(ntvc)cname <- c(cname,colnames(z$tvcov$tvcov))
	dimnames(coef.table) <- list(cname,c("estimate","se"))
	print.default(coef.table,digits=digits,print.gap=2)
	if(z$npvl>1&&z$link=="identity"&&is.null(z$varfn)&&correlation){
		cat("\nCorrelation matrix of linear parameters\n")
		print.default(z$betacorr,digits=digits)}}
else if(z$model=="logistic"){
	coef.table <- cbind(z$coef[1:4],z$nlse[1:4])
	if(ntvc)cname  <- c("kappa1","kappa3","kappa4","beta")
	else cname <- c("kappa1","kappa2","kappa3","kappa4")
	dimnames(coef.table) <- list(cname,c("estimate","se"))
	print.default(coef.table,digits=digits,print.gap=2)
	if(z$full){
		if(ntvc){
			coef.table <- cbind(z$coef[5:z$npv],z$nlse[5:z$npv])
			cname <- colnames(z$ccov$ccov)
			dimnames(coef.table) <- list(cname,c("estimate","se"))
			print.default(coef.table,digits=digits,print.gap=2)}
		else {
			for(i in 1:nccov){
				cat("   ",colnames(z$ccov$ccov)[i],"\n")
				coef.table <- cbind(z$coef[(i*4+1):((i+1)*4)],z$nlse[(i*4+1):((i+1)*4)])
				dimnames(coef.table) <- list(cname,c("estimate", "se"))
				print.default(coef.table,digits=digits,print.gap=2)}}}}
else if(z$model=="pkpd"){
	coef.table <- cbind(z$coef[1:3], z$nlse[1:3])
	cname <- c("log k_a","log k_e","log V")
	dimnames(coef.table) <- list(cname,c("estimate","se"))
	print.default(coef.table,digits=digits,print.gap=2)
	if(z$full){
		for(i in 1:nccov){
			cat("   ",colnames(z$ccov$ccov)[i],"\n")
			coef.table <- cbind(z$coef[i+3],z$nlse[i+3])
			dimnames(coef.table) <- list(cname[3],c("estimate","se"))
			print.default(coef.table,digits=digits,print.gap=2)}}}
if(inherits(z$sh1,"formulafn")){
	if(z$common){
		if(z$distribution=="normal")cat("\nVariance function\n")
		else cat("\nDispersion function\n")}
	else {
		if(z$distribution=="normal")
			cat("\nVariance function parameters\n")
		else cat("\nDispersion function parameters\n")
		cname <- NULL}
	if(!is.null(attr(z$sh1,"formula")))
		cat(deparse(attr(z$sh1,"formula")),sep="\n")
	else if(!is.null(attr(z$sh1,"model"))){
		t <- deparse(attr(z$sh1,"model"))
		t[1] <- sub("expression\\(","",t[1])
		t[length(t)] <- sub("\\)$","",t[length(t)])
		cat(t,sep="\n")}
	cname <- c(cname,if(is.character(attr(z$sh1,"model")))
		attr(z$sh1,"model")
		else if(is.expression(attr(z$sh1,"model")))
		attr(z$sh1,"parameters")
		else attr(z$sh1,"parameters")[1:z$npvar])
	if(z$shfn)cname <- cname[cname!="mu"]
	if(!z$common)coef.table <- cbind(z$coef[(z$npv+1):(z$npv+z$npvar)], z$nlse[(z$npv+1):(z$npv+z$npvar)])
	else cat("\nCommon parameters\n")
	dimnames(coef.table) <- list(unique(cname),c("estimate","se"))
	print.default(coef.table,digits=digits,print.gap=2)}
else if(!is.function(z$model)&&!inherits(z$mu1,"formulafn")&&z$model=="pkpd"
	&&z$npvar==4){
	if(z$distribution=="normal")cat("\nVariance parameters\n")
	else cat("\nDispersion parameters\n")
	coef.table <- cbind(z$sigsq, z$nlse[(z$npv+1):(z$npv+4)])
	cname <- c("log k_a","log k_e","log V","power")
	dimnames(coef.table) <- list(cname,c("estimate","se"))
	print.default(coef.table,digits=digits,print.gap=2)}
else {
	if(z$distribution=="normal"){
		if(z$npvar==1)cat("\nVariance\n")
		else cat("\nVariance parameters\n")}
	else {
		if(z$npvar==1)cat("\nDispersion\n")
		else cat("\nDispersion parameters\n")}
	if(!is.null(z$varfn)){
		cat(z$varfn,"function of the location parameter\n")
		vname <- "factor"
		if(z$npvar==2)vname <- c("constant",vname)}
	else if(z$npvar>1)
		vname <- c("(Intercept)",paste("t^",1:(z$npvar-1),sep=""))
	else vname <- ""
	coef.table <- cbind(z$coef[(z$npv+1):(z$npv+z$npvar)],
		z$nlse[(z$npv+1):(z$npv+z$npvar)],z$sigsq)
	dimnames(coef.table) <- list(vname,c("estimate","se","sigsq"))
	print.default(coef.table,digits=digits,print.gap=2)}
if(z$npre>0){
	if(is.null(z$covfn)){
		cat("\nVariance components\n")
		coef.table <- cbind(z$coef[(z$npv+z$npvar+1):(z$npv+z$npvar+z$npre)],
			z$nlse[(z$npv+z$npvar+1):(z$npv+z$npvar+z$npre)],z$tausq)
		if(z$npre==1)cname <- "tausq"
		else cname <- c("Level 1","Level 2")
		dimnames(coef.table) <- list(cname,c("estimate","se",""))
		print.default(coef.table,digits=digits,print.gap=2)}
	else if(inherits(z$cv1,"formulafn")){
		cat("\nCovariance function parameters\n")
		cname <- NULL
		if(!is.null(attr(z$cv1,"formula")))
			cat(deparse(attr(z$cv1,"formula")),sep="\n")
		else if(!is.null(attr(z$cv1,"model"))){
			t <- deparse(attr(z$cv1,"model"))
			t[1] <- sub("expression\\(","",t[1])
			t[length(t)] <- sub("\\)$","",t[length(t)])
			cat(t,sep="\n")}
		cname <- c(cname,if(is.character(attr(z$cv1,"model")))
				attr(z$cv1,"model")
			else if(is.expression(attr(z$cv1,"model")))
				attr(z$cv1,"parameters")
			else attr(z$cv1,"parameters")[1:z$npre])
		coef.table <- cbind(z$coef[(z$npv+z$npvar+1):z$np], z$nlse[(z$npv+z$npvar+1):z$np])
		dimnames(coef.table) <- list(unique(cname),c("estimate","se"))
		print.default(coef.table,digits=digits,print.gap=2)}}
if(z$rho!=0){
	cat("\n",z$autocorr," autocorrelation\n",sep="")
	coef.table <- cbind(z$coef[z$npv+z$npvar+z$npre+1],
		z$nlse[z$npv+z$npvar+z$npre+1],z$rho)
	dimnames(coef.table) <- list("rho",c("estimate","se",""))
	print.default(coef.table,digits=digits,print.gap=2)}
if(z$npell==1){
	coef.table <- cbind(z$coef[z$np-z$npvl],z$nlse[z$np-z$npvl],
		if(z$distribution=="Laplace")NULL
			else exp(z$coef[z$np-z$npvl]))
	if(z$distribution=="power exponential"){
		cat("\nPower exponential parameter\n")
		dimnames(coef.table) <- list("",c("estimate","se","power"))}
	else if(z$distribution=="Student t"){
		cat("\nDegrees of freedom parameter\n")
		dimnames(coef.table) <- list("",c("estimate","se","d.f."))}
	else if(z$distribution=="Laplace"){
		cat("\nAsymmetry parameter\n")
		dimnames(coef.table) <- list("",c("estimate","se"))}
	print.default(coef.table,digits=digits,print.gap=2)}
if(z$np-z$npvl>1&&correlation){
	cat("\nCorrelation matrix of nonlinear parameters\n")
	print.default(z$nlcorr,digits=digits)}}
