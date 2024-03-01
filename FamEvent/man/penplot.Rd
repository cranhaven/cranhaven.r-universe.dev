\name{penplot}
\alias{penplot}
\title{
Plot penetrance functions
}
\description{
Plots the penetrance functions given the values of baseline parameters and regression coefficients and choices of baseline and frailty distributions.
}
\usage{
penplot(base.parms, vbeta, cuts = NULL, variation = "none", base.dist = "Weibull", 
frailty.dist = NULL, depend = 1, agemin = 20, agemax = 80, print = TRUE, 
col = c("blue","red","blue","red"),  lty = c(1, 1, 2, 2), add.legend = TRUE, 
add.title = TRUE, x = "topleft", y = NULL, xlab = "Age at onset", ylab = "Penetrance", 
ylim = NULL, main = NULL, ...)
}
\arguments{
  \item{base.parms}{
Vector of parameter values for the specified baseline hazard function: \code{base.parms = c(lambda, rho)} should be specified for \code{base.dist = "Weibull"}, \code{"loglogistic"}, \code{"Gompertz"}, \code{"gamma"}, and \code{"lognormal"}, \code{c(lambda, rho, eta)} for \code{base.dist = "logBurr"}, or interval constant hazard values for the intervals produced by \code{cuts} for \code{base.dist = "piecewise"}.
}
 \item{vbeta}{
 Vector of regression coefficients for gender and majorgene, \code{vbeta = c(beta.s, beta.g)}. If \code{variation = "secondgene"}, regression coefficients for gender, major gene and second gene, \code{vbeta = c(beta.s, beta.g1, beta.g2)}, should be specified.
}
\item{cuts}{
Vector of cut points defining the intervals where the hazard function is constant. The \code{cuts} should be specified when \code{base.dist = "piecewise"} and must be strictly positive and finite. Default is \code{NULL}.}
\item{variation}{
Source of residual familial correlation.  Possible choices are: \code{"frailty"} for frailty shared within families, \code{"secondgene"} for second gene variation, or \code{"none"} for no residual familial correlation. Default is \code{"none"}.
}
\item{base.dist}{
Choice of baseline hazard distribution. Possible choices are: \code{"Weibull"}, \code{"loglogistic"}, \code{"Gompertz"}, \code{"lognormal"},  \code{"gamma"}, or \code{"piecewise"}. Default is \code{"Weibull"}.
}
 \item{frailty.dist}{
Choice of frailty distribution. Possible choices are \code{"gamma"} for gamma distribution or \code{"lognormal"} for log normal distributions when \code{variation = "frailty"}. Default is \code{NULL}.
}
\item{depend}{
Variance of the frailty distribution. Dependence within families increases with \code{depend} value.  Default value is 1. 
}
\item{agemin}{
	Minimum age of disease onset. Default is 20 years of age.
}
\item{agemax}{
	Maximum age of disease onset. Default is 80 years of age.
}
\item{print}{
	Logical; if \code{TRUE}, prints the penetrance values by age 70 obtained from the assumed model for male carriers, female carriers, male noncarrers, and female noncarriers.	 Default is \code{TRUE}.
}
\item{col}{
Colors of lines for male carriers, female carriers, male noncarrers, and female noncarriers. Default is \code{c("blue", "red", "blue", "red")}.
}
\item{lty}{
Types of lines for male carriers, female carriers, male noncarriers, and female noncarriers. Default is \code{c(1, 1, 2, 2)}.
}
\item{add.legend}{Logical; if \code{TRUE}, displays legend in the plot. Default is \code{TRUE}.}
\item{add.title}{Logical; if \code{TRUE}, displays title in the plot. Default is \code{TRUE}.}
\item{x, y}{Position of legend; see \link{legend}. Defaults are \code{x = "topleft", y = NULL}.}
\item{xlab}{Title for the x-axis. Default is \code{"Age at onset"}.}
\item{ylab}{Title for the y-axis. Default is \code{"Penetrance"}.}
\item{ylim}{Limits of the y-axis. Default is \code{NULL}. If \code{NULL}, \code{ylim} will be automatically determined. }
\item{main}{Main title of the plot. Default is \code{NULL}. If \code{NULL}, the title will be automatically created.}
\item{...}{Other parameters to be passed through to plotting functions.}
}
\details{
%At the moment the function will only generate penetrance curves for the gamma frailty model and the second gene model. The penetrance curves for the log-normal frailty will be implemented at a later date.
\emph{Proportional hazard models}

The penetrance model conditional on the covariates \ifelse{html}{ \out{ X = c(x<sub>s</sub>, x<sub>g</sub>)}}{\eqn{X = (x_s, x_g)}{X = c(xs, xg)}} is assumed to have the following hazard function:
\ifelse{html}{ \out{h(t|X) = h<sub>0</sub>(t - t<sub>0</sub>) exp(&beta;<sub>s</sub> * x<sub>s</sub> + &beta;<sub>g</sub> * x<sub>g</sub>),}}{\deqn{ h(t|X) = h_0(t - t_0) \exp(\beta_s x_s+\beta_g x_g),}{ h(g|X) = h0(t - t0) * exp(beta.s * xs + beta.g * xg) ,}}
where \ifelse{html}{\out{h<sub>0</sub>(t)}}{\eqn{h_0(t)}{h0(t)}} is the baseline hazard function, \ifelse{html}{\out{t<sub>0</sub>}}{\eqn{t_0}{t0}} is a minimum age of disease onset, \ifelse{html}{\out{x<sub>x</sub>}}{\eqn{x_s}{xs}} and \ifelse{html}{\out{x<sub>g</sub>}}{\eqn{x_g}{xg}} indicate male (1) or female (0) and carrier (1) or non-carrier (0) of a main gene of interest, respectively.

The penetrance function for the penetrance model has the form,
\ifelse{html}{\out{1 - exp(- H<sup>0</sup>(t - t<sub>0</sub>) * exp(&beta;<sub>s</sub> * x<sub>s</sub> + &beta;<sub>g</sub> * x<sub>g</sub> )), }}{\deqn{1-\exp\left\{-H_0(t-t_0) \exp (\beta_s x_s+\beta_g x_g) \right\}, }{ 1 - exp(-H0(t - t0) * exp(beta.s * xs + beta.g * xg)), }}
where \ifelse{html}{\out{H<sub>0</sub>(t)}}{\eqn{H_0(t)}{H0(t)}} is the cumulative baseline hazard function.

\emph{Shared frailty models}

The penetrance model conditional on the frailty \eqn{Z} and covariates 
\ifelse{html}{ \out{ X = c(x<sub>s</sub>, x<sub>g</sub>)}}{\eqn{X=(x_s, x_g)}{X = c(xs, xg)}} is assumed to have the following hazard function:
\ifelse{html}{ \out{ h(t|X,Z) = h<sub>0</sub>(t - t<sub>0</sub>) Z exp(&beta;<sub>s</sub> * x<sub>s</sub> + &beta;<sub>g</sub> * x<sub>g</sub>),}}{	
		\deqn{ h(t|X,Z) = h_0(t - t_0) Z \exp(\beta_s x_s+\beta_g x_g) ,}{ h(g|X,Z) = h0(t - t0) * Z * exp(beta.s * xs + beta.g * xg) ,}}
where \ifelse{html}{\out{h<sub>0</sub>(t)}}{\eqn{h_0(t)}{h0(t)}} is the baseline hazard function, \ifelse{html}{\out{t<sub>0</sub>}}{\eqn{t_0}{t0}} is a minimum age of disease onset, \ifelse{html}{\out{x<sub>x</sub>}}{\eqn{x_s}{xs}} and \ifelse{html}{\out{x<sub>g</sub>}}{\eqn{x_g}{xg}} indicate male (1) or female (0) and carrier (1) or non-carrier (0) of a main gene of interest, respectively.

For example, when using a Weibull distribution for baseline hazard and a gamma distribution for frailty, the penetrance function has the form
\ifelse{html}{\out{1 - (1 + &lambda;<sup>&rho;</sup> * (t - t<sub>0</sub>)<sup>&rho;</sup> * exp(&beta;<sub>s</sub> * x<sub>s</sub> + &beta;<sub>g</sub> * x<sub>g</sub>)/&kappa;)<sup>-&kappa;</sup>.}}{\deqn{1-\left\{1+\frac{\lambda^\rho (t-t_0)^\rho \exp(\beta_s x_s+\beta_g x_g)}{\kappa}\right\}^{-\kappa} .}{1 - (1 + lambda^rho * (t - t0)^rho * exp(beta.s * xs + beta.g * xg)/kappa)^(-kappa).}}

\emph{Two-gene models}

The penetrance curve for the two-gene model is generated by
\ifelse{html}{\out{1 - exp(- H<sup>0</sup>(t - t<sub>0</sub>) * exp(&beta;<sub>s</sub> * x<sub>s</sub> + &beta;<sub>1</sub> * x<sub>1</sub> + &beta;<sub>2</sub> * x<sub>2</sub>)), }}{\deqn{1-\exp \left\{-H_0(t-t_0)\exp(\beta_s x_s+\beta_{g1} x_{g1} + \beta_{g2} x_{g2}) \right\} , }{ 1 - exp(-H0(t - t0) * exp(beta.s * xs + beta.g1 * g1 + beta.g2 * g2)) , }}
where \ifelse{html}{\out{H<sub>0</sub>(t)}}{\eqn{H_0(t)}{H0(t)}} is the cumulative baseline hazard function,
\ifelse{html}{\out{x<sub>1</sub>}}{\eqn{x_{g1}}{g1}} indicates carrior (1) or non-carrior (0) of a major gene and \ifelse{html}{\out{x<sub>2</sub>}}{\eqn{x_{g2}}{g2}} indicates carrior (1) or non-carrior (0) of a second gene.
When plotting with the two-gene model, the plot will generate separate curves for mutation carriers and noncarriers, and separate curves for the second gene carriers and noncarriers.

}
\value{
Displays plots of the penetrance functions and returns the following values:

\item{pen70}{Penetrance estimates by age 70 specific to gender and mutation-status subgroups. }
\item{x.age}{Vector of ages of onset ranging from \code{agemin} to \code{agemax} years}
\item{pen}{Penetrance estimates computed at each age of \code{x.age}; if \code{variation = "none"} or \code{"frailty"}, it includes subgroups specific to gender and mutation status for major gene. If \code{variation = "secondgene"}, it includes subgroups specific to gender and both mutation statuses for major gene and second gene.}
}

\author{
Yun-Hee Choi
}
\seealso{
\code{\link{simfam}, \link{plot.penmodel}}
}
\examples{
# Penetrance function curves based on Weibull baseline hazard function

penplot(base.parms = c(0.01,3), vbeta = c(0.5, 2), variation = "none", base.dist = "Weibull", 
		agemin = 20, ylim = c(0,1))
}
\keyword{Penetrance plot}
