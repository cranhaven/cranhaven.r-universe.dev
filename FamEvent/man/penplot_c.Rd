\name{penplot_c}
\alias{penplot_c}
\title{
Plot penetrance functions from competing risk models
}
\description{
Plots the penetrance functions from competing risk models given the values of baseline parameters and regression coefficients and choices of baseline and frailty distributions.
}
\usage{
penplot_c(event, base.parms, vbeta, cuts = NULL, variation = "none", 
          base.dist = "Weibull", frailty.dist = NULL, depend = c(1, 1), 
          agemin = 20, agemax = 80, print = TRUE, 
          col = c("blue","red","blue","red"),  lty = c(1, 1, 2, 2), 
          add.legend = TRUE, add.title = TRUE, x = "topleft", 
          y = NULL, xlab = "Age at onset", ylab = "Penetrance", 
          ylim = NULL, main = NULL, ...)
}
\arguments{
  \item{event}{Event of interest for penetrance function: either 1 or 2. Default is \code{1}.}
  \item{base.parms}{
List of vectors of parameter values for the specified baseline hazard functions for both events. For example, \code{base.parms = list(c(lambda1, rho1), c(lambda2, rho2))} should be specified when \code{base.dist = c("Weibull", "Weibull")}.
}
 \item{vbeta}{
List of vectors of regression coefficients for gender and majorgene, \code{vbeta = list(c(beta1.s, beta1.g), c(beta2.s, beta2.g))} for both events. If \code{variation = "secondgene"}, regression coefficients for gender, major gene and second gene, \code{vbeta = list(c(beta1.s, beta1.g1, beta1.g2), c(beta2.s, beta2.g1, beta2.g2))}, should be specified for each event.
}
\item{cuts}{
Vector of cut points defining the intervals when \code{base.dist = "piecewise"} is specified and must be strictly positive and finite and greater than \code{agemin}. Default is \code{NULL}.}
\item{variation}{
Source of residual familial correlation.  Possible choices are: \code{"frailty"} for frailty shared within families, \code{"secondgene"} for second gene variation, or \code{"none"} for no residual familial correlation. Default is \code{"none"}.
}
\item{base.dist}{
Vector of two baseline hazard distributions chosen for competing events. Possible choices are: \code{"Weibull"}, \code{"loglogistic"}, \code{"Gompertz"}, \code{"lognormal"},  \code{"gamma"}, or \code{"piecewise"}.If only one distribution is specified, the same distribution will be assumed for both events. A vector of two distributions should be specified if different baseline distributions are assumed for different events. Default is \code{c("Weibull", "Weibull")}.
}
 \item{frailty.dist}{
Choice of frailty distribution. Possible choices are \code{"gamma"} for independent gamma, \code{"lognormal"} for independent log-normal, \code{"cgamma"} for correlated gamma, \code{"clognormal"} for correlated log-normal distribution when \code{variation = "frailty"} or \code{"none" or NULL} when no frailty distribution is assumed. Default is \code{NULL}.
}
\item{depend}{
Vector of frailty parameter values assumed for specified frailty distribution.  They  corresponds inverse of variance of the frailty distribution. Dependence within families decreases with \code{depend} value.  Default value is \code{c(1,1)}. 
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
\item{xlab}{Title for the x-axis. Default is \code{"Age at Onset"}.}
\item{ylab}{Title for the y-axis. Default is \code{"Penetrance"}.}
\item{ylim}{Limits of the y-axis. Default is \code{NULL}. If \code{NULL}, \code{ylim} will be automatically determined.}
\item{main}{Main title of the plot. Default is \code{NULL}. If \code{NULL}, the title will be automatically created.}
\item{...}{Other parameters to be passed through to plotting functions.}
}
\details{
%At the moment the function will only generate penetrance curves for the gamma frailty model and the second gene model. The penetrance curves for the log-normal frailty will be implemented at a later date.
\emph{Cause-specific proportional hazard models}
The penetrance models for competing events conditional on the covariates \ifelse{html}{ \out{ X = c(x<sub>s</sub>, x<sub>g</sub>)}}{\eqn{X = (x_s, x_g)}{X = c(xs, xg)}} are assumed to have the following hazard functions for event \eqn{j = 1, 2}:
\ifelse{html}{ \out{h<sub>j</sub>(t|X) = h<sub>0j</sub>(t - t<sub>0</sub>) exp(&beta;<sub>js</sub> * x<sub>s</sub> + &beta;<sub>jg</sub> * x<sub>g</sub>),}}{\deqn{ h_j(t|X) = h_{0j}(t - t_0) \exp(\beta_{js} x_{s}+\beta_{jg} x_{g}),}{ hj(t|X) = h0(t - t0) * exp(betaj.s * xs + betaj.g * xg),}} 
where \ifelse{html}{\out{h<sub>0j</sub>(t)}}{\eqn{h_{0j}(t)}{h0j(t)}} is the baseline hazard function for event \eqn{j}, \eqn{j=1,2}, \ifelse{html}{\out{t<sub>0j</sub>}}{\eqn{t_0}{t0}} is a minimum age of disease onset, \ifelse{html}{\out{x<sub>s</sub>}}{\eqn{x_s}{xs}} and \ifelse{html}{\out{x<sub>g</sub>}}{\eqn{x_g}{xg}} indicate male (1) or female (0) and carrier (1) or non-carrier (0) of a major gene of interest, respectively.

The penetrance function for the penetrance model has the form, \ifelse{html}{\out{1 - exp(- H<sup>0</sup>(t - t<sub>0</sub>) * exp(&beta;<sub>s</sub> * x<sub>s</sub> + &beta;<sub>g</sub> * x<sub>g</sub> )), }}{\deqn{1-\exp\left\{-H_0(t-t_0) \exp (\beta_s x_s+\beta_g x_g) \right\}, }{ 1 - exp(-H0(t - t0) * exp(beta.s * xs + beta.g * xg)), }}
where \ifelse{html}{\out{H<sub>0</sub>(t)}}{\eqn{H_0(t)}{H0(t)}} is the cumulative baseline hazard function.

\emph{Shared frailty models}

The penetrance model conditional on the frailty \eqn{Z} and covariates 
\ifelse{html}{ \out{ X = c(x<sub>s</sub>, x<sub>g</sub>)}}{\eqn{X=(x_s, x_g)}{X = c(xs, xg)}} is assumed to have the following hazard function:
\ifelse{html}{ \out{h(t|X,Z) = h<sub>0</sub>(t - t<sub>0</sub>) Z exp(&beta;<sub>s</sub> * x<sub>s</sub> + &beta;<sub>g</sub> * x<sub>g</sub>),}}{	
		\deqn{ h(t|X,Z) = h_0(t - t_0) Z \exp(\beta_s x_s+\beta_g x_g) ,}{ h(g|X,Z) = h0(t - t0) * Z * exp(beta.s * xs + beta.g * xg) ,}}
where \ifelse{html}{\out{h<sub>0</sub>(t)}}{\eqn{h_0(t)}{h0(t)}} is the baseline hazard function, \ifelse{html}{\out{t<sub>0</sub>}}{\eqn{t_0}{t0}} is a minimum age of disease onset, \ifelse{html}{\out{x<sub>x</sub>}}{\eqn{x_s}{xs}} and \ifelse{html}{\out{x<sub>g</sub>}}{\eqn{x_g}{xg}} indicate male (1) or female (0) and carrier (1) or non-carrier (0) of a main gene of interest, respectively.

For example, when using a Weibull distribution for baseline hazard and a gamma distribution for frailty, the penetrance function has the form
\ifelse{html}{\out{ 1 - (1 + &lambda;<sup>&rho;</sup> * (t - t<sub>0</sub>)<sup>&rho;</sup> * exp(&beta;<sub>s</sub> * x<sub>s</sub> + &beta;<sub>g</sub> * x<sub>g</sub>)/&kappa;)<sup>-&kappa;</sup>.}}{\deqn{1-\left\{1+\frac{\lambda^\rho (t-t_0)^\rho \exp(\beta_s x_s+\beta_g x_g)}{\kappa}\right\}^{-\kappa} .}{1 - (1 + lambda^rho * (t - t0)^rho * exp(beta.s * xs + beta.g * xg)/kappa)^(-kappa).}}

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
\code{\link{simfam_c}, \link{plot.penmodel_c}}
}
\examples{
# Penetrance function curves for event 1 
# based on Weibull baselines (no frailty)
penplot_c(event=1, base.parms = list(c(0.01,3), c(0.01, 3)), 
        vbeta = list(c(-1, 2), c(-1, 1)), variation = "none", 
        base.dist = "Weibull", agemin = 20, ylim = c(0,1))

# Penetrance function curves for event 1 
# based on gamma frailty and Weibull baselines
penplot_c(event=1, base.parms = list(c(0.01,3), c(0.01, 3)), 
        vbeta = list(c(-1, 2), c(-1, 1)), depend=c(2, 2), 
        variation = "frailty", frailty.dist="gamma", base.dist = "Weibull", 
        agemin = 20, ylim = c(0,1))

# Penetrance function curves for event 1 
# based on correlated gamma frailty and Weibull baselines
penplot_c(event=1, base.parms = list(c(0.01,3), c(0.01, 3)), 
        vbeta = list(c(-1, 2), c(-1, 1)), depend=c(2, 2, 0.2), 
        variation = "frailty", frailty.dist="cgamma", 
        base.dist = "Weibull", agemin = 20, ylim = c(0,1))

# Penetrance function curves for event 1 
# based on correlated lognormal frailty and Weibull baselines
penplot_c(event=1, base.parms = list(c(0.01,3), c(0.01, 3)), 
        vbeta = list(c(-1, 2), c(-1, 1)), depend=c(2, 2, 0.2), 
        variation = "frailty", frailty.dist="clognormal", 
        base.dist = "Weibull", agemin = 20, ylim = c(0,1))
}
\keyword{Penetrance plot}
